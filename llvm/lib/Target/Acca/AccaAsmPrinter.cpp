//===-- AccaAsmPrinter.cpp - Acca LLVM assembly writer ------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file contains a printer that converts from our internal representation
// of machine-dependent LLVM code to Acca assembly language.
//
//===----------------------------------------------------------------------===//


#include "Acca.h"
#include "AccaMCInstLower.h"
#include "AccaMachineFunctionInfo.h"
#include "AccaRegisterInfo.h"
#include "AccaSubtarget.h"
#include "AccaTargetObjectFile.h"
//#include "MCTargetDesc/AccaAddressingModes.h"
#include "MCTargetDesc/AccaInstPrinter.h"
#include "MCTargetDesc/AccaMCExpr.h"
#include "MCTargetDesc/AccaMCTargetDesc.h"
#include "MCTargetDesc/AccaTargetStreamer.h"
#include "TargetInfo/AccaTargetInfo.h"
//#include "Utils/AArch64BaseInfo.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/ADT/Triple.h"
#include "llvm/ADT/Twine.h"
#include "llvm/BinaryFormat/COFF.h"
#include "llvm/BinaryFormat/ELF.h"
#include "llvm/CodeGen/AsmPrinter.h"
#include "llvm/CodeGen/FaultMaps.h"
#include "llvm/CodeGen/MachineBasicBlock.h"
#include "llvm/CodeGen/MachineFunction.h"
#include "llvm/CodeGen/MachineInstr.h"
#include "llvm/CodeGen/MachineJumpTableInfo.h"
#include "llvm/CodeGen/MachineModuleInfoImpls.h"
#include "llvm/CodeGen/MachineOperand.h"
#include "llvm/CodeGen/StackMaps.h"
#include "llvm/CodeGen/TargetRegisterInfo.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/IR/DebugInfoMetadata.h"
#include "llvm/MC/MCAsmInfo.h"
#include "llvm/MC/MCContext.h"
#include "llvm/MC/MCInst.h"
#include "llvm/MC/MCInstBuilder.h"
#include "llvm/MC/MCSectionELF.h"
#include "llvm/MC/MCStreamer.h"
#include "llvm/MC/MCSymbol.h"
#include "llvm/MC/TargetRegistry.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Transforms/Instrumentation/HWAddressSanitizer.h"
#include <algorithm>
#include <cassert>
#include <cstdint>
#include <map>
#include <memory>

using namespace llvm;

#define DEBUG_TYPE "asm-printer"

namespace {

class AccaAsmPrinter : public AsmPrinter {
  AccaMCInstLower MCInstLowering;
  FaultMaps FM;
  const AccaSubtarget *STI;
  bool ShouldEmitWeakSwiftAsyncExtendedFramePointerFlags = false;

public:
  AccaAsmPrinter(TargetMachine &TM, std::unique_ptr<MCStreamer> Streamer)
      : AsmPrinter(TM, std::move(Streamer)), MCInstLowering(OutContext, *this),
        FM(*this) {}

  StringRef getPassName() const override { return "Acca Assembly Printer"; }

  /// Wrapper for MCInstLowering.lowerOperand() for the
  /// tblgen'erated pseudo lowering.
  bool lowerOperand(const MachineOperand &MO, MCOperand &MCOp) const {
    return MCInstLowering.lowerOperand(MO, MCOp);
  }

  /// tblgen'erated driver function for lowering simple MI->MC
  /// pseudo instructions.
  bool emitPseudoExpansionLowering(MCStreamer &OutStreamer,
                                   const MachineInstr *MI);

  void emitInstruction(const MachineInstr *MI) override;

  void getAnalysisUsage(AnalysisUsage &AU) const override {
    AsmPrinter::getAnalysisUsage(AU);
    AU.setPreservesAll();
  }

  bool runOnMachineFunction(MachineFunction &MF) override {
    STI = &MF.getSubtarget<AccaSubtarget>();

    SetupMachineFunction(MF);

    // Emit the rest of the function body.
    emitFunctionBody();

    // Emit the XRay table for this function.
    emitXRayTable();

    // We didn't modify anything.
    return false;
  }
};

} // end anonymous namespace

// Simple pseudo-instructions have their lowering (with expansion to real
// instructions) auto-generated.
#include "AccaGenMCPseudoLowering.inc"

void AccaAsmPrinter::emitInstruction(const MachineInstr *MI) {
  Acca_MC::verifyInstructionPredicates(MI->getOpcode(), STI->getFeatureBits());

  // Do any auto-generated pseudo lowerings.
  if (emitPseudoExpansionLowering(*OutStreamer, MI))
    return;

  // Finally, do the automated lowerings for everything else.
  MCInst TmpInst;
  MCInstLowering.Lower(MI, TmpInst);
  EmitToStreamer(*OutStreamer, TmpInst);
}

// Force static initialization.
extern "C" LLVM_EXTERNAL_VISIBILITY void LLVMInitializeAccaAsmPrinter() {
  RegisterAsmPrinter<AccaAsmPrinter> X(getTheAccaTarget());
}
