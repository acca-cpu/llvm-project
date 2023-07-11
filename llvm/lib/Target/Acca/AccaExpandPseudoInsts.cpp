//===----- AccaExpandPseudoInsts.cpp - Expand pseudo instructions ---------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file contains a pass that expands pseudo instructions into target
// instructions.
//
//===----------------------------------------------------------------------===//

#include "Acca.h"
#include "AccaInstrInfo.h"
#include "AccaRegisterInfo.h"
#include "llvm/CodeGen/MachineFunctionPass.h"
#include "AccaBaseInfo.h"
#include "MCTargetDesc/AccaMCTargetDesc.h"

using namespace llvm;

#define ACCA_PRERA_EXPAND_PSEUDO_NAME                                     \
  "Acca Pre-RA pseudo instruction expansion pass"

namespace {

class AccaPreRAExpandPseudo : public MachineFunctionPass {
public:
  const AccaInstrInfo *TII;
  static char ID;

  AccaPreRAExpandPseudo() : MachineFunctionPass(ID) {
    initializeAccaPreRAExpandPseudoPass(*PassRegistry::getPassRegistry());
  }

  bool runOnMachineFunction(MachineFunction &MF) override;

  void getAnalysisUsage(AnalysisUsage &AU) const override {
    AU.setPreservesCFG();
    MachineFunctionPass::getAnalysisUsage(AU);
  }
  StringRef getPassName() const override {
    return ACCA_PRERA_EXPAND_PSEUDO_NAME;
  }

private:
  bool expandMBB(MachineBasicBlock &MBB);
  bool expandMI(MachineBasicBlock &MBB, MachineBasicBlock::iterator MBBI,
                MachineBasicBlock::iterator &NextMBBI);
  bool expandFunctionCALL(MachineBasicBlock &MBB,
                          MachineBasicBlock::iterator MBBI,
                          MachineBasicBlock::iterator &NextMBBI,
                          bool IsTailCall);
};

char AccaPreRAExpandPseudo::ID = 0;

bool AccaPreRAExpandPseudo::runOnMachineFunction(MachineFunction &MF) {
  TII =
      static_cast<const AccaInstrInfo *>(MF.getSubtarget().getInstrInfo());
  bool Modified = false;
  for (auto &MBB : MF)
    Modified |= expandMBB(MBB);
  return Modified;
}

bool AccaPreRAExpandPseudo::expandMBB(MachineBasicBlock &MBB) {
  bool Modified = false;

  MachineBasicBlock::iterator MBBI = MBB.begin(), E = MBB.end();
  while (MBBI != E) {
    MachineBasicBlock::iterator NMBBI = std::next(MBBI);
    Modified |= expandMI(MBB, MBBI, NMBBI);
    MBBI = NMBBI;
  }

  return Modified;
}

bool AccaPreRAExpandPseudo::expandMI(
    MachineBasicBlock &MBB, MachineBasicBlock::iterator MBBI,
    MachineBasicBlock::iterator &NextMBBI) {
  switch (MBBI->getOpcode()) {
  case Acca::PseudoCALL:
    return expandFunctionCALL(MBB, MBBI, NextMBBI, /*IsTailCall=*/false);
  case Acca::PseudoTAIL:
    return expandFunctionCALL(MBB, MBBI, NextMBBI, /*IsTailCall=*/true);
  }
  return false;
}

bool AccaPreRAExpandPseudo::expandFunctionCALL(
    MachineBasicBlock &MBB, MachineBasicBlock::iterator MBBI,
    MachineBasicBlock::iterator &NextMBBI, bool IsTailCall) {
  MachineFunction *MF = MBB.getParent();
  MachineInstr &MI = *MBBI;
  DebugLoc DL = MI.getDebugLoc();
  const MachineOperand &Func = MI.getOperand(0);
  MachineInstrBuilder CALL;
  unsigned Opcode;

  switch (MF->getTarget().getCodeModel()) {
  default:
    report_fatal_error("Unsupported code model");
    break;
  case CodeModel::Small: {
    // CALL:
    // callr func
    // TAIL:
    // jmpr func
    Opcode = IsTailCall ? Acca::PseudoJMP_TAIL : Acca::CALL_immrel_nocond;
    CALL = BuildMI(MBB, MBBI, DL, TII->get(Opcode)).add(Func);
    break;
  }
  case CodeModel::Large: {
    // CALL:
    // ldi  $scratch, %rel64_d0(func),  0, 3
    // ldi  $scratch, %rel64_d1(func), 16, 0
    // ldi  $scratch, %rel64_d2(func), 32, 0
    // ldi  $scratch, %rel64_d3(func), 48, 0
    // callr      $scratch
    // TAIL:
    // ldi  $scratch, %rel64_d0(func),  0, 3
    // ldi  $scratch, %rel64_d1(func), 16, 0
    // ldi  $scratch, %rel64_d2(func), 32, 0
    // ldi  $scratch, %rel64_d3(func), 48, 0
    // jmpr $scratch
    Opcode =
        IsTailCall ? Acca::PseudoJMPREGREL_TAIL : Acca::PseudoCALLREGREL;
    Register ScratchReg = MF->getRegInfo()
      .createVirtualRegister(&Acca::I64RegsRegClass);
    MachineInstrBuilder DB0 = BuildMI(MBB, MBBI, DL,
                                      TII->get(Acca::LDI_noshift_clearall),
                                      ScratchReg);
    MachineInstrBuilder DB1 = BuildMI(MBB, MBBI, DL,
                                      TII->get(Acca::LDI), ScratchReg);
    MachineInstrBuilder DB2 = BuildMI(MBB, MBBI, DL,
                                      TII->get(Acca::LDI), ScratchReg);
    MachineInstrBuilder DB3 = BuildMI(MBB, MBBI, DL,
                                      TII->get(Acca::LDI), ScratchReg);
    CALL = BuildMI(MBB, MBBI, DL, TII->get(Opcode)).addReg(ScratchReg);
    if (Func.isSymbol()) {
      const char *FnName = Func.getSymbolName();
      DB0.addExternalSymbol(FnName, AccaII::MO_REL64_D0);
      DB1.addExternalSymbol(FnName, AccaII::MO_REL64_D1);
      DB2.addExternalSymbol(FnName, AccaII::MO_REL64_D2);
      DB3.addExternalSymbol(FnName, AccaII::MO_REL64_D3);
    } else {
      assert(Func.isGlobal() && "Expected a GlobalValue at this time");
      const GlobalValue *GV = Func.getGlobal();
      DB0.addGlobalAddress(GV, 0, AccaII::MO_REL64_D0);
      DB1.addGlobalAddress(GV, 0, AccaII::MO_REL64_D1);
      DB2.addGlobalAddress(GV, 0, AccaII::MO_REL64_D2);
      DB3.addGlobalAddress(GV, 0, AccaII::MO_REL64_D3);
    }
    DB1.addImm(16);
    DB1.addImm(0);
    DB2.addImm(32);
    DB2.addImm(0);
    DB3.addImm(48);
    DB3.addImm(0);
    break;
  }
  }

  // Transfer implicit operands.
  CALL.copyImplicitOps(MI);

  // Transfer MI flags.
  CALL.setMIFlags(MI.getFlags());

  MI.eraseFromParent();
  return true;
}

} // end namespace

INITIALIZE_PASS(AccaPreRAExpandPseudo, "acca-prera-expand-pseudo",
                ACCA_PRERA_EXPAND_PSEUDO_NAME, false, false)

namespace llvm {

FunctionPass *createAccaPreRAExpandPseudoPass() {
  return new AccaPreRAExpandPseudo();
}

} // end namespace llvm
