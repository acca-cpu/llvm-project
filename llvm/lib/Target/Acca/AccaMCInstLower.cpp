//==-- AccaMCInstLower.cpp - Convert Acca MachineInstr to an MCInst --==//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file contains code to lower Acca MachineInstrs to their corresponding
// MCInst records.
//
//===----------------------------------------------------------------------===//

#include "AccaMCInstLower.h"
#include "MCTargetDesc/AccaMCExpr.h"
#include "llvm/CodeGen/AsmPrinter.h"
#include "llvm/CodeGen/MachineBasicBlock.h"
#include "llvm/CodeGen/MachineInstr.h"
#include "llvm/CodeGen/MachineModuleInfoImpls.h"
#include "llvm/IR/Mangler.h"
#include "llvm/MC/MCContext.h"
#include "llvm/MC/MCExpr.h"
#include "llvm/MC/MCInst.h"
#include "llvm/MC/MCStreamer.h"
#include "llvm/Support/CodeGen.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Target/TargetLoweringObjectFile.h"
#include "llvm/Target/TargetMachine.h"
using namespace llvm;

extern cl::opt<bool> EnableAccaELFLocalDynamicTLSGeneration;

AccaMCInstLower::AccaMCInstLower(MCContext &ctx, AsmPrinter &printer)
    : Ctx(ctx), Printer(printer) {}

MCSymbol *
AccaMCInstLower::GetGlobalAddressSymbol(const MachineOperand &MO) const {
  return Printer.getSymbolPreferLocal(*MO.getGlobal());
}

MCSymbol *
AccaMCInstLower::GetExternalSymbolSymbol(const MachineOperand &MO) const {
  return Printer.GetExternalSymbolSymbol(MO.getSymbolName());
}

MCOperand AccaMCInstLower::lowerSymbolOperandELF(const MachineOperand &MO,
                                                    MCSymbol *Sym) const {
  uint32_t RefFlags = 0;

  const MCExpr *Expr =
      MCSymbolRefExpr::create(Sym, MCSymbolRefExpr::VK_None, Ctx);
  if (!MO.isJTI() && MO.getOffset())
    Expr = MCBinaryExpr::createAdd(
        Expr, MCConstantExpr::create(MO.getOffset(), Ctx), Ctx);

  AccaMCExpr::VariantKind RefKind;
  RefKind = static_cast<AccaMCExpr::VariantKind>(RefFlags);
  Expr = AccaMCExpr::create(Expr, RefKind, Ctx);

  return MCOperand::createExpr(Expr);
}

MCOperand AccaMCInstLower::LowerSymbolOperand(const MachineOperand &MO,
                                                 MCSymbol *Sym) const {
  assert(Printer.TM.getTargetTriple().isOSBinFormatELF() && "Invalid target");
  return lowerSymbolOperandELF(MO, Sym);
}

bool AccaMCInstLower::lowerOperand(const MachineOperand &MO,
                                      MCOperand &MCOp) const {
  switch (MO.getType()) {
  default:
    llvm_unreachable("unknown operand type");
  case MachineOperand::MO_Register:
    // Ignore all implicit register operands.
    if (MO.isImplicit())
      return false;
    MCOp = MCOperand::createReg(MO.getReg());
    break;
  case MachineOperand::MO_RegisterMask:
    // Regmasks are like implicit defs.
    return false;
  case MachineOperand::MO_Immediate:
    MCOp = MCOperand::createImm(MO.getImm());
    break;
  case MachineOperand::MO_MachineBasicBlock:
    MCOp = MCOperand::createExpr(
        MCSymbolRefExpr::create(MO.getMBB()->getSymbol(), Ctx));
    break;
  case MachineOperand::MO_GlobalAddress:
    MCOp = LowerSymbolOperand(MO, GetGlobalAddressSymbol(MO));
    break;
  case MachineOperand::MO_ExternalSymbol:
    MCOp = LowerSymbolOperand(MO, GetExternalSymbolSymbol(MO));
    break;
  case MachineOperand::MO_MCSymbol:
    MCOp = LowerSymbolOperand(MO, MO.getMCSymbol());
    break;
  case MachineOperand::MO_JumpTableIndex:
    MCOp = LowerSymbolOperand(MO, Printer.GetJTISymbol(MO.getIndex()));
    break;
  case MachineOperand::MO_ConstantPoolIndex:
    MCOp = LowerSymbolOperand(MO, Printer.GetCPISymbol(MO.getIndex()));
    break;
  case MachineOperand::MO_BlockAddress:
    MCOp = LowerSymbolOperand(
        MO, Printer.GetBlockAddressSymbol(MO.getBlockAddress()));
    break;
  }
  return true;
}

void AccaMCInstLower::Lower(const MachineInstr *MI, MCInst &OutMI) const {
  OutMI.setOpcode(MI->getOpcode());

  for (const MachineOperand &MO : MI->operands()) {
    MCOperand MCOp;
    if (lowerOperand(MO, MCOp))
      OutMI.addOperand(MCOp);
  }
}
