//===-- AccaInstPrinter.cpp - Convert Acca MCInst to assembly syntax -----==//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This class prints an Acca MCInst to a .s file.
//
//===----------------------------------------------------------------------===//

#include "AccaInstPrinter.h"
#include "Acca.h"
#include "llvm/MC/MCExpr.h"
#include "llvm/MC/MCInst.h"
#include "llvm/MC/MCRegisterInfo.h"
#include "llvm/MC/MCSubtargetInfo.h"
#include "llvm/MC/MCSymbol.h"
#include "llvm/Support/raw_ostream.h"
using namespace llvm;

#define DEBUG_TYPE "asm-printer"

#define GET_INSTRUCTION_NAME
#define PRINT_ALIAS_INSTR
#include "AccaGenAsmWriter.inc"

void AccaInstPrinter::printRegName(raw_ostream &OS, MCRegister Reg) const {
  OS << StringRef(getRegisterName(Reg)).lower();
}

void AccaInstPrinter::printInst(const MCInst *MI, uint64_t Address,
                                 StringRef Annot, const MCSubtargetInfo &STI,
                                 raw_ostream &O) {
  if (!printAliasInstr(MI, Address, O))
    printInstruction(MI, Address, O);
  printAnnotation(O, Annot);
}

void AccaInstPrinter::
printOperand(const MCInst *MI, int opNum, raw_ostream &O) {
  const MCOperand &MO = MI->getOperand(opNum);

  if (MO.isReg()) {
    printRegName(O, MO.getReg());
    return ;
  }

  if (MO.isImm()) {
    switch (MI->getOpcode()) {
      default:
        O << (int)MO.getImm();
        return;
    }
  }

  assert(MO.isExpr() && "Unknown operand kind in printOperand");
  MO.getExpr()->print(O, &MAI);
};

void AccaInstPrinter::
printCondCode(const MCInst *MI, int opNum, raw_ostream &OS) {
  int64_t CC = MI->getOperand(opNum).getImm();

  switch (CC) {
    case 0: OS << "c" ; break;
    case 1: OS << "nc"; break;
    case 2: OS << "z" ; break;
    case 3: OS << "nz"; break;
    case 4: OS << "o" ; break;
    case 5: OS << "no"; break;
    case 6: OS << "s" ; break;
    case 7: OS << "ns"; break;
    case 8: OS << "l" ; break;
    case 9: OS << "nl"; break;
    default: OS << "<unknown>"; break;
  }
};
