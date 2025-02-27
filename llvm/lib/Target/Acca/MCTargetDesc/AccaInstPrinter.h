//===-- AccaInstPrinter.h - Convert Acca MCInst to assembly syntax ------===//
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

#ifndef LLVM_LIB_TARGET_ACCA_MCTARGETDESC_ACCAINSTPRINTER_H
#define LLVM_LIB_TARGET_ACCA_MCTARGETDESC_ACCAINSTPRINTER_H

#include "llvm/MC/MCInstPrinter.h"

namespace llvm {

class AccaInstPrinter : public MCInstPrinter {
public:
  AccaInstPrinter(const MCAsmInfo &MAI, const MCInstrInfo &MII,
                   const MCRegisterInfo &MRI)
      : MCInstPrinter(MAI, MII, MRI) {}

  void printRegName(raw_ostream &OS, MCRegister Reg) const override;
  void printInst(const MCInst *MI, uint64_t Address, StringRef Annot,
                 const MCSubtargetInfo &STI, raw_ostream &O) override;

  // Autogenerated by tblgen.
  std::pair<const char *, uint64_t> getMnemonic(const MCInst *MI) override;
  void printInstruction(const MCInst *MI, uint64_t Address, raw_ostream &O);
  bool printAliasInstr(const MCInst *MI, uint64_t Address, raw_ostream &O);
  static const char *getRegisterName(MCRegister Reg);

  void printOperand(const MCInst *MI, int opNum, raw_ostream &OS);
  void printCondCode(const MCInst *MI, int opNum, raw_ostream &OS);
};
} // end namespace llvm

#endif
