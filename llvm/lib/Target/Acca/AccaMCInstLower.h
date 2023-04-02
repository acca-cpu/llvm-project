//===-- AccaMCInstLower.h - Lower MachineInstr to MCInst ---------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_ACCA_ACCAMCINSTLOWER_H
#define LLVM_LIB_TARGET_ACCA_ACCAMCINSTLOWER_H

#include "llvm/ADT/Triple.h"
#include "llvm/Support/Compiler.h"

namespace llvm {
class AsmPrinter;
class MCContext;
class MCInst;
class MCOperand;
class MCSymbol;
class MachineInstr;
class MachineOperand;

/// AccaMCInstLower - This class is used to lower an MachineInstr
/// into an MCInst.
class LLVM_LIBRARY_VISIBILITY AccaMCInstLower {
  MCContext &Ctx;
  AsmPrinter &Printer;
  Triple TargetTriple;

public:
  AccaMCInstLower(MCContext &ctx, AsmPrinter &printer);

  bool lowerOperand(const MachineOperand &MO, MCOperand &MCOp) const;
  void Lower(const MachineInstr *MI, MCInst &OutMI) const;

  MCOperand lowerSymbolOperandELF(const MachineOperand &MO,
                                  MCSymbol *Sym) const;
  MCOperand LowerSymbolOperand(const MachineOperand &MO, MCSymbol *Sym) const;

  MCSymbol *GetGlobalAddressSymbol(const MachineOperand &MO) const;
  MCSymbol *GetExternalSymbolSymbol(const MachineOperand &MO) const;
};
}

#endif
