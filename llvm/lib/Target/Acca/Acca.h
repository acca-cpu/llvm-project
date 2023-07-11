//===-- Acca.h - Top-level interface for Acca representation --*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file contains the entry points for global functions defined in the LLVM
// Acca back-end.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_ACCA_ACCA_H
#define LLVM_LIB_TARGET_ACCA_ACCA_H

#include "MCTargetDesc/AccaMCTargetDesc.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Target/TargetMachine.h"

namespace llvm {
class AsmPrinter;
class FunctionPass;
class MCInst;
class MachineInstr;
class PassRegistry;
class AccaTargetMachine;

FunctionPass *createAccaISelDag(AccaTargetMachine &TM);
FunctionPass *createAccaPreRAExpandPseudoPass();

void initializeAccaDAGToDAGISelPass(PassRegistry &);
void initializeAccaPreRAExpandPseudoPass(PassRegistry &);
} // namespace llvm

#endif
