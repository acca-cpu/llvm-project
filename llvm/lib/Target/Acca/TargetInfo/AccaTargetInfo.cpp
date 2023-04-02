//===-- AccaTargetInfo.cpp - Acca Target Implementation -----------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "TargetInfo/AccaTargetInfo.h"
#include "llvm/MC/TargetRegistry.h"
using namespace llvm;

Target &llvm::getTheAccaTarget() {
  static Target TheAccaTarget;
  return TheAccaTarget;
}

extern "C" LLVM_EXTERNAL_VISIBILITY void LLVMInitializeAccaTargetInfo() {
  RegisterTarget<Triple::acca, /*HasJIT=*/false> X(getTheAccaTarget(), "acca", "Acca", "Acca");
}
