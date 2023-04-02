//===-- AccaSubtarget.cpp - Acca Subtarget Information ------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file implements the Acca specific subclass of TargetSubtargetInfo.
//
//===----------------------------------------------------------------------===//

#include "AccaSubtarget.h"

using namespace llvm;

#define DEBUG_TYPE "acca-subtarget"

#define GET_SUBTARGETINFO_TARGET_DESC
#define GET_SUBTARGETINFO_CTOR
#include "AccaGenSubtargetInfo.inc"

AccaSubtarget::AccaSubtarget(const Triple &TT, StringRef CPU, StringRef FS,
                             const TargetMachine &TM):
  AccaGenSubtargetInfo(TT, CPU, CPU, FS), TargetTriple(TT),
  InstrInfo(), TLInfo(TM, *this),
  FrameLowering() {}
