//===-- AccaSubtarget.h - Define Subtarget for Acca -------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file declares the Acca specific subclass of TargetSubtargetInfo.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_ACCA_ACCASUBTARGET_H
#define LLVM_LIB_TARGET_ACCA_ACCASUBTARGET_H

#include "AccaFrameLowering.h"
#include "AccaISelLowering.h"
#include "AccaInstrInfo.h"
#include "llvm/ADT/Triple.h"
#include "llvm/CodeGen/SelectionDAGTargetInfo.h"
#include "llvm/CodeGen/TargetSubtargetInfo.h"
#include "llvm/IR/DataLayout.h"
#include <string>

#define GET_SUBTARGETINFO_HEADER
#include "AccaGenSubtargetInfo.inc"

namespace llvm {

class AccaSubtarget : public AccaGenSubtargetInfo {
  Triple TargetTriple;

  AccaInstrInfo InstrInfo;
  AccaTargetLowering TLInfo;
  SelectionDAGTargetInfo TSInfo;
  AccaFrameLowering FrameLowering;
public:
  AccaSubtarget(const Triple &TT, StringRef CPU, StringRef FS, const TargetMachine &TM);

  const AccaInstrInfo *getInstrInfo() const override { return &InstrInfo; }
  const TargetFrameLowering *getFrameLowering() const override {
    return &FrameLowering;
  }
  const AccaRegisterInfo *getRegisterInfo() const override {
    return &InstrInfo.getRegisterInfo();
  }
  const AccaTargetLowering *getTargetLowering() const override {
    return &TLInfo;
  }
  const SelectionDAGTargetInfo *getSelectionDAGInfo() const override {
    return &TSInfo;
  }

  void ParseSubtargetFeatures(StringRef CPU, StringRef TuneCPU, StringRef FS);
};

} // end namespace llvm

#endif
