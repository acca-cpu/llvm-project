//===- AccaMachineFunctionInfo.h - Acca Machine Function Info -*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file declares Acca specific per-machine-function information.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_ACCA_ACCAMACHINEFUNCTIONINFO_H
#define LLVM_LIB_TARGET_ACCA_ACCAMACHINEFUNCTIONINFO_H

#include "llvm/CodeGen/MachineFunction.h"

namespace llvm {
  class AccaMachineFunctionInfo : public MachineFunctionInfo {
    virtual void anchor();
  public:
    AccaMachineFunctionInfo() {}
    AccaMachineFunctionInfo(const Function &F, const TargetSubtargetInfo *STI)
      {}

    MachineFunctionInfo *
    clone(BumpPtrAllocator &Allocator, MachineFunction &DestMF,
          const DenseMap<MachineBasicBlock *, MachineBasicBlock *> &Src2DstMBB)
        const override;
  };
}

#endif
