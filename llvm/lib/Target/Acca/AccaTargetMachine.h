//===-- AccaTargetMachine.h - Define TargetMachine for Acca ---*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file declares the Acca specific subclass of TargetMachine.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_ACCA_ACCATARGETMACHINE_H
#define LLVM_LIB_TARGET_ACCA_ACCATARGETMACHINE_H

#include "AccaInstrInfo.h"
#include "AccaSubtarget.h"
#include "llvm/Target/TargetMachine.h"

namespace llvm {

class AccaTargetMachine : public LLVMTargetMachine {
  std::unique_ptr<TargetLoweringObjectFile> TLOF;
  AccaSubtarget Subtarget;
  mutable StringMap<std::unique_ptr<AccaSubtarget>> SubtargetMap;

public:
  AccaTargetMachine(const Target &T, const Triple &TT, StringRef CPU,
                    StringRef FS, const TargetOptions &Options,
                    std::optional<Reloc::Model> RM,
                    std::optional<CodeModel::Model> CM,
                    CodeGenOpt::Level OL, bool JIT);
  ~AccaTargetMachine() override;

  const AccaSubtarget *getSubtargetImpl() const { return &Subtarget; }
  const AccaSubtarget *getSubtargetImpl(const Function &) const override;

  // Pass Pipeline Configuration
  TargetPassConfig *createPassConfig(PassManagerBase &PM) override;
  TargetLoweringObjectFile *getObjFileLowering() const override {
    return TLOF.get();
  }

  MachineFunctionInfo *
  createMachineFunctionInfo(BumpPtrAllocator &Allocator, const Function &F,
                            const TargetSubtargetInfo *STI) const override;
};

} // end namespace llvm

#endif
