//===-- AccaFrameLowering.cpp - Acca Frame Information ------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file contains the Acca implementation of TargetFrameLowering class.
//
//===----------------------------------------------------------------------===//

#include "AccaFrameLowering.h"
#include "AccaInstrInfo.h"
#include "AccaMachineFunctionInfo.h"
#include "AccaSubtarget.h"
#include "llvm/CodeGen/MachineFrameInfo.h"
#include "llvm/CodeGen/MachineFunction.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/CodeGen/MachineModuleInfo.h"
#include "llvm/CodeGen/MachineRegisterInfo.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/IR/Function.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Target/TargetOptions.h"

using namespace llvm;

AccaFrameLowering::AccaFrameLowering()
  : TargetFrameLowering(StackDirection::StackGrowsDown, Align(16), 0,
                        Align(16), true) {};

void AccaFrameLowering::
emitPrologue(MachineFunction &MF, MachineBasicBlock &MBB) const {
  MachineBasicBlock::iterator MBBI = MBB.begin();
  const AccaSubtarget &Subtarget = MF.getSubtarget<AccaSubtarget>();
  const TargetInstrInfo *TII = Subtarget.getInstrInfo();

  DebugLoc DL;

#if 0
  // pushp rfp, rlr
  BuildMI(MBB, MBBI, DL, TII->get(Acca::PUSHP_word_nonnull))
    .addReg(Acca::R14)
    .addReg(Acca::R15)
    .addReg(Acca::R13, RegState::ImplicitDefine);

  // copy rfp, rsp
  BuildMI(MBB, MBBI, DL, TII->get(Acca::COPY_word), Acca::R14)
    .addReg(Acca::R13);
#endif
};

void AccaFrameLowering::
emitEpilogue(MachineFunction &MF, MachineBasicBlock &MBB) const {
  MachineBasicBlock::iterator MBBI = MBB.getLastNonDebugInstr();
  const AccaSubtarget &Subtarget = MF.getSubtarget<AccaSubtarget>();
  const TargetInstrInfo *TII = Subtarget.getInstrInfo();

  DebugLoc DL;

  if (MBB.end() != MBBI) {
    DL = MBBI->getDebugLoc();
  }

#if 0
  // copy rsp, rfp
  BuildMI(MBB, MBBI, DL, TII->get(Acca::COPY_word), Acca::R13)
    .addReg(Acca::R14);

  // popp rfp, rlr
  BuildMI(MBB, MBBI, DL, TII->get(Acca::POPP_word_nonnull))
    .addReg(Acca::R14)
    .addReg(Acca::R15)
    .addReg(Acca::R13, RegState::ImplicitDefine);
#endif
};

MachineBasicBlock::iterator AccaFrameLowering::
eliminateCallFramePseudoInstr(MachineFunction &MF,
                              MachineBasicBlock &MBB,
                              MachineBasicBlock::iterator I) const {
  llvm_unreachable("unimplemented");
};

bool AccaFrameLowering::hasReservedCallFrame(const MachineFunction &MF) const {
  // Reserve call frame if there are no variable sized objects on the stack.
  return !MF.getFrameInfo().hasVarSizedObjects();
};

bool AccaFrameLowering::hasFP(const MachineFunction &MF) const {
  const TargetRegisterInfo *RegInfo = MF.getSubtarget().getRegisterInfo();

  const MachineFrameInfo &MFI = MF.getFrameInfo();
  return MF.getTarget().Options.DisableFramePointerElim(MF) ||
         RegInfo->hasStackRealignment(MF) || MFI.hasVarSizedObjects() ||
         MFI.isFrameAddressTaken();
};

void AccaFrameLowering::
determineCalleeSaves(MachineFunction &MF, BitVector &SavedRegs,
                     RegScavenger *RS) const {
  TargetFrameLowering::determineCalleeSaves(MF, SavedRegs, RS);
};

StackOffset AccaFrameLowering::
getFrameIndexReference(const MachineFunction &MF, int FI,
                       Register &FrameReg) const {
  const AccaSubtarget &Subtarget = MF.getSubtarget<AccaSubtarget>();
  const MachineFrameInfo &MFI = MF.getFrameInfo();
  const AccaRegisterInfo *RegInfo = Subtarget.getRegisterInfo();

  int64_t FrameOffset = MFI.getObjectOffset(FI);

  if (hasFP(MF)) {
    FrameReg = RegInfo->getFrameRegister(MF);
    return StackOffset::getFixed(FrameOffset);
  } else {
    FrameReg = Acca::R13; // a.k.a. RSP
    return StackOffset::getFixed(FrameOffset + MFI.getStackSize());
  }
};
