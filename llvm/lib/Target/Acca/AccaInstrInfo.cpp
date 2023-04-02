//===-- AccaInstrInfo.cpp - Acca Instruction Information ----------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file contains the Acca implementation of the TargetInstrInfo class.
//
//===----------------------------------------------------------------------===//

#include "AccaInstrInfo.h"
#include "Acca.h"
#include "AccaMachineFunctionInfo.h"
#include "AccaSubtarget.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/CodeGen/MachineFrameInfo.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/CodeGen/MachineMemOperand.h"
#include "llvm/CodeGen/MachineRegisterInfo.h"
#include "llvm/MC/TargetRegistry.h"
#include "llvm/Support/ErrorHandling.h"

using namespace llvm;

#define GET_INSTRINFO_CTOR_DTOR
#include "AccaGenInstrInfo.inc"

// Pin the vtable to this file.
void AccaInstrInfo::anchor() {}

AccaInstrInfo::AccaInstrInfo()
    : AccaGenInstrInfo(), RI() {}

bool AccaInstrInfo::isBranchOffsetInRange(unsigned BranchOpc,
                                          int64_t BrOffset) const {
  assert((BrOffset & 3) == 0 && "branch offset must be a multiple of 4");
  BrOffset /= 4;
  switch (BranchOpc) {
    case Acca::CJMP_byte_immrel:
    case Acca::CJMP_doublebyte_immrel:
    case Acca::CJMP_quadbyte_immrel:
    case Acca::CJMP_word_immrel:
      return isIntN(13, BrOffset);
    case Acca::JMP_immrel_cond:
    case Acca::JMP_immrel_nocond:
    case Acca::CALL_immrel_cond:
    case Acca::CALL_immrel_nocond:
      return isIntN(22, BrOffset);
    default:
      return false;
  }
};

MachineBasicBlock *AccaInstrInfo::
getBranchDestBlock(const MachineInstr &MI) const {
  llvm_unreachable("unimplemented");
};

void AccaInstrInfo::insertIndirectBranch(MachineBasicBlock &MBB,
                                         MachineBasicBlock &NewDestBB,
                                         MachineBasicBlock &RestoreBB,
                                         const DebugLoc &DL, int64_t BrOffset,
                                         RegScavenger *RS) const {
  llvm_unreachable("unimplemented");
};

static bool isUncondBranchOpcode(int Opc) {
  switch (Opc) {
    case Acca::JMP_abs_nocond:
    case Acca::JMP_immrel_nocond:
    case Acca::JMP_regrel_nocond:
    case Acca::CALL_abs_nocond:
    case Acca::CALL_immrel_nocond:
    case Acca::CALL_regrel_nocond:
      return true;
    default:
      return false;
  }
}

static bool isCondBranchOpcode(int Opc) {
  switch (Opc) {
    case Acca::JMP_abs_cond:
    case Acca::JMP_immrel_cond:
    case Acca::JMP_regrel_cond:
    case Acca::CJMP_byte_abs:
    case Acca::CJMP_doublebyte_abs:
    case Acca::CJMP_quadbyte_abs:
    case Acca::CJMP_word_abs:
    case Acca::CJMP_byte_immrel:
    case Acca::CJMP_doublebyte_immrel:
    case Acca::CJMP_quadbyte_immrel:
    case Acca::CJMP_word_immrel:
    case Acca::CJMP_byte_regrel:
    case Acca::CJMP_doublebyte_regrel:
    case Acca::CJMP_quadbyte_regrel:
    case Acca::CJMP_word_regrel:
    case Acca::CALL_abs_cond:
    case Acca::CALL_immrel_cond:
    case Acca::CALL_regrel_cond:
      return true;
    default:
      return false;
  }
}

static bool isIndirectBranchOpcode(int Opc) {
  switch (Opc) {
    case Acca::JMP_abs_cond:
    case Acca::JMP_abs_nocond:
    case Acca::CJMP_byte_abs:
    case Acca::CJMP_doublebyte_abs:
    case Acca::CJMP_quadbyte_abs:
    case Acca::CJMP_word_abs:
    case Acca::CALL_abs_cond:
    case Acca::CALL_abs_nocond:
      return true;
    default:
      return false;
  }
}

unsigned AccaInstrInfo::removeBranch(MachineBasicBlock &MBB,
                                     int *BytesRemoved) const {
  assert(!BytesRemoved && "code size not handled");

  MachineBasicBlock::iterator I = MBB.end();
  unsigned Count = 0;
  while (I != MBB.begin()) {
    --I;

    if (I->isDebugInstr())
      continue;

    if (!isCondBranchOpcode(I->getOpcode()) &&
        !isUncondBranchOpcode(I->getOpcode()))
      break; // Not a branch

    I->eraseFromParent();
    I = MBB.end();
    ++Count;
  }
  return Count;
};

unsigned AccaInstrInfo::
insertBranch(MachineBasicBlock &MBB, MachineBasicBlock *TBB,
             MachineBasicBlock *FBB, ArrayRef<MachineOperand> Cond,
             const DebugLoc &DL,
             int *BytesAdded) const {
  llvm_unreachable("unimplemented");
};

unsigned AccaInstrInfo::
reduceLoopCount(MachineBasicBlock &MBB,
                MachineBasicBlock &PreHeader,
                MachineInstr *IndVar, MachineInstr &Cmp,
                SmallVectorImpl<MachineOperand> &Cond,
                SmallVectorImpl<MachineInstr *> &PrevInsts,
                unsigned Iter, unsigned MaxIter) const {
  llvm_unreachable("unimplemented");
};

void AccaInstrInfo::
copyPhysReg(MachineBasicBlock &MBB, MachineBasicBlock::iterator I,
            const DebugLoc &DL, MCRegister DestReg, MCRegister SrcReg,
            bool KillSrc) const {
  llvm_unreachable("unimplemented");
};

void AccaInstrInfo::
storeRegToStackSlot(MachineBasicBlock &MBB,
                    MachineBasicBlock::iterator MBBI, Register SrcReg,
                    bool isKill, int FrameIndex,
                    const TargetRegisterClass *RC,
                    const TargetRegisterInfo *TRI,
                    Register VReg) const {
  llvm_unreachable("unimplemented");
};

void AccaInstrInfo::
loadRegFromStackSlot(MachineBasicBlock &MBB,
                     MachineBasicBlock::iterator MBBI, Register DestReg,
                     int FrameIndex, const TargetRegisterClass *RC,
                     const TargetRegisterInfo *TRI,
                     Register VReg) const {
  llvm_unreachable("unimplemented");
};

unsigned AccaInstrInfo::
isLoadFromStackSlot(const MachineInstr &MI, int &FrameIndex) const {
  llvm_unreachable("unimplemented");
};

unsigned AccaInstrInfo::
isStoreToStackSlot(const MachineInstr &MI, int &FrameIndex) const {
  llvm_unreachable("unimplemented");
};

bool AccaInstrInfo::
reverseBranchCondition(SmallVectorImpl<MachineOperand> &Cond) const {
  llvm_unreachable("unimplemented");
};
