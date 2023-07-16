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
#include "AccaRegisterInfo.h"
#include "AccaSubtarget.h"
#include "MCTargetDesc/AccaMCTargetDesc.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/CodeGen/MachineFrameInfo.h"
#include "llvm/CodeGen/MachineInstr.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/CodeGen/MachineMemOperand.h"
#include "llvm/CodeGen/MachineRegisterInfo.h"
#include "llvm/MC/MCRegisterInfo.h"
#include "llvm/MC/TargetRegistry.h"
#include "llvm/Support/ErrorHandling.h"
#include "AccaBaseInfo.h"

using namespace llvm;

#define GET_INSTRINFO_CTOR_DTOR
#include "AccaGenInstrInfo.inc"

// Pin the vtable to this file.
void AccaInstrInfo::anchor() {}

AccaInstrInfo::AccaInstrInfo()
    : AccaGenInstrInfo(Acca::ADJCALLSTACKDOWN, Acca::ADJCALLSTACKUP), RI() {}

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
  MachineRegisterInfo& MRI = MBB.getParent()->getRegInfo();

  if (Acca::CCRRegClass.contains(DestReg) &&
      Acca::CCRRegClass.contains(SrcReg)) {
    llvm_unreachable("Unexpected CCR register copy");
  }

  {
    unsigned Opcode = Acca::INSTRUCTION_LIST_END;
    unsigned SrcBits = MRI.getTargetRegisterInfo()->getRegSizeInBits(SrcReg, MRI);
    unsigned DestBits = MRI.getTargetRegisterInfo()->getRegSizeInBits(DestReg, MRI);
    unsigned DestSubreg = Acca::NoSubRegister;
    // DestReg may be smaller than SrcReg, but Acca only supports symmetric register copies.
    // so, just copy to the equivalent super-register. since we're only called post-RA, we can
    // safely do so.
    MCRegister ActualDestReg = DestReg;

    if (DestBits <= SrcBits) {
      const TargetRegisterClass* SuperRegClass = nullptr;
      if (SrcBits == 8) {
        Opcode = Acca::COPY_byte;
        SuperRegClass = &Acca::I8RegsRegClass;
      } else if (SrcBits == 16) {
        Opcode = Acca::COPY_doublebyte;
        SuperRegClass = &Acca::I16RegsRegClass;
      } else if (SrcBits == 32) {
        Opcode = Acca::COPY_quadbyte;
        SuperRegClass = &Acca::I32RegsRegClass;
      } else if (SrcBits == 64) {
        Opcode = Acca::COPY_word;
        SuperRegClass = &Acca::I64RegsRegClass;
      }
      if (DestBits < SrcBits) {
        if (DestBits == 8) {
          DestSubreg = Acca::subreg_byte;
        } else if (DestBits == 16) {
          DestSubreg = Acca::subreg_doublebyte;
        } else if (DestBits == 32) {
          DestSubreg = Acca::subreg_quadbyte;
        }
        ActualDestReg = MRI.getTargetRegisterInfo()->getMatchingSuperReg(DestReg, DestSubreg, SuperRegClass);
      }
    }

    if (Opcode != Acca::INSTRUCTION_LIST_END) {
      BuildMI(MBB, I, DL, get(Opcode), ActualDestReg)
        .addReg(SrcReg, getKillRegState(KillSrc));
      if (ActualDestReg != DestReg) {
        BuildMI(MBB, I, DL, get(Acca::KILL))
          .addReg(DestReg, RegState::Define)
          .addReg(ActualDestReg, RegState::Kill);
      }
      return;
    }
  }

  if (Acca::CCRRegClass.contains(DestReg)) {
    assert(Acca::I64RegsRegClass.contains(SrcReg) &&
           "CCR register update must have 64-bit source register");

    llvm_unreachable("TODO: allow register-to-flags copy");
  }

  if (Acca::CCRRegClass.contains(SrcReg)) {
    assert(Acca::I64RegsRegClass.contains(DestReg) &&
           "CCR register update must have 64-bit source register");

    BuildMI(MBB, I, DL, get(Acca::LDM), DestReg)
      .addImm(AccaSysReg::flags);

    BuildMI(MBB, I, DL, get(Acca::AND_word_nonnull_imm_noarith_nosetflags), DestReg)
      .addReg(DestReg)
      .addImm(0xf) // the CZOS bits are bits 0-3
      .addImm(0);
  }

  llvm_unreachable("Invalid register copy");
};

void AccaInstrInfo::
storeRegToStackSlot(MachineBasicBlock &MBB,
                    MachineBasicBlock::iterator MBBI, Register SrcReg,
                    bool IsKill, int FrameIndex,
                    const TargetRegisterClass *RC,
                    const TargetRegisterInfo *TRI,
                    Register VReg) const {
  DebugLoc DL;
  if (MBBI != MBB.end())
    DL = MBBI->getDebugLoc();
  MachineFunction *MF = MBB.getParent();
  MachineFrameInfo &MFI = MF->getFrameInfo();

  MachineMemOperand *MMO = MF->getMachineMemOperand(
    MachinePointerInfo::getFixedStack(*MF, FrameIndex),
    MachineMemOperand::MOStore, MFI.getObjectSize(FrameIndex),
    MFI.getObjectAlign(FrameIndex));

  BuildMI(MBB, MBBI, DL, get(Acca::STS_word))
    .addMemOperand(MMO)
    .addFrameIndex(FrameIndex)
    .addReg(SrcReg, getKillRegState(IsKill));
};

void AccaInstrInfo::
loadRegFromStackSlot(MachineBasicBlock &MBB,
                     MachineBasicBlock::iterator MBBI, Register DestReg,
                     int FrameIndex, const TargetRegisterClass *RC,
                     const TargetRegisterInfo *TRI,
                     Register VReg) const {
  DebugLoc DL;
  if (MBBI != MBB.end())
    DL = MBBI->getDebugLoc();
  MachineFunction *MF = MBB.getParent();
  MachineFrameInfo &MFI = MF->getFrameInfo();

  MachineMemOperand *MMO = MF->getMachineMemOperand(
      MachinePointerInfo::getFixedStack(*MF, FrameIndex),
      MachineMemOperand::MOLoad, MFI.getObjectSize(FrameIndex),
      MFI.getObjectAlign(FrameIndex));

  BuildMI(MBB, MBBI, DL, get(Acca::LDS_word), DestReg)
    .addFrameIndex(FrameIndex)
    .addMemOperand(MMO);
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

void AccaInstrInfo::movImm(MachineBasicBlock &MBB,
                           MachineBasicBlock::iterator MBBI,
                           const DebugLoc &DL, Register DstReg,
                           uint64_t Val, MachineInstr::MIFlag Flag) const {
  // TODO: optimize this better according to the value. for now, we just build
  //       a bunch of LDIs shifted by 16 bits each time. however, LDI also
  //       accepts arbitrary shifts to load 16-bits anywhere, so we can check
  //       if the value would fit into 16 bits shifted and use that instead.

  MachineRegisterInfo &MRI = MBB.getParent()->getRegInfo();

  Register PrevReg;

  // FIXME: this whole virtual register dance is required to preserve SSA form.
  //        however, i'm not sure if it's actually guaranteed that a destination
  //        register of the same class as a killed register will be assigned to the
  //        killed register. from initial testing, it seems to be true, but i
  //        would prefer something more concrete.
  //
  //        really, we should just emit a pseudo-instr (PseudoLDI) and change that
  //        instr to be expanded during MCInstLower. the current code is temporary.

  for (uint8_t DoubleBytes = 0; DoubleBytes < 4; ++DoubleBytes) {
    Register ThisDstReg = DstReg;
    bool FitsInImm = isUInt<16>(Val);

    if (!FitsInImm) {
      ThisDstReg = MRI.createVirtualRegister(&Acca::I64RegsRegClass);
    }

    if (DoubleBytes == 0) {
      BuildMI(MBB, MBBI, DL, get(Acca::LDI_noshift_clearall), ThisDstReg)
        .addImm(Val & 0xffff)
        .setMIFlag(Flag);
    } else {
      BuildMI(MBB, MBBI, DL, get(Acca::LDI), ThisDstReg)
        .addImm(Val & 0xffff)
        .addImm(DoubleBytes * 16)
        .addImm(0)
        .addReg(PrevReg, RegState::ImplicitKill)
        .setMIFlag(Flag);
    }

    PrevReg = ThisDstReg;
    Val >>= 16;

    if (FitsInImm) {
      break;
    }
  }
};
