//===-- AccaRegisterInfo.cpp - Acca Register Information ----------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file contains the Acca implementation of the TargetRegisterInfo class.
//
//===----------------------------------------------------------------------===//

#include "AccaRegisterInfo.h"
#include "Acca.h"
#include "AccaMachineFunctionInfo.h"
#include "AccaSubtarget.h"
#include "llvm/ADT/BitVector.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/CodeGen/MachineFrameInfo.h"
#include "llvm/CodeGen/MachineFunction.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/CodeGen/RegisterScavenging.h"
#include "llvm/CodeGen/TargetInstrInfo.h"
#include "llvm/IR/Type.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/ErrorHandling.h"

using namespace llvm;

#define GET_REGINFO_TARGET_DESC
#include "AccaGenRegisterInfo.inc"

AccaRegisterInfo::AccaRegisterInfo():
  AccaGenRegisterInfo(Acca::R15) {};

const MCPhysReg *AccaRegisterInfo::
getCalleeSavedRegs(const MachineFunction *MF) const {
  return CSR_Acca_SaveList;
};

const uint32_t *AccaRegisterInfo::
getCallPreservedMask(const MachineFunction &MF,
                     CallingConv::ID CC) const {
  return CSR_Acca_RegMask;
};

BitVector AccaRegisterInfo::
getReservedRegs(const MachineFunction &MF) const {
  BitVector Reserved(getNumRegs());

  // the stack pointer (r13/rsp) and link register (r15/rlr) are always reserved
  Reserved.set(Acca::R13);
  Reserved.set(Acca::R15);

  // likewise, so are their subregisters
  Reserved.set(Acca::R13B);
  Reserved.set(Acca::R13D);
  Reserved.set(Acca::R13Q);
  Reserved.set(Acca::R15B);
  Reserved.set(Acca::R15D);
  Reserved.set(Acca::R15Q);

  return Reserved;
};

const TargetRegisterClass *AccaRegisterInfo::
getPointerRegClass(const MachineFunction &MF,
                   unsigned Kind) const {
  return &Acca::I64RegsRegClass;
};

bool AccaRegisterInfo::
eliminateFrameIndex(MachineBasicBlock::iterator II,
                    int SPAdj, unsigned FIOperandNum,
                    RegScavenger *RS) const {
  MachineInstr &MI = *II;
  DebugLoc dl = MI.getDebugLoc();
  int FrameIndex = MI.getOperand(FIOperandNum).getIndex();
  MachineFunction &MF = *MI.getParent()->getParent();
  const AccaFrameLowering *TFI = getFrameLowering(MF);

  Register FrameReg;
  int Offset;
  Offset = TFI->getFrameIndexReference(MF, FrameIndex, FrameReg).getFixed();

  Offset += MI.getOperand(FIOperandNum + 1).getImm();

  const TargetInstrInfo &TII = *MF.getSubtarget().getInstrInfo();

  if (Offset >= -1024 && Offset <= 1023) {
    // TODO: Acca also supports shifted immediates, but for now we don't implement that here
    MI.getOperand(FIOperandNum).ChangeToRegister(FrameReg, false);
    MI.getOperand(FIOperandNum + 1).ChangeToImmediate(Offset);
  } else {
    Register TempReg = RS->scavengeRegister(&Acca::I64RegsRegClass, II, SPAdj);

    if (TempReg == Acca::NoRegister) {
      llvm_unreachable("Failed to scavenge register for frame index elimination");
    }

    // load the offset into a temporary register

    // ldi <tmp>, (offset & 0xffff), 0, 3 (clear all)
    BuildMI(*MI.getParent(), II, dl, TII.get(Acca::LDI), TempReg)
      .addImm((uint64_t)Offset & 0xffff)
      .addImm(0)
      .addImm(3);

    BuildMI(*MI.getParent(), II, dl, TII.get(Acca::LDI), TempReg)
      .addImm(((uint64_t)Offset >> 16) & 0xffff)
      .addImm(16)
      .addImm(0);

    BuildMI(*MI.getParent(), II, dl, TII.get(Acca::LDI), TempReg)
      .addImm(((uint64_t)Offset >> 32) & 0xffff)
      .addImm(32)
      .addImm(0);

    BuildMI(*MI.getParent(), II, dl, TII.get(Acca::LDI), TempReg)
      .addImm(((uint64_t)Offset >> 48) & 0xffff)
      .addImm(48)
      .addImm(0);

    MI.getOperand(FIOperandNum).ChangeToRegister(FrameReg, false);
    MI.getOperand(FIOperandNum + 1).ChangeToRegister(TempReg, false);
  }

  return false;
};

Register AccaRegisterInfo::
getFrameRegister(const MachineFunction &MF) const {
  return Acca::R14;
};

bool AccaRegisterInfo::
canRealignStack(const MachineFunction &MF) const {
  return false;
};
