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
  const AccaFrameLowering *TFI = getFrameLowering(MF);
  BitVector Reserved(getNumRegs());

  // the stack pointer (r13/rsp) and link register (r15/rlr) are always reserved
  markSuperRegs(Reserved, Acca::R13);
  markSuperRegs(Reserved, Acca::R15);

  if (TFI->hasFP(MF))
    markSuperRegs(Reserved, Acca::R14); // rfp

  assert(checkAllSuperRegsMarked(Reserved));
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
  DebugLoc DL = MI.getDebugLoc();
  int FrameIndex = MI.getOperand(FIOperandNum).getIndex();
  MachineFunction &MF = *MI.getParent()->getParent();
  const AccaFrameLowering *TFI = getFrameLowering(MF);

  Register FrameReg;
  int Offset;
  Offset = TFI->getFrameIndexReference(MF, FrameIndex, FrameReg).getFixed();

  const TargetInstrInfo &TII = *MF.getSubtarget().getInstrInfo();

  bool OffsetInRange = Offset >= -1024 && Offset <= 1023;
  bool IsLoadOrStore = MI.getOpcode() == Acca::LDS_word ||
                       MI.getOpcode() == Acca::STS_word;
  bool NeedTemp = !OffsetInRange || IsLoadOrStore;

  if (!IsLoadOrStore)
    Offset += MI.getOperand(FIOperandNum + 1).getImm();

  Register TempReg = 0;

  if (NeedTemp) {
    if (RS)
      TempReg = RS->scavengeRegister(&Acca::I64RegsRegClass, II, SPAdj);
    else
      TempReg = MI.getMF()->getRegInfo().createVirtualRegister(&Acca::I64RegsRegClass);

    if (TempReg == Acca::NoRegister)
      llvm_unreachable("Failed to scavenge register for frame index elimination");
  }

  if (!OffsetInRange) {
    // load the offset into a temporary register

    // ldi <tmp>, (offset & 0xffff), 0, 3 (clear all)
    BuildMI(*MI.getParent(), II, DL, TII.get(Acca::LDI), TempReg)
      .addImm((uint64_t)Offset & 0xffff)
      .addImm(0)
      .addImm(3);

    BuildMI(*MI.getParent(), II, DL, TII.get(Acca::LDI), TempReg)
      .addImm(((uint64_t)Offset >> 16) & 0xffff)
      .addImm(16)
      .addImm(0);

    BuildMI(*MI.getParent(), II, DL, TII.get(Acca::LDI), TempReg)
      .addImm(((uint64_t)Offset >> 32) & 0xffff)
      .addImm(32)
      .addImm(0);

    BuildMI(*MI.getParent(), II, DL, TII.get(Acca::LDI), TempReg)
      .addImm(((uint64_t)Offset >> 48) & 0xffff)
      .addImm(48)
      .addImm(0);
  }

  MachineInstrBuilder AddMIB;

  if (IsLoadOrStore) {
    // the operands will be updated later
    AddMIB = BuildMI(*MI.getParent(), II, DL,
        TII.get(Acca::ADD_word_nonnull_imm_nocarry_nosetflags_arith), TempReg)
      .addReg(Register())
      .addImm(0)
      .addImm(0);
  }

  MachineInstr& AddMI = IsLoadOrStore ? *AddMIB : MI;

  switch (AddMI.getOpcode()) {
    case Acca::ADD_word_nonnull_imm_nocarry_nosetflags_arith:
    case Acca::ADD_word_nonnull_imm_nocarry_nosetflags_noarith:
      break;
    default:
      llvm_unreachable("Unexpected instruction with FrameIndex operand");
  }

  // make sure the add instruction is uses sign-extension
  AddMIB = BuildMI(*MI.getParent(), II, DL,
    TII.get(Acca::ADD_word_nonnull_imm_nocarry_nosetflags_arith),
    AddMI.getOperand(0).getReg());
  AddMI.eraseFromParent();

  if (OffsetInRange) {
    // TODO: Acca also supports shifted immediates, but for now we don't implement that here
    AddMIB
      .addReg(FrameReg)
      .addImm(Offset)
      .addImm(0);
  } else {
    AddMIB
      .addReg(FrameReg)
      .addReg(TempReg, RegState::Kill);
  }

  if (IsLoadOrStore) {
    MI.getOperand(FIOperandNum).ChangeToRegister(TempReg, false, false, true);
  }

  return false;
};

Register AccaRegisterInfo::
getFrameRegister(const MachineFunction &MF) const {
  const AccaFrameLowering *TFI = getFrameLowering(MF);
  return TFI->hasFP(MF) ? Acca::R14 : Acca::R13;
};

bool AccaRegisterInfo::
canRealignStack(const MachineFunction &MF) const {
  return false;
};

bool AccaRegisterInfo::requiresRegisterScavenging(
    const MachineFunction &MF) const {
  return true;
}
