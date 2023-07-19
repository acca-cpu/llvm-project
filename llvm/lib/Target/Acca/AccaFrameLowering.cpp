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
#include "MCTargetDesc/AccaBaseInfo.h"
#include "AccaInstrInfo.h"
#include "AccaMachineFunctionInfo.h"
#include "AccaRegisterInfo.h"
#include "AccaSubtarget.h"
#include "MCTargetDesc/AccaMCTargetDesc.h"
#include "llvm/CodeGen/MachineFrameInfo.h"
#include "llvm/CodeGen/MachineFunction.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/CodeGen/MachineModuleInfo.h"
#include "llvm/CodeGen/MachineRegisterInfo.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/IR/Function.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/ErrorHandling.h"
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
  MachineFrameInfo &MFI = MF.getFrameInfo();
  const AccaRegisterInfo *RI = Subtarget.getRegisterInfo();

  DebugLoc DL;

  // Determine the correct frame layout
  determineFrameLayout(MF);

  // First, compute final stack size.
  uint64_t StackSize = MFI.getStackSize();
  uint64_t RealStackSize = StackSize;

  // Early exit if there is no need to allocate space in the stack.
  if (StackSize == 0 && !MFI.adjustsStack())
    return;

  uint64_t FirstSPAdjustAmount = getFirstSPAdjustAmount(MF);
  uint64_t SecondSPAdjustAmount = RealStackSize - FirstSPAdjustAmount;
  // Split the SP adjustment to reduce the offsets of callee saved spill.
  if (FirstSPAdjustAmount)
    StackSize = FirstSPAdjustAmount;

  adjustReg(MBB, MBBI, DL, Acca::R13, Acca::R13, -StackSize, MachineInstr::FrameSetup);

  // Emit ".cfi_def_cfa_offset StackSize"
  unsigned CFIIndex = MF.addFrameInst(
      MCCFIInstruction::cfiDefCfaOffset(nullptr, StackSize));
  BuildMI(MBB, MBBI, DL, TII->get(TargetOpcode::CFI_INSTRUCTION))
      .addCFIIndex(CFIIndex)
      .setMIFlag(MachineInstr::FrameSetup);

  const auto &CSI = MFI.getCalleeSavedInfo();

  // The frame pointer is callee-saved, and code has been generated for us to
  // save it to the stack. We need to skip over the storing of callee-saved
  // registers as the frame pointer must be modified after it has been saved
  // to the stack, not before.
  std::advance(MBBI, CSI.size());

  // Iterate over list of callee-saved registers and emit .cfi_offset
  // directives.
  for (const auto &Entry : CSI) {
    int64_t Offset = MFI.getObjectOffset(Entry.getFrameIdx());
    unsigned CFIIndex = MF.addFrameInst(MCCFIInstruction::createOffset(
        nullptr, RI->getDwarfRegNum(Entry.getReg(), true), Offset));
    BuildMI(MBB, MBBI, DL, TII->get(TargetOpcode::CFI_INSTRUCTION))
        .addCFIIndex(CFIIndex)
        .setMIFlag(MachineInstr::FrameSetup);
  }

  // Generate new FP.
  if (hasFP(MF)) {
    adjustReg(MBB, MBBI, DL, Acca::R14, Acca::R13, StackSize,
              MachineInstr::FrameSetup);

    // Emit ".cfi_def_cfa $fp, 0"
    unsigned CFIIndex = MF.addFrameInst(
        MCCFIInstruction::cfiDefCfa(nullptr, RI->getDwarfRegNum(Acca::R14, true),
                                    0));
    BuildMI(MBB, MBBI, DL, TII->get(TargetOpcode::CFI_INSTRUCTION))
        .addCFIIndex(CFIIndex)
        .setMIFlag(MachineInstr::FrameSetup);
  }

  // Emit the second SP adjustment after saving callee saved registers.
  if (FirstSPAdjustAmount && SecondSPAdjustAmount) {
    if (hasFP(MF)) {
      assert(SecondSPAdjustAmount > 0 &&
             "SecondSPAdjustAmount should be greater than zero");
      adjustReg(MBB, MBBI, DL, Acca::R13, Acca::R13, -SecondSPAdjustAmount,
                MachineInstr::FrameSetup);
    } else {
      // FIXME: RegScavenger will place the spill instruction before the
      // prologue if a VReg is created in the prologue. This will pollute the
      // caller's stack data. Therefore, until there is better way, we just use
      // the `sub.w` instruction for stack adjustment to ensure that VReg
      // will not be created.
      for (int Val = SecondSPAdjustAmount; Val > 0; /* handled in the body */) {
        uint16_t Imm;
        uint8_t ShiftFactor;
        auto SubVal = AccaUtil::asUnsignedShiftedImm11RoundDown(Val, Imm,
                                                                ShiftFactor);
        BuildMI(MBB, MBBI, DL,
                TII->get(Acca::SUB_word_nonnull_imm_nocarry_nosetflags_noarith),
                Acca::R13)
          .addReg(Acca::R13)
          .addImm(Imm)
          .addImm(ShiftFactor)
          .setMIFlag(MachineInstr::FrameSetup);
        Val -= SubVal;
      }

      // If we are using a frame-pointer, and thus emitted ".cfi_def_cfa fp, 0",
      // don't emit an sp-based .cfi_def_cfa_offset
      // Emit ".cfi_def_cfa_offset RealStackSize"
      unsigned CFIIndex = MF.addFrameInst(
          MCCFIInstruction::cfiDefCfaOffset(nullptr, RealStackSize));
      BuildMI(MBB, MBBI, DL, TII->get(TargetOpcode::CFI_INSTRUCTION))
          .addCFIIndex(CFIIndex)
          .setMIFlag(MachineInstr::FrameSetup);
    }
  }

  if (hasFP(MF)) {
    // Realign stack.
    if (RI->hasStackRealignment(MF)) {
      llvm_unreachable("unimplemented");
    }
  }
};

void AccaFrameLowering::
emitEpilogue(MachineFunction &MF, MachineBasicBlock &MBB) const {
  MachineBasicBlock::iterator MBBI = MBB.getFirstTerminator();
  const AccaSubtarget &Subtarget = MF.getSubtarget<AccaSubtarget>();
  const TargetInstrInfo *TII = Subtarget.getInstrInfo();
  MachineFrameInfo &MFI = MF.getFrameInfo();
  DebugLoc DL = MBBI != MBB.end() ? MBBI->getDebugLoc() : DebugLoc();
  const AccaRegisterInfo *RI = Subtarget.getRegisterInfo();
  Register SPReg = Acca::R13;

  const auto &CSI = MFI.getCalleeSavedInfo();

  // Skip to before the restores of callee-saved registers.
  auto LastFrameDestroy = MBBI;
  if (!CSI.empty())
    LastFrameDestroy = std::prev(MBBI, CSI.size());

  // Get the number of bytes from FrameInfo.
  uint64_t StackSize = MFI.getStackSize();

  // Restore the stack pointer.
  if (RI->hasStackRealignment(MF) || MFI.hasVarSizedObjects()) {
    assert(hasFP(MF) && "frame pointer should not have been eliminated");
    llvm_unreachable("unimplemented");
  }

  uint64_t FirstSPAdjustAmount = getFirstSPAdjustAmount(MF);
  if (FirstSPAdjustAmount) {
    uint64_t SecondSPAdjustAmount = StackSize - FirstSPAdjustAmount;
    assert(SecondSPAdjustAmount > 0 &&
           "SecondSPAdjustAmount should be greater than zero");

    adjustReg(MBB, LastFrameDestroy, DL, SPReg, SPReg, SecondSPAdjustAmount,
              MachineInstr::FrameDestroy);
    StackSize = FirstSPAdjustAmount;
  }

  // Deallocate stack
  adjustReg(MBB, MBBI, DL, SPReg, SPReg, StackSize, MachineInstr::FrameDestroy);
};

// Determine the size of the frame and maximum call frame size.
void AccaFrameLowering::determineFrameLayout(MachineFunction &MF) const {
  MachineFrameInfo &MFI = MF.getFrameInfo();

  // Get the number of bytes to allocate from the FrameInfo.
  uint64_t FrameSize = MFI.getStackSize();

  // Make sure the frame is aligned.
  FrameSize = alignTo(FrameSize, getStackAlign());

  // Update frame info.
  MFI.setStackSize(FrameSize);
}

void AccaFrameLowering::adjustReg(MachineBasicBlock &MBB,
                                  MachineBasicBlock::iterator MBBI,
                                  const DebugLoc &DL, Register DestReg,
                                  Register SrcReg, int64_t Val,
                                  MachineInstr::MIFlag Flag) const {
  MachineFunction& MF = *MBB.getParent();
  const AccaSubtarget &Subtarget = MF.getSubtarget<AccaSubtarget>();
  const AccaInstrInfo *TII = Subtarget.getInstrInfo();

  if (DestReg == SrcReg && Val == 0)
    return;

  if (DestReg != SrcReg && Val == 0) {
    // simplify it to just a copy
    BuildMI(MBB, MBBI, DL, TII->get(Acca::COPY_word), DestReg)
      .addReg(SrcReg)
      .setMIFlag(Flag);
    return;
  }

  uint16_t ShiftedImmBase;
  uint8_t ShiftedImmFactor;

  // try the unsigned immediate first
  if (AccaUtil::asUnsignedShiftedImm11(Val, ShiftedImmBase,
      ShiftedImmFactor)) {
    // add.w $DestReg, $SrcReg, imm_base, imm_shift_factor, 0, 0, 0
    BuildMI(MBB, MBBI, DL,
            TII->get(Acca::ADD_word_nonnull_imm_nocarry_nosetflags_noarith),
            DestReg)
      .addReg(SrcReg)
      .addImm(ShiftedImmBase)
      .addImm(ShiftedImmFactor)
      .setMIFlag(Flag);
    return;
  }

  // now try the signed immediate
  if (AccaUtil::asSignedShiftedImm11(Val, ShiftedImmBase, ShiftedImmFactor)) {
    // add.w $DestReg, $SrcReg, imm_base, imm_shift_factor, 1, 0, 0
    BuildMI(MBB, MBBI, DL,
            TII->get(Acca::ADD_word_nonnull_imm_nocarry_nosetflags_arith),
            DestReg)
      .addReg(SrcReg)
      .addImm(ShiftedImmBase)
      .addImm(ShiftedImmFactor)
      .setMIFlag(Flag);
    return;
  }

  if (Val < 0) {
    // try subtracting
    auto NegVal = -Val;

    if (AccaUtil::asUnsignedShiftedImm11(NegVal, ShiftedImmBase,
      ShiftedImmFactor)) {
      // sub.w $DestReg, $SrcReg, imm_base, imm_shift_factor, 0, 0, 0
      BuildMI(MBB, MBBI, DL,
              TII->get(Acca::SUB_word_nonnull_imm_nocarry_nosetflags_noarith),
              DestReg)
        .addReg(SrcReg)
        .addImm(ShiftedImmBase)
        .addImm(ShiftedImmFactor)
        .setMIFlag(Flag);
      return;
    }

    if (AccaUtil::asSignedShiftedImm11(NegVal, ShiftedImmBase, ShiftedImmFactor)) {
      // sub.w $DestReg, $SrcReg, imm_base, imm_shift_factor, 1, 0, 0
      BuildMI(MBB, MBBI, DL,
              TII->get(Acca::SUB_word_nonnull_imm_nocarry_nosetflags_arith),
              DestReg)
        .addReg(SrcReg)
        .addImm(ShiftedImmBase)
        .addImm(ShiftedImmFactor)
        .setMIFlag(Flag);
      return;
    }
  }

  // <<load immediate into $ScratchReg>>
  // {add,sub}.w $DestReg, $SrcReg, $ScratchReg, 0, 0

  unsigned Opc = Acca::ADD_word_nonnull_reg_nocarry_nosetflags;
  if (Val < 0) {
    Val = -Val;
    Opc = Acca::SUB_word_nonnull_reg_nocarry_nosetflags;
  }

  MachineRegisterInfo &MRI = MBB.getParent()->getRegInfo();
  Register ScratchReg = MRI.createVirtualRegister(&Acca::I64RegsRegClass);
  TII->movImm(MBB, MBBI, DL, ScratchReg, Val, Flag);
  BuildMI(MBB, MBBI, DL, TII->get(Opc), DestReg)
      .addReg(SrcReg)
      .addReg(ScratchReg, RegState::Kill)
      .setMIFlag(Flag);
}

MachineBasicBlock::iterator AccaFrameLowering::
eliminateCallFramePseudoInstr(MachineFunction &MF,
                              MachineBasicBlock &MBB,
                              MachineBasicBlock::iterator I) const {
  Register SPReg = Acca::R13;
  DebugLoc DL = I->getDebugLoc();

  if (!hasReservedCallFrame(MF)) {
    // If space has not been reserved for a call frame, ADJCALLSTACKDOWN and
    // ADJCALLSTACKUP must be converted to instructions manipulating the stack
    // pointer. This is necessary when there is a variable length stack
    // allocation (e.g. alloca), which means it's not possible to allocate
    // space for outgoing arguments from within the function prologue.
    int64_t Amount = I->getOperand(0).getImm();

    if (Amount != 0) {
      // Ensure the stack remains aligned after adjustment.
      Amount = alignSPAdjust(Amount);

      if (I->getOpcode() == Acca::ADJCALLSTACKDOWN)
        Amount = -Amount;

      adjustReg(MBB, I, DL, SPReg, SPReg, Amount, MachineInstr::NoFlags);
    }
  }

  return MBB.erase(I);
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

  // Unconditionally spill rfp if the function uses a frame pointer.
  if (hasFP(MF)) {
    SavedRegs.set(Acca::R14);
  }
};

StackOffset AccaFrameLowering::
getFrameIndexReference(const MachineFunction &MF, int FI,
                       Register &FrameReg) const {
  const AccaSubtarget &Subtarget = MF.getSubtarget<AccaSubtarget>();
  const MachineFrameInfo &MFI = MF.getFrameInfo();
  const AccaRegisterInfo *RI = Subtarget.getRegisterInfo();

  uint64_t StackSize = MFI.getStackSize();
  uint64_t FirstSPAdjustAmount = getFirstSPAdjustAmount(MF);

  // Callee-saved registers should be referenced relative to the stack
  // pointer (positive offset), otherwise use the frame pointer (negative
  // offset).
  const auto &CSI = MFI.getCalleeSavedInfo();
  int MinCSFI = 0;
  int MaxCSFI = -1;
  StackOffset Offset =
      StackOffset::getFixed(MFI.getObjectOffset(FI) - getOffsetOfLocalArea() +
                            MFI.getOffsetAdjustment());

  if (CSI.size()) {
    MinCSFI = CSI[0].getFrameIdx();
    MaxCSFI = CSI[CSI.size() - 1].getFrameIdx();
  }

  if (FI >= MinCSFI && FI <= MaxCSFI) {
    FrameReg = Acca::R13;
    if (FirstSPAdjustAmount)
      Offset += StackOffset::getFixed(FirstSPAdjustAmount);
    else
      Offset += StackOffset::getFixed(StackSize);
  } else if (RI->hasStackRealignment(MF) && !MFI.isFixedObjectIndex(FI)) {
    llvm_unreachable("unimplemented");
  } else {
    FrameReg = RI->getFrameRegister(MF);
    if (hasFP(MF))
      Offset += StackOffset::getFixed(0);
    else
      Offset += StackOffset::getFixed(StackSize);
  }

  return Offset;
};

uint64_t AccaFrameLowering::
getFirstSPAdjustAmount(const MachineFunction &MF) const {
  const MachineFrameInfo &MFI = MF.getFrameInfo();
  const std::vector<CalleeSavedInfo> &CSI = MFI.getCalleeSavedInfo();
  uint64_t StackSize = MFI.getStackSize();

  uint64_t ShiftedImm = AccaUtil::roundDownUnsignedShiftedImm11(StackSize);

  // if the stack size doesn't fit into an 11-bit shifted immediate AND we need
  // to save registers, split the stack adjustment
  if (ShiftedImm != StackSize && (CSI.size() > 0)) {
    // ShiftedImm is the largest possible adjustment we can make to the stack
    // with a single `sub` instruction.
    return ShiftedImm;
  }

  return 0;
};
