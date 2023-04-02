//===-- AccaMCTargetDesc.cpp - Acca Target Descriptions -----------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file provides Acca specific target descriptions.
//
//===----------------------------------------------------------------------===//

#include "AccaMCTargetDesc.h"
#include "AccaInstPrinter.h"
#include "AccaMCAsmInfo.h"
#include "AccaTargetStreamer.h"
#include "TargetInfo/AccaTargetInfo.h"
#include "llvm/MC/MCInstrInfo.h"
#include "llvm/MC/MCRegisterInfo.h"
#include "llvm/MC/MCSubtargetInfo.h"
#include "llvm/MC/TargetRegistry.h"
#include "llvm/Support/ErrorHandling.h"

using namespace llvm;

#define GET_INSTRINFO_MC_DESC
#define ENABLE_INSTR_PREDICATE_VERIFIER
#include "AccaGenInstrInfo.inc"

#define GET_SUBTARGETINFO_MC_DESC
#include "AccaGenSubtargetInfo.inc"

#define GET_REGINFO_MC_DESC
#include "AccaGenRegisterInfo.inc"

static MCAsmInfo *createAccaMCAsmInfo(const MCRegisterInfo &MRI,
                                       const Triple &TT,
                                       const MCTargetOptions &Options) {
  MCAsmInfo *MAI = new AccaELFMCAsmInfo(TT);
  unsigned Reg = MRI.getDwarfRegNum(Acca::R13, true);
  MCCFIInstruction Inst = MCCFIInstruction::cfiDefCfa(nullptr, Reg, 0);
  MAI->addInitialFrameState(Inst);
  return MAI;
}

static MCInstrInfo *createAccaMCInstrInfo() {
  MCInstrInfo *X = new MCInstrInfo();
  InitAccaMCInstrInfo(X);
  return X;
}

static MCRegisterInfo *createAccaMCRegisterInfo(const Triple &TT) {
  MCRegisterInfo *X = new MCRegisterInfo();
  InitAccaMCRegisterInfo(X, Acca::R15);
  return X;
}

static MCSubtargetInfo *
createAccaMCSubtargetInfo(const Triple &TT, StringRef CPU, StringRef FS) {
  if (CPU.empty())
    CPU = "generic";
  return createAccaMCSubtargetInfoImpl(TT, CPU, /*TuneCPU*/ CPU, FS);
}

static MCTargetStreamer *
createObjectTargetStreamer(MCStreamer &S, const MCSubtargetInfo &STI) {
  return new AccaTargetELFStreamer(S);
}

static MCTargetStreamer *createTargetAsmStreamer(MCStreamer &S,
                                                 formatted_raw_ostream &OS,
                                                 MCInstPrinter *InstPrint,
                                                 bool isVerboseAsm) {
  return new AccaTargetAsmStreamer(S, OS);
}

static MCTargetStreamer *createNullTargetStreamer(MCStreamer &S) {
  return new AccaTargetStreamer(S);
}

static MCInstPrinter *createAccaMCInstPrinter(const Triple &T,
                                               unsigned SyntaxVariant,
                                               const MCAsmInfo &MAI,
                                               const MCInstrInfo &MII,
                                               const MCRegisterInfo &MRI) {
  return new AccaInstPrinter(MAI, MII, MRI);
}

extern "C" LLVM_EXTERNAL_VISIBILITY void LLVMInitializeAccaTargetMC() {
  // Register the MC asm info.
  RegisterMCAsmInfoFn X(getTheAccaTarget(), createAccaMCAsmInfo);

  Target* T = &getTheAccaTarget();

  // Register the MC instruction info.
  TargetRegistry::RegisterMCInstrInfo(*T, createAccaMCInstrInfo);

  // Register the MC register info.
  TargetRegistry::RegisterMCRegInfo(*T, createAccaMCRegisterInfo);

  // Register the MC subtarget info.
  TargetRegistry::RegisterMCSubtargetInfo(*T, createAccaMCSubtargetInfo);

  // Register the MC Code Emitter.
  TargetRegistry::RegisterMCCodeEmitter(*T, createAccaMCCodeEmitter);

  // Register the asm backend.
  TargetRegistry::RegisterMCAsmBackend(*T, createAccaAsmBackend);

  // Register the object target streamer.
  TargetRegistry::RegisterObjectTargetStreamer(*T,
                                                createObjectTargetStreamer);

  // Register the asm streamer.
  TargetRegistry::RegisterAsmTargetStreamer(*T, createTargetAsmStreamer);

  // Register the null streamer.
  TargetRegistry::RegisterNullTargetStreamer(*T, createNullTargetStreamer);

  // Register the MCInstPrinter
  TargetRegistry::RegisterMCInstPrinter(*T, createAccaMCInstPrinter);
}
