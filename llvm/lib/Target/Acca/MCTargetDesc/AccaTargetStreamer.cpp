//===-- AccaTargetStreamer.cpp - Acca Target Streamer Methods -----------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file provides Acca specific target streamer methods.
//
//===----------------------------------------------------------------------===//

#include "AccaTargetStreamer.h"
#include "AccaInstPrinter.h"
#include "llvm/MC/MCRegister.h"
#include "llvm/Support/FormattedStream.h"

using namespace llvm;

// pin vtable to this file
AccaTargetStreamer::AccaTargetStreamer(MCStreamer &S) : MCTargetStreamer(S) {}

void AccaTargetStreamer::anchor() {}

AccaTargetAsmStreamer::AccaTargetAsmStreamer(MCStreamer &S,
                                               formatted_raw_ostream &OS)
    : AccaTargetStreamer(S), OS(OS) {}

AccaTargetELFStreamer::AccaTargetELFStreamer(MCStreamer &S)
    : AccaTargetStreamer(S) {}

MCELFStreamer &AccaTargetELFStreamer::getStreamer() {
  return static_cast<MCELFStreamer &>(Streamer);
}
