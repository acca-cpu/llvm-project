//===-- AccaTargetStreamer.h - Acca Target Streamer ----------*- C++ -*--===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_ACCA_MCTARGETDESC_ACCATARGETSTREAMER_H
#define LLVM_LIB_TARGET_ACCA_MCTARGETDESC_ACCATARGETSTREAMER_H

#include "llvm/MC/MCELFStreamer.h"
#include "llvm/MC/MCStreamer.h"

namespace llvm {

class formatted_raw_ostream;

class AccaTargetStreamer : public MCTargetStreamer {
  virtual void anchor();

public:
  AccaTargetStreamer(MCStreamer &S);
};

// This part is for ascii assembly output
class AccaTargetAsmStreamer : public AccaTargetStreamer {
  formatted_raw_ostream &OS;

public:
  AccaTargetAsmStreamer(MCStreamer &S, formatted_raw_ostream &OS);
};

// This part is for ELF object output
class AccaTargetELFStreamer : public AccaTargetStreamer {
public:
  AccaTargetELFStreamer(MCStreamer &S);
  MCELFStreamer &getStreamer();
};
} // end namespace llvm

#endif
