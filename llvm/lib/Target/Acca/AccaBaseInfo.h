//===----- AccaBaseInfo.h - Top level definitions for Acca ------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file contains small standalone helper functions and enum definitions for
// the Acca target useful for the compiler back-end and the MC libraries.
// As such, it deliberately does not include references to LLVM core
// code gen types, passes, etc..
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_ACCA_ACCABASEINFO_H
#define LLVM_LIB_TARGET_ACCA_ACCABASEINFO_H

#include "llvm/ADT/StringRef.h"
#include <cstdint>
namespace llvm {
// This namespace holds all of the target specific flags that instruction info
// tracks.
namespace AccaII {
enum {
  MO_None,
  MO_CALL,
  MO_CALL_PLT,
  MO_REL64_D0,
  MO_REL64_D1,
  MO_REL64_D2,
  MO_REL64_D3,
  // TODO: Add more flags.
};
} // namespace AccaII

namespace AccaSysReg {
  struct SysReg {
    const char *Name;
    const char *AltName;
    unsigned Encoding;
    bool Readable;
    bool Writeable;
  };

  #define GET_SYSREG_DECL
  #include "AccaGenSystemOperands.inc"

  const SysReg *lookupSysRegByName(StringRef);
  const SysReg *lookupSysRegByEncoding(uint32_t);
} // namespace AccaSysReg

namespace AccaUtil {
  bool asUnsignedShiftedImm11(uint64_t Value, uint16_t& Imm,
                              uint8_t& ShiftFactor);
  bool asSignedShiftedImm11(int64_t Value, uint16_t& Imm,
                            uint8_t& ShiftFactor);
  uint64_t asUnsignedShiftedImm11RoundDown(uint64_t Value, uint16_t& Imm,
                                           uint8_t& ShiftFactor);
  uint64_t roundDownUnsignedShiftedImm11(uint64_t Value);
} // namespace AccaUtil
} // end namespace llvm

#endif
