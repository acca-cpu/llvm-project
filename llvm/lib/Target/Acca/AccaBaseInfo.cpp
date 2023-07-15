//===----- AccaBaseInfo.cpp - Acca Base encoding information---------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file provides basic encoding and assembly information for Acca.
//
//===----------------------------------------------------------------------===//
#include "AccaBaseInfo.h"
#include "MCTargetDesc/AccaMCTargetDesc.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/Support/MathExtras.h"
#include <cstdint>

using namespace llvm;

namespace llvm {
  namespace AccaSysReg {
#define GET_SYSREG_IMPL
#include "AccaGenSystemOperands.inc"
  } // namespace AccaSysReg
} // namespace llvm

bool llvm::AccaUtil::asUnsignedShiftedImm11(uint64_t Value, uint16_t& Imm,
                                            uint8_t& ShiftFactor) {
  // Acca has shift factors of up to (and including) 6 and 7, but that would
  // just shift 66 or 77 bits, which wouldn't really make sense
  constexpr uint64_t U64Bits = CHAR_BIT * sizeof(uint64_t);
  for (uint8_t Factor = 0; Factor < 6; ++Factor) {
    auto Shift = static_cast<uint64_t>(Factor) * 11;
    auto NextShift = (Shift + 11 >= U64Bits) ? 63 : (Shift + 11);
    auto Mask = maskTrailingOnes<uint64_t>(Shift);
    if ((Value & Mask) == 0 && (Value >> NextShift) == 0) {
      Imm = static_cast<uint16_t>((Value >> Shift) & 0x7ff);
      ShiftFactor = Factor;
      return true;
    }
  }
  return false;
}

bool llvm::AccaUtil::asSignedShiftedImm11(int64_t Value, uint16_t& Imm,
                                          uint8_t& ShiftFactor) {
  uint64_t UnsignedValue = static_cast<uint64_t>(Value);
  constexpr uint64_t U64Bits = CHAR_BIT * sizeof(uint64_t);
  constexpr uint64_t I64Bits = CHAR_BIT * sizeof(int64_t);
  static_assert(U64Bits == I64Bits);
  for (uint8_t Factor = 0; Factor < 6; ++Factor) {
    auto Shift = static_cast<uint64_t>(Factor) * 11;
    auto NextShift = (Shift + 11 >= U64Bits) ? 63 : (Shift + 11);
    auto Mask = maskTrailingOnes<uint64_t>(Shift);
    bool Fits;
    // for both the positive and negative cases, the checks are similar to the
    // ones in asUnsignedShiftedImm11, except that we additionally need to
    // make sure the sign bit within the base of the immediate would match the
    // result (since this is what the processor uses to sign-extend to the rest
    // of the immediate).
    if (Value < 0) {
      Fits = (UnsignedValue & Mask) == 0 &&
             (Value >> NextShift) == INT64_MAX &&
             ((UnsignedValue >> Shift) & (1ull << 10)) != 0;
    } else {
      Fits = (UnsignedValue & Mask) == 0 &&
             (UnsignedValue >> NextShift) == 0 &&
             ((UnsignedValue >> Shift) & (1ull << 10)) == 0;
    }
    if (Fits) {
      Imm = static_cast<uint16_t>((UnsignedValue >> Shift) & 0x7ff);
      ShiftFactor = Factor;
      return true;
    }
  }
  return false;
}

uint64_t llvm::AccaUtil::
asUnsignedShiftedImm11RoundDown(uint64_t Value, uint16_t& Imm,
                                uint8_t& ShiftFactor) {
  constexpr uint64_t U64Bits = CHAR_BIT * sizeof(uint64_t);
  for (uint8_t FactorPlusOne = 6; FactorPlusOne > 0; --FactorPlusOne) {
    auto Factor = FactorPlusOne - 1;
    auto Shift = static_cast<uint64_t>(Factor) * 11;
    auto NextShift = (Shift + 11 >= U64Bits) ? 63 : (Shift + 11);
    auto Base = static_cast<uint16_t>((Value >> Shift) & 0x7ff);
    if ((Value >> NextShift) == 0 && Base != 0) {
      Imm = Base;
      ShiftFactor = Factor;
      return static_cast<uint64_t>(Imm) << (static_cast<uint64_t>(ShiftFactor) * 11);
    }
  }

  // the only case where this should happen is when Value is zero
  assert(Value == 0);
  Imm = 0;
  ShiftFactor = 0;
  return 0;
}

uint64_t llvm::AccaUtil::roundDownUnsignedShiftedImm11(uint64_t Value) {
  [[maybe_unused]]
  uint16_t UnusedImm;
  [[maybe_unused]]
  uint8_t UnusedShiftFactor;
  return asUnsignedShiftedImm11RoundDown(Value, UnusedImm, UnusedShiftFactor);
}
