//===------ AccaFixupKinds.h - Acca Specific Fixup Entries ------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_ACCA_MCTARGETDESC_ACCAFIXUPKINDS_H
#define LLVM_LIB_TARGET_ACCA_MCTARGETDESC_ACCAFIXUPKINDS_H

#include "llvm/BinaryFormat/ELF.h"
#include "llvm/MC/MCFixup.h"

#undef Acca

namespace llvm {
namespace Acca {
//
// This table *must* be in the same order of
// MCFixupKindInfo Infos[Acca::NumTargetFixupKinds] in
// AccaAsmBackend.cpp.
//
enum Fixups {
  // Define fixups that can be handled by AccaAsmBackend::applyFixup.
  //
  // 22-bit fixup corresponding to foo/%plt(foo) for instruction callr/jmpr.
  fixup_acca_rel22 = FirstTargetFixupKind,
  // 13-bit fixup corresponding to foo/%plt(foo) for instruction cjmpr.
  fixup_acca_rel13,
  // 16-bit fixup corresponding to %rel64_d0(foo) for instruction ldi.
  fixup_acca_rel64_d0,
  // 16-bit fixup corresponding to %rel64_d1(foo) for instruction ldi.
  fixup_acca_rel64_d1,
  // 16-bit fixup corresponding to %rel64_d2(foo) for instruction ldi.
  fixup_acca_rel64_d2,
  // 16-bit fixup corresponding to %rel64_d3(foo) for instruction ldi.
  fixup_acca_rel64_d3,
  // 22-bit fixup corresponding to %rel22_byte(foo) for instruction ldr.
  fixup_acca_rel22_byte,

  // Used as a sentinel, must be the last of the fixup which can be handled by
  // AccaAsmBackend::applyFixup.
  fixup_acca_invalid,
  NumTargetFixupKinds = fixup_acca_invalid - FirstTargetFixupKind,

  // Define fixups for force relocation as FirstLiteralRelocationKind+V.
  // V represents the relocation type.
  //
  // TODO
};
} // end namespace Acca
} // end namespace llvm

#endif
