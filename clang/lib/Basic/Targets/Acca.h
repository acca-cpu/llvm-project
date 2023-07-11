//===--- Acca.h - declare Acca target feature support ---------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file declares Acca TargetInfo objects.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_CLANG_LIB_BASIC_TARGETS_ACCA_H
#define LLVM_CLANG_LIB_BASIC_TARGETS_ACCA_H

#include "clang/Basic/TargetInfo.h"
#include "clang/Basic/TargetOptions.h"
#include "llvm/ADT/Triple.h"
#include "llvm/Support/Compiler.h"

namespace clang {
namespace targets {

class LLVM_LIBRARY_VISIBILITY AccaTargetInfo : public TargetInfo {
  static const char *const GCCRegNames[];

public:
  AccaTargetInfo(const llvm::Triple &Triple, const TargetOptions &)
    : TargetInfo(Triple) {
    // * little-endian
    // * stack aligned to 16 bytes (128 bits)
    // * 64-bit pointers
    // * 64-bit aligned 64-bit integers (and all other integers are naturally aligned as well)
    // * Mach-O symbol mangling
    // * CPU supports 8-, 16-, 32-, and 64-bit arithmetic efficiently
    resetDataLayout("e-S128-p:64:64-i64:64-m:o-n8:16:32:64");
    SuitableAlign = 128;
    SigAtomicType = UnsignedChar;
    PointerWidth = 64;
    PointerAlign = 64;
    TLSSupported = false;
    VLASupported = false;
    HasLongDouble = false;
    LongWidth = 64;
    LongAlign = 64;
  }

  void getTargetDefines(const LangOptions &Opts, MacroBuilder &Builder) const override;

  ArrayRef<const char*> getGCCRegNames() const override;

  ArrayRef<TargetInfo::GCCRegAlias> getGCCRegAliases() const override;

  ArrayRef<TargetInfo::AddlRegName> getGCCAddlRegNames() const override;

  BuiltinVaListKind getBuiltinVaListKind() const override {
    return TargetInfo::VoidPtrBuiltinVaList;
  }

  ArrayRef<Builtin::Info> getTargetBuiltins() const override {
    return std::nullopt;
  }

  bool validateAsmConstraint(const char *&Name, TargetInfo::ConstraintInfo &info) const override {
    return false;
  }

  const char *getClobbers() const override {
    return "";
  }
};

} // namespace targets
} // namespace clang

#endif // LLVM_CLANG_LIB_BASIC_TARGETS_ACCA_H
