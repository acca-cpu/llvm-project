//===--- Acca.cpp - Implement Acca target feature support -------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file implements Acca TargetInfo objects.
//
//===----------------------------------------------------------------------===//

#include "Acca.h"
#include "clang/Basic/MacroBuilder.h"
#include "llvm/ADT/StringSwitch.h"

using namespace clang;
using namespace clang::targets;

const char *const AccaTargetInfo::GCCRegNames[] = {
  "r0", "r1",  "r2",  "r3",  "r4",  "r5",  "r6",  "r7",
  "r8", "r9", "r10", "r11", "r12", "r13", "r14", "r15",
};

const TargetInfo::GCCRegAlias GCCRegAliases[] = {
  {{"rsp"}, "r13"}, {{"rfp"}, "r14"}, {{"rlr"}, "r15"},
};

const TargetInfo::AddlRegName AddlRegNames[] = {
  {{ "r0b",  "r0d",  "r0w",  "r0q"},  0},
  {{ "r1b",  "r1d",  "r1w",  "r1q"},  1},
  {{ "r2b",  "r2d",  "r2w",  "r2q"},  2},
  {{ "r3b",  "r3d",  "r3w",  "r3q"},  3},
  {{ "r4b",  "r4d",  "r4w",  "r4q"},  4},
  {{ "r5b",  "r5d",  "r5w",  "r5q"},  5},
  {{ "r6b",  "r6d",  "r6w",  "r6q"},  6},
  {{ "r7b",  "r7d",  "r7w",  "r7q"},  7},
  {{ "r8b",  "r8d",  "r8w",  "r8q"},  8},
  {{ "r9b",  "r9d",  "r9w",  "r9q"},  9},
  {{"r10b", "r10d", "r10w", "r10q"}, 10},
  {{"r11b", "r11d", "r11w", "r11q"}, 11},
  {{"r12b", "r12d", "r12w", "r12q"}, 12},
  {{"r13b", "r13d", "r13w", "r13q"}, 13},
  {{"r14b", "r14d", "r14w", "r14q"}, 14},
  {{"r15b", "r15d", "r15w", "r15q"}, 15},
};

ArrayRef<const char *> AccaTargetInfo::getGCCRegNames() const {
  return llvm::ArrayRef(GCCRegNames);
}

ArrayRef<TargetInfo::GCCRegAlias> AccaTargetInfo::getGCCRegAliases() const {
  return llvm::ArrayRef(GCCRegAliases);
}

ArrayRef<TargetInfo::AddlRegName> AccaTargetInfo::getGCCAddlRegNames() const {
  return llvm::ArrayRef(AddlRegNames);
}

void AccaTargetInfo::getTargetDefines(const LangOptions &Opts, MacroBuilder &Builder) const {
  Builder.defineMacro("__ACCA__");
}
