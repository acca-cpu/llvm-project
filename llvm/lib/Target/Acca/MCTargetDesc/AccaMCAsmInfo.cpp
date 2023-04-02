//===- AccaMCAsmInfo.cpp - Acca asm properties --------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file contains the declarations of the AccaMCAsmInfo properties.
//
//===----------------------------------------------------------------------===//

#include "AccaMCAsmInfo.h"
//#include "AccaMCExpr.h"
#include "llvm/ADT/Triple.h"
#include "llvm/BinaryFormat/Dwarf.h"
#include "llvm/MC/MCExpr.h"
#include "llvm/MC/MCStreamer.h"
#include "llvm/MC/MCTargetOptions.h"

using namespace llvm;

void AccaELFMCAsmInfo::anchor() {}

AccaELFMCAsmInfo::AccaELFMCAsmInfo(const Triple &TheTriple) {
  CodePointerSize = CalleeSaveStackSlotSize = 8;

  MinInstAlignment = 4;

  DollarIsPC = true;
  DotIsPC = false;

  SeparatorString = "\n";

  SupportsQuotedNames = false;

  HasLEB128Directives = false;

  // TODO: support these in the Acca assembler
  AsciiDirective = nullptr;
  AscizDirective = nullptr;

  ByteListDirective = "\t.write.b\t";

  Data8bitsDirective = "\t.write.b\t";
  Data16bitsDirective = "\t.write.d\t";
  Data32bitsDirective = "\t.write.q\t";
  Data64bitsDirective = "\t.write.w\t";

  UsesELFSectionDirectiveForBSS = true;

  HasBasenameOnlyForFileDirective = false;

  HasDotTypeDotSizeDirective = false;

  // the Acca assembler doesn't support visibility yet
  HiddenVisibilityAttr = MCSA_Invalid;
  HiddenDeclarationVisibilityAttr = MCSA_Invalid;
  ProtectedVisibilityAttr = MCSA_Invalid;
}
