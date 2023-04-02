//===-- AccaMCExpr.cpp - Acca specific MC expression classes --------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file contains the implementation of the assembly expression modifiers
// accepted by the Acca architecture.
//
//===----------------------------------------------------------------------===//

#include "AccaMCExpr.h"
#include "llvm/BinaryFormat/ELF.h"
#include "llvm/MC/MCContext.h"
#include "llvm/MC/MCStreamer.h"
#include "llvm/MC/MCSymbolELF.h"
#include "llvm/MC/MCValue.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/ErrorHandling.h"

using namespace llvm;

#define DEBUG_TYPE "accasymbolrefexpr"

const AccaMCExpr *AccaMCExpr::create(const MCExpr *Expr, VariantKind Kind,
                                       MCContext &Ctx) {
  return new (Ctx) AccaMCExpr(Expr, Kind);
}

StringRef AccaMCExpr::getVariantKindName() const {
  switch (static_cast<uint32_t>(getKind())) {
  case VK_ABS:                return "";
  default:
    llvm_unreachable("Invalid ELF symbol kind");
  }
}

void AccaMCExpr::printImpl(raw_ostream &OS, const MCAsmInfo *MAI) const {
  OS << getVariantKindName();
  Expr->print(OS, MAI);
}

void AccaMCExpr::visitUsedExpr(MCStreamer &Streamer) const {
  Streamer.visitUsedExpr(*getSubExpr());
}

MCFragment *AccaMCExpr::findAssociatedFragment() const {
  llvm_unreachable("unimplemented");
}

bool AccaMCExpr::evaluateAsRelocatableImpl(MCValue &Res,
                                              const MCAsmLayout *Layout,
                                              const MCFixup *Fixup) const {
  if (!getSubExpr()->evaluateAsRelocatable(Res, Layout, Fixup))
    return false;

  Res =
      MCValue::get(Res.getSymA(), Res.getSymB(), Res.getConstant(), getKind());

  return true;
}

void AccaMCExpr::fixELFSymbolsInTLSFixups(MCAssembler &Asm) const {
  llvm_unreachable("unimplemented");
}

