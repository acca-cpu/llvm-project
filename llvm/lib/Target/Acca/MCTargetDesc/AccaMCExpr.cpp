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
  case VK_CALL_PLT: return "plt";
  case VK_REL64_D0: return "rel64_d0";
  case VK_REL64_D1: return "rel64_d1";
  case VK_REL64_D2: return "rel64_d2";
  case VK_REL64_D3: return "rel64_d3";
  default:
    llvm_unreachable("Invalid ELF symbol kind");
  }
}

AccaMCExpr::VariantKind AccaMCExpr::getVariantKindForName(StringRef name) {
  return StringSwitch<AccaMCExpr::VariantKind>(name)
    .Case("plt", VK_CALL_PLT)
    .Case("rel64_d0", VK_REL64_D0)
    .Case("rel64_d1", VK_REL64_D1)
    .Case("rel64_d2", VK_REL64_D2)
    .Case("rel64_d3", VK_REL64_D3)
    .Default(VK_INVALID);
};

void AccaMCExpr::printImpl(raw_ostream &OS, const MCAsmInfo *MAI) const {
  VariantKind Kind = getKind();
  bool HasVariant =
      ((Kind != VK_NONE) && (Kind != VK_CALL));

  if (HasVariant)
    OS << '%' << getVariantKindName() << '(';
  Expr->print(OS, MAI);
  if (HasVariant)
    OS << ')';
}

void AccaMCExpr::visitUsedExpr(MCStreamer &Streamer) const {
  Streamer.visitUsedExpr(*getSubExpr());
}

MCFragment *AccaMCExpr::findAssociatedFragment() const {
  return getSubExpr()->findAssociatedFragment();
}

bool AccaMCExpr::evaluateAsRelocatableImpl(MCValue &Res,
                                              const MCAsmLayout *Layout,
                                              const MCFixup *Fixup) const {
  if (!getSubExpr()->evaluateAsRelocatable(Res, Layout, Fixup))
    return false;

  Res =
      MCValue::get(Res.getSymA(), Res.getSymB(), Res.getConstant(), getKind());

  return Res.getSymB() ? getKind() == VK_NONE : true;
}

void AccaMCExpr::fixELFSymbolsInTLSFixups(MCAssembler &Asm) const {
  switch (getKind()) {
  default:
    return;
  // TODO: once we have TLS support in Acca, we'll need to introduce relocs for this
  }
}

