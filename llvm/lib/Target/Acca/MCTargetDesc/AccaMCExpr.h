//=--- AccaMCExpr.h - Acca specific MC expression classes ---*- C++ -*-=//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file describes Acca-specific MCExprs.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_ACCA_MCTARGETDESC_ACCAMCEXPR_H
#define LLVM_LIB_TARGET_ACCA_MCTARGETDESC_ACCAMCEXPR_H

#include "llvm/MC/MCExpr.h"
#include "llvm/Support/ErrorHandling.h"

namespace llvm {

class AccaMCExpr : public MCTargetExpr {
public:
  enum VariantKind {
    VK_NONE,
    VK_ABS,
    VK_CALL,
    VK_CALL_PLT,
    VK_REL64_D0,
    VK_REL64_D1,
    VK_REL64_D2,
    VK_REL64_D3,
    VK_INVALID = 0xfff,
  };
private:
  const MCExpr *Expr;
  const VariantKind Kind;

  explicit AccaMCExpr(const MCExpr *Expr, VariantKind Kind)
    : Expr(Expr), Kind(Kind) {}

public:
  static const AccaMCExpr *create(const MCExpr *Expr, VariantKind Kind,
                                   MCContext &Ctx);

  VariantKind getKind() const { return Kind; }

  const MCExpr *getSubExpr() const { return Expr; }

  StringRef getVariantKindName() const;

  void printImpl(raw_ostream &OS, const MCAsmInfo *MAI) const override;

  void visitUsedExpr(MCStreamer &Streamer) const override;

  MCFragment *findAssociatedFragment() const override;

  bool evaluateAsRelocatableImpl(MCValue &Res, const MCAsmLayout *Layout,
                                 const MCFixup *Fixup) const override;

  void fixELFSymbolsInTLSFixups(MCAssembler &Asm) const override;

  static bool classof(const MCExpr *E) {
    return E->getKind() == MCExpr::Target;
  }

  static bool classof(const AccaMCExpr *) { return true; }

  static VariantKind getVariantKindForName(StringRef name);
};
} // end namespace llvm

#endif
