//===-- AccaELFObjectWriter.cpp - Acca ELF Writer -----------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

//#include "MCTargetDesc/AccaFixupKinds.h"
//#include "MCTargetDesc/AccaMCExpr.h"
#include "MCTargetDesc/AccaMCTargetDesc.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/MC/MCELFObjectWriter.h"
#include "llvm/MC/MCExpr.h"
#include "llvm/MC/MCObjectWriter.h"
#include "llvm/MC/MCValue.h"
#include "llvm/Support/ErrorHandling.h"

using namespace llvm;

namespace {
  class AccaELFObjectWriter : public MCELFObjectTargetWriter {
  public:
    AccaELFObjectWriter(uint8_t OSABI)
      : MCELFObjectTargetWriter(true, OSABI, ELF::EM_ACCA,
                                /*HasRelocationAddend*/ true) {}

    ~AccaELFObjectWriter() override = default;

  protected:
    unsigned getRelocType(MCContext &Ctx, const MCValue &Target,
                          const MCFixup &Fixup, bool IsPCRel) const override;
  };
}

unsigned AccaELFObjectWriter::
getRelocType(MCContext &Ctx, const MCValue &Target,
             const MCFixup &Fixup, bool IsPCRel) const {
  // TODO
  return ELF::R_ACCA_NONE;
};

std::unique_ptr<MCObjectTargetWriter>
llvm::createAccaELFObjectWriter(uint8_t OSABI) {
  return std::make_unique<AccaELFObjectWriter>(OSABI);
}
