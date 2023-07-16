//===-- AccaELFObjectWriter.cpp - Acca ELF Writer -----------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

//#include "MCTargetDesc/AccaFixupKinds.h"
//#include "MCTargetDesc/AccaMCExpr.h"
#include "AccaFixupKinds.h"
#include "MCTargetDesc/AccaMCTargetDesc.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/BinaryFormat/ELF.h"
#include "llvm/MC/MCContext.h"
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
} // namespace

unsigned AccaELFObjectWriter::
getRelocType(MCContext &Ctx, const MCValue &Target,
             const MCFixup &Fixup, bool IsPCRel) const {
  // Determine the type of the relocation
  unsigned Kind = Fixup.getTargetKind();

  if (Kind >= FirstLiteralRelocationKind)
    return Kind - FirstLiteralRelocationKind;

  switch (Kind) {
  default:
    Ctx.reportError(Fixup.getLoc(), "Unsupported relocation type");
    return ELF::R_LARCH_NONE;
  case FK_Data_1:
    Ctx.reportError(Fixup.getLoc(), "1-byte data relocations not supported");
    return ELF::R_LARCH_NONE;
  case FK_Data_2:
    Ctx.reportError(Fixup.getLoc(), "2-byte data relocations not supported");
    return ELF::R_LARCH_NONE;
  case FK_Data_4:
    return IsPCRel ? ELF::R_LARCH_32_PCREL : ELF::R_LARCH_32;
  case FK_Data_8:
    return ELF::R_LARCH_64;
  case Acca::fixup_acca_rel22:
    return ELF::R_ACCA_REL22;
  case Acca::fixup_acca_rel13:
    return ELF::R_ACCA_REL13;
  case Acca::fixup_acca_rel64_d0:
    return ELF::R_ACCA_REL64_D0;
  case Acca::fixup_acca_rel64_d1:
    return ELF::R_ACCA_REL64_D1;
  case Acca::fixup_acca_rel64_d2:
    return ELF::R_ACCA_REL64_D2;
  case Acca::fixup_acca_rel64_d3:
    return ELF::R_ACCA_REL64_D3;
  case Acca::fixup_acca_rel22_byte:
    return ELF::R_ACCA_REL22_BYTE;
    // TODO: Handle more fixup-kinds.
  }
};

std::unique_ptr<MCObjectTargetWriter>
llvm::createAccaELFObjectWriter(uint8_t OSABI) {
  return std::make_unique<AccaELFObjectWriter>(OSABI);
}
