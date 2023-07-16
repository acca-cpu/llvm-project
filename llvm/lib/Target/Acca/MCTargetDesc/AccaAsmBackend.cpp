//===-- AccaAsmBackend.cpp - Acca Assembler Backend ---------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

//#include "MCTargetDesc/AccaFixupKinds.h"
#include "AccaFixupKinds.h"
#include "MCTargetDesc/AccaMCTargetDesc.h"
#include "llvm/ADT/StringSwitch.h"
#include "llvm/MC/MCAsmBackend.h"
#include "llvm/MC/MCAssembler.h"
#include "llvm/MC/MCContext.h"
#include "llvm/MC/MCELFObjectWriter.h"
#include "llvm/MC/MCExpr.h"
#include "llvm/MC/MCFixup.h"
#include "llvm/MC/MCFixupKindInfo.h"
#include "llvm/MC/MCObjectWriter.h"
#include "llvm/MC/MCSubtargetInfo.h"
#include "llvm/MC/MCValue.h"
#include "llvm/MC/TargetRegistry.h"
#include "llvm/Support/EndianStream.h"

using namespace llvm;

namespace {
  class AccaAsmBackend : public MCAsmBackend {
  protected:
    const Target &TheTarget;

  public:
    AccaAsmBackend(const Target &T)
        : MCAsmBackend(support::little),
          TheTarget(T) {}

    unsigned getNumFixupKinds() const override {
      llvm_unreachable("unimplemented");
    }

    std::optional<MCFixupKind> getFixupKind(StringRef Name) const override {
      llvm_unreachable("unimplemented");
    }

    const MCFixupKindInfo &getFixupKindInfo(MCFixupKind Kind) const override {
      llvm_unreachable("unimplemented");
    }

    bool shouldForceRelocation(const MCAssembler &Asm, const MCFixup &Fixup,
                               const MCValue &Target) override {
      llvm_unreachable("unimplemented");
    }

    bool fixupNeedsRelaxation(const MCFixup &Fixup,
                              uint64_t Value,
                              const MCRelaxableFragment *DF,
                              const MCAsmLayout &Layout) const override {
      llvm_unreachable("unimplemented");
    }

    void relaxInstruction(MCInst &Inst,
                          const MCSubtargetInfo &STI) const override {
      llvm_unreachable("unimplemented");
    }

    bool writeNopData(raw_ostream &OS, uint64_t Count,
                      const MCSubtargetInfo *STI) const override {
      llvm_unreachable("unimplemented");
    }
  };

  class ELFAccaAsmBackend : public AccaAsmBackend {
    Triple::OSType OSType;
  public:
    ELFAccaAsmBackend(const Target &T, Triple::OSType OSType) :
      AccaAsmBackend(T), OSType(OSType) { }

    unsigned getNumFixupKinds() const override {
      return Acca::NumTargetFixupKinds;
    }

    std::optional<MCFixupKind> getFixupKind(StringRef Name) const override {
      auto Type = llvm::StringSwitch<unsigned>(Name)
#define ELF_RELOC(X, Y) .Case(#X, Y)
#include "llvm/BinaryFormat/ELFRelocs/Acca.def"
#undef ELF_RELOC
        .Case("BFD_RELOC_NONE", ELF::R_ACCA_NONE)
        .Default(-1u);
      if (Type != -1u)
        return static_cast<MCFixupKind>(FirstLiteralRelocationKind + Type);
      return std::nullopt;
    }

    const MCFixupKindInfo &getFixupKindInfo(MCFixupKind Kind) const override;

    bool shouldForceRelocation(const MCAssembler &Asm, const MCFixup &Fixup,
                               const MCValue &Target) override;

    bool fixupNeedsRelaxation(const MCFixup &Fixup,
                              uint64_t Value,
                              const MCRelaxableFragment *DF,
                              const MCAsmLayout &Layout) const override {
      return false;
    }

    void relaxInstruction(MCInst &Inst,
                          const MCSubtargetInfo &STI) const override {}

    bool writeNopData(raw_ostream &OS, uint64_t Count,
                      const MCSubtargetInfo *STI) const override;

    void applyFixup(const MCAssembler &Asm, const MCFixup &Fixup,
                    const MCValue &Target, MutableArrayRef<char> Data,
                    uint64_t Value, bool IsResolved,
                    const MCSubtargetInfo *STI) const override;

    std::unique_ptr<MCObjectTargetWriter>
    createObjectTargetWriter() const override {
      uint8_t OSABI = MCELFObjectTargetWriter::getOSABI(OSType);
      return createAccaELFObjectWriter(OSABI);
    }
  };

} // end anonymous namespace

const MCFixupKindInfo &ELFAccaAsmBackend::
getFixupKindInfo(MCFixupKind Kind) const {
  const static MCFixupKindInfo Infos[] = {
    // This table *must* be in the order that the fixup_* kinds are defined in
    // AccaFixupKinds.h.
    //
    // {name, offset, bits, flags}
    {"fixup_acca_rel22", 0, 22, MCFixupKindInfo::FKF_IsPCRel},
    {"fixup_acca_rel13", 0, 13, MCFixupKindInfo::FKF_IsPCRel},
    {"fixup_acca_rel64_d0", 0, 16, MCFixupKindInfo::FKF_IsPCRel},
    {"fixup_acca_rel64_d1", 0, 16, MCFixupKindInfo::FKF_IsPCRel},
    {"fixup_acca_rel64_d2", 0, 16, MCFixupKindInfo::FKF_IsPCRel},
    {"fixup_acca_rel64_d3", 0, 16, MCFixupKindInfo::FKF_IsPCRel},
    {"fixup_acca_rel22_byte", 0, 22, MCFixupKindInfo::FKF_IsPCRel},
    // TODO: Add more fixup kinds.
  };

  static_assert((std::size(Infos)) == Acca::NumTargetFixupKinds,
                "Not all fixup kinds added to Infos array");

  // Fixup kinds from .reloc directive are like R_ACCA_NONE. They
  // do not require any extra processing.
  if (Kind >= FirstLiteralRelocationKind)
    return MCAsmBackend::getFixupKindInfo(FK_NONE);

  if (Kind < FirstTargetFixupKind)
    return MCAsmBackend::getFixupKindInfo(Kind);

  assert(unsigned(Kind - FirstTargetFixupKind) < getNumFixupKinds() &&
        "Invalid kind!");
  return Infos[Kind - FirstTargetFixupKind];
}

bool ELFAccaAsmBackend::
shouldForceRelocation(const MCAssembler &Asm, const MCFixup &Fixup,
                      const MCValue &Target) {
  if (Fixup.getKind() >= FirstLiteralRelocationKind)
    return true;
  switch (Fixup.getTargetKind()) {
  default:
    return false;
  case FK_Data_1:
  case FK_Data_2:
  case FK_Data_4:
  case FK_Data_8:
    return !Target.isAbsolute();
  }
}

static void reportOutOfRangeError(MCContext &Ctx, SMLoc Loc, unsigned N) {
  Ctx.reportError(Loc, "fixup value out of range [" + Twine(llvm::minIntN(N)) +
                           ", " + Twine(llvm::maxIntN(N)) + "]");
}

static uint64_t adjustFixupValue(const MCFixup &Fixup, uint64_t Value,
                                 MCContext &Ctx) {
  switch (Fixup.getTargetKind()) {
  default:
    llvm_unreachable("Unknown fixup kind");
  case FK_Data_1:
  case FK_Data_2:
  case FK_Data_4:
  case FK_Data_8:
    return Value;
  case Acca::fixup_acca_rel22: {
    // LLVM's PCRel calculation provides the value with the current PC as the base;
    // Acca calculates PCRel values using the *next* instruction's PC as the base
    Value -= 4;
    if (!isInt<24>(Value))
      reportOutOfRangeError(Ctx, Fixup.getLoc(), 24);
    if (Value % 4)
      Ctx.reportError(Fixup.getLoc(), "fixup value must be 4-byte aligned");
    return static_cast<int64_t>(Value >> 2) & 0x3fffff;
  }
  case Acca::fixup_acca_rel13: {
    Value -= 4;
    if (!isInt<15>(Value))
      reportOutOfRangeError(Ctx, Fixup.getLoc(), 15);
    if (Value % 4)
      Ctx.reportError(Fixup.getLoc(), "fixup value must be 4-byte aligned");
    return static_cast<int64_t>(Value >> 2) & 0x1fff;
  }
  case Acca::fixup_acca_rel64_d0:
    return (static_cast<int64_t>(Value - 4) >> 2) & 0xffff;
  case Acca::fixup_acca_rel64_d1:
    return ((static_cast<int64_t>(Value - 4) >> 2) >> 16) & 0xffff;
  case Acca::fixup_acca_rel64_d2:
    return ((static_cast<int64_t>(Value - 4) >> 2) >> 32) & 0xffff;
  case Acca::fixup_acca_rel64_d3:
    return ((static_cast<int64_t>(Value - 4) >> 2) >> 48) & 0xffff;
  case Acca::fixup_acca_rel22_byte: {
    if (!isInt<22>(Value))
      reportOutOfRangeError(Ctx, Fixup.getLoc(), 22);
    return (Value - 4) & 0x3fffff;
  }
  }
};

void ELFAccaAsmBackend::
applyFixup(const MCAssembler &Asm, const MCFixup &Fixup,
           const MCValue &Target, MutableArrayRef<char> Data,
           uint64_t Value, bool IsResolved,
           const MCSubtargetInfo *STI) const {
  if (!Value)
    return; // Doesn't change encoding.

  MCFixupKind Kind = Fixup.getKind();
  if (Kind >= FirstLiteralRelocationKind)
    return;
  MCFixupKindInfo Info = getFixupKindInfo(Kind);
  MCContext &Ctx = Asm.getContext();

  // Apply any target-specific value adjustments.
  Value = adjustFixupValue(Fixup, Value, Ctx);

  // Shift the value into position.
  Value <<= Info.TargetOffset;

  unsigned Offset = Fixup.getOffset();
  unsigned NumBytes = alignTo(Info.TargetSize + Info.TargetOffset, 8) / 8;

  assert(Offset + NumBytes <= Data.size() && "Invalid fixup offset!");
  // For each byte of the fragment that the fixup touches, mask in the
  // bits from the fixup value.
  for (unsigned I = 0; I != NumBytes; ++I) {
    Data[Offset + I] |= uint8_t((Value >> (I * 8)) & 0xff);
  }
}

bool ELFAccaAsmBackend::
writeNopData(raw_ostream &OS, uint64_t Count,
             const MCSubtargetInfo *STI) const {
  // We mostly follow binutils' convention here: align to 4-byte boundary with a
  // 0-fill padding.
  OS.write_zeros(Count % 4);

  // The remainder is now padded with 4-byte nops.
  for (; Count >= 4; Count -= 4)
    OS.write("\0\0\0\x04", 4);

  return true;
}

MCAsmBackend *llvm::createAccaAsmBackend(const Target &T,
                                         const MCSubtargetInfo &STI,
                                         const MCRegisterInfo &MRI,
                                         const MCTargetOptions &Options) {
  return new ELFAccaAsmBackend(T, STI.getTargetTriple().getOS());
}
