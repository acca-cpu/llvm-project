//===-- AccaAsmBackend.cpp - Acca Assembler Backend ---------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

//#include "MCTargetDesc/AccaFixupKinds.h"
#include "MCTargetDesc/AccaMCTargetDesc.h"
#include "llvm/ADT/StringSwitch.h"
#include "llvm/MC/MCAsmBackend.h"
#include "llvm/MC/MCELFObjectWriter.h"
#include "llvm/MC/MCExpr.h"
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
      return 0;
    }

    std::optional<MCFixupKind> getFixupKind(StringRef Name) const override {
      return std::nullopt;
    }

    const MCFixupKindInfo &getFixupKindInfo(MCFixupKind Kind) const override {
      abort();
    }

    bool shouldForceRelocation(const MCAssembler &Asm, const MCFixup &Fixup,
                               const MCValue &Target) override {
      return false;
    }

    bool fixupNeedsRelaxation(const MCFixup &Fixup,
                              uint64_t Value,
                              const MCRelaxableFragment *DF,
                              const MCAsmLayout &Layout) const override {
      abort();
    }
    void relaxInstruction(MCInst &Inst,
                          const MCSubtargetInfo &STI) const override {
      abort();
    }

    bool writeNopData(raw_ostream &OS, uint64_t Count,
                      const MCSubtargetInfo *STI) const override {
      abort();
    }
  };

  class ELFAccaAsmBackend : public AccaAsmBackend {
    Triple::OSType OSType;
  public:
    ELFAccaAsmBackend(const Target &T, Triple::OSType OSType) :
      AccaAsmBackend(T), OSType(OSType) { }

    void applyFixup(const MCAssembler &Asm, const MCFixup &Fixup,
                    const MCValue &Target, MutableArrayRef<char> Data,
                    uint64_t Value, bool IsResolved,
                    const MCSubtargetInfo *STI) const override {
      abort();
    }

    std::unique_ptr<MCObjectTargetWriter>
    createObjectTargetWriter() const override {
      uint8_t OSABI = MCELFObjectTargetWriter::getOSABI(OSType);
      return createAccaELFObjectWriter(OSABI);
    }
  };

} // end anonymous namespace

MCAsmBackend *llvm::createAccaAsmBackend(const Target &T,
                                         const MCSubtargetInfo &STI,
                                         const MCRegisterInfo &MRI,
                                         const MCTargetOptions &Options) {
  return new ELFAccaAsmBackend(T, STI.getTargetTriple().getOS());
}
