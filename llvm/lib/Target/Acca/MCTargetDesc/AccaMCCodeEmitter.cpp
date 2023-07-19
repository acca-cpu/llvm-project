//===-- AccaMCCodeEmitter.cpp - Convert Acca code to machine code -------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file implements the AccaMCCodeEmitter class.
//
//===----------------------------------------------------------------------===//

//#include "MCTargetDesc/AccaFixupKinds.h"
//#include "AccaMCExpr.h"
#include "AccaFixupKinds.h"
#include "AccaMCExpr.h"
#include "AccaMCTargetDesc.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/MC/MCAsmInfo.h"
#include "llvm/MC/MCCodeEmitter.h"
#include "llvm/MC/MCContext.h"
#include "llvm/MC/MCExpr.h"
#include "llvm/MC/MCFixup.h"
#include "llvm/MC/MCInst.h"
#include "llvm/MC/MCInstrInfo.h"
#include "llvm/MC/MCObjectFileInfo.h"
#include "llvm/MC/MCRegisterInfo.h"
#include "llvm/MC/MCSubtargetInfo.h"
#include "llvm/MC/MCSymbol.h"
#include "llvm/MC/SubtargetFeature.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/Endian.h"
#include "llvm/Support/EndianStream.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/raw_ostream.h"
#include <cassert>
#include <cstdint>

using namespace llvm;

#define DEBUG_TYPE "mccodeemitter"

STATISTIC(MCNumEmitted, "Number of MC instructions emitted.");

namespace {

class AccaMCCodeEmitter : public MCCodeEmitter {
  MCContext &Ctx;

public:
  AccaMCCodeEmitter(const MCInstrInfo &, MCContext &ctx)
      : Ctx(ctx) {}
  AccaMCCodeEmitter(const AccaMCCodeEmitter &) = delete;
  AccaMCCodeEmitter &operator=(const AccaMCCodeEmitter &) = delete;
  ~AccaMCCodeEmitter() override = default;

  void encodeInstruction(const MCInst &MI, raw_ostream &OS,
                         SmallVectorImpl<MCFixup> &Fixups,
                         const MCSubtargetInfo &STI) const override;

  // getBinaryCodeForInstr - TableGen'erated function for getting the
  // binary encoding for an instruction.
  uint64_t getBinaryCodeForInstr(const MCInst &MI,
                                 SmallVectorImpl<MCFixup> &Fixups,
                                 const MCSubtargetInfo &STI) const;

  /// getMachineOpValue - Return binary encoding of operand. If the machine
  /// operand requires relocation, record the relocation and return zero.
  unsigned getMachineOpValue(const MCInst &MI, const MCOperand &MO,
                             SmallVectorImpl<MCFixup> &Fixups,
                             const MCSubtargetInfo &STI) const;
  unsigned getShiftedImmOpValue(const MCInst &MI, unsigned OpNo,
                            SmallVectorImpl<MCFixup> &Fixups,
                            const MCSubtargetInfo &STI) const;
  unsigned getExprOpValue(const MCInst &MI, const MCOperand &MO,
                          SmallVectorImpl<MCFixup> &Fixups,
                          const MCSubtargetInfo &STI) const;
};

} // end anonymous namespace

void AccaMCCodeEmitter::
encodeInstruction(const MCInst &MI, raw_ostream &OS,
                  SmallVectorImpl<MCFixup> &Fixups,
                  const MCSubtargetInfo &STI) const {
  uint64_t Binary = getBinaryCodeForInstr(MI, Fixups, STI);
  support::endian::write<uint32_t>(OS, Binary, support::little);
  ++MCNumEmitted; // Keep track of the # of mi's emitted.
};

unsigned AccaMCCodeEmitter::
getMachineOpValue(const MCInst &MI, const MCOperand &MO,
                  SmallVectorImpl<MCFixup> &Fixups,
                  const MCSubtargetInfo &STI) const {
  if (MO.isReg())
    return Ctx.getRegisterInfo()->getEncodingValue(MO.getReg());

  if (MO.isImm())
    return static_cast<unsigned>(MO.getImm());

  // MO must be an Expr.
  assert(MO.isExpr());
  return getExprOpValue(MI, MO, Fixups, STI);
};

unsigned
AccaMCCodeEmitter::getExprOpValue(const MCInst &MI, const MCOperand &MO,
                                  SmallVectorImpl<MCFixup> &Fixups,
                                  const MCSubtargetInfo &STI) const {
  assert(MO.isExpr() && "getExprOpValue expects only expressions");
  const MCExpr *Expr = MO.getExpr();
  MCExpr::ExprKind Kind = Expr->getKind();
  Acca::Fixups FixupKind = Acca::fixup_acca_invalid;

  if (Kind == MCExpr::Target) {
    const AccaMCExpr *AccaExpr = cast<AccaMCExpr>(Expr);

    switch (AccaExpr->getKind()) {
    case AccaMCExpr::VK_NONE:
    case AccaMCExpr::VK_INVALID:
    case AccaMCExpr::VK_ABS:
      llvm_unreachable("Unhandled fixup kind!");
    case AccaMCExpr::VK_CALL:
    case AccaMCExpr::VK_CALL_PLT:
      FixupKind = Acca::fixup_acca_rel22;
      break;
    case AccaMCExpr::VK_REL64_D0:
      FixupKind = Acca::fixup_acca_rel64_d0;
      break;
    case AccaMCExpr::VK_REL64_D1:
      FixupKind = Acca::fixup_acca_rel64_d1;
      break;
    case AccaMCExpr::VK_REL64_D2:
      FixupKind = Acca::fixup_acca_rel64_d2;
      break;
    case AccaMCExpr::VK_REL64_D3:
      FixupKind = Acca::fixup_acca_rel64_d3;
      break;
    case AccaMCExpr::VK_REL22_BYTE:
      FixupKind = Acca::fixup_acca_rel22_byte;
    }
  } else if (Kind == MCExpr::SymbolRef &&
             cast<MCSymbolRefExpr>(Expr)->getKind() ==
                 MCSymbolRefExpr::VK_None) {
    switch (MI.getOpcode()) {
    default:
      break;
    case Acca::CALL_immrel_cond:
    case Acca::CALL_immrel_nocond:
    case Acca::JMP_immrel_cond:
    case Acca::JMP_immrel_nocond:
      FixupKind = Acca::fixup_acca_rel22;
      break;
    case Acca::CJMP_byte_immrel:
    case Acca::CJMP_doublebyte_immrel:
    case Acca::CJMP_quadbyte_immrel:
    case Acca::CJMP_word_immrel:
      FixupKind = Acca::fixup_acca_rel13;
      break;
    }
  }

  assert(FixupKind != Acca::fixup_acca_invalid &&
         "Unhandled expression!");

  Fixups.push_back(
      MCFixup::create(0, Expr, MCFixupKind(FixupKind), MI.getLoc()));
  return 0;
};

#include "AccaGenMCCodeEmitter.inc"

MCCodeEmitter *llvm::createAccaMCCodeEmitter(const MCInstrInfo &MCII,
                                             MCContext &Ctx) {
  return new AccaMCCodeEmitter(MCII, Ctx);
}
