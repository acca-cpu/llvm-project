//===-------- AccaDisassembler.cpp - Disassembler for Acca ----------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file implements the AccaDisassembler class.
//
//===----------------------------------------------------------------------===//

#include "MCTargetDesc/AccaBaseInfo.h"
#include "MCTargetDesc/AccaMCTargetDesc.h"
#include "TargetInfo/AccaTargetInfo.h"
#include "llvm/MC/MCContext.h"
#include "llvm/MC/MCDecoderOps.h"
#include "llvm/MC/MCDisassembler/MCDisassembler.h"
#include "llvm/MC/MCInst.h"
#include "llvm/MC/MCInstrInfo.h"
#include "llvm/MC/MCRegisterInfo.h"
#include "llvm/MC/MCSubtargetInfo.h"
#include "llvm/MC/TargetRegistry.h"
#include "llvm/Support/Endian.h"

using namespace llvm;

#define DEBUG_TYPE "acca-disassembler"

typedef MCDisassembler::DecodeStatus DecodeStatus;

namespace {
class AccaDisassembler : public MCDisassembler {
public:
  AccaDisassembler(const MCSubtargetInfo &STI, MCContext &Ctx)
      : MCDisassembler(STI, Ctx) {}

  DecodeStatus getInstruction(MCInst &Instr, uint64_t &Size,
                              ArrayRef<uint8_t> Bytes, uint64_t Address,
                              raw_ostream &CStream) const override;
};
} // end namespace

static MCDisassembler *createAccaDisassembler(const Target &T,
                                                   const MCSubtargetInfo &STI,
                                                   MCContext &Ctx) {
  return new AccaDisassembler(STI, Ctx);
}

extern "C" LLVM_EXTERNAL_VISIBILITY void LLVMInitializeAccaDisassembler() {
  TargetRegistry::RegisterMCDisassembler(getTheAccaTarget(),
                                         createAccaDisassembler);
}

static DecodeStatus DecodeI64RegsRegisterClass(MCInst &Inst, uint64_t RegNo,
                                           uint64_t Address,
                                           const MCDisassembler *Decoder) {
  if (RegNo >= 16)
    return MCDisassembler::Fail;
  Inst.addOperand(MCOperand::createReg(Acca::R0 + RegNo));
  return MCDisassembler::Success;
}

static DecodeStatus DecodeI32RegsRegisterClass(MCInst &Inst, uint64_t RegNo,
                                           uint64_t Address,
                                           const MCDisassembler *Decoder) {
  if (RegNo >= 16)
    return MCDisassembler::Fail;
  Inst.addOperand(MCOperand::createReg(Acca::R0Q + RegNo));
  return MCDisassembler::Success;
}

static DecodeStatus DecodeI16RegsRegisterClass(MCInst &Inst, uint64_t RegNo,
                                           uint64_t Address,
                                           const MCDisassembler *Decoder) {
  if (RegNo >= 16)
    return MCDisassembler::Fail;
  Inst.addOperand(MCOperand::createReg(Acca::R0D + RegNo));
  return MCDisassembler::Success;
}

static DecodeStatus DecodeI8RegsRegisterClass(MCInst &Inst, uint64_t RegNo,
                                           uint64_t Address,
                                           const MCDisassembler *Decoder) {
  if (RegNo >= 16)
    return MCDisassembler::Fail;
  Inst.addOperand(MCOperand::createReg(Acca::R0B + RegNo));
  return MCDisassembler::Success;
}

template <unsigned N, int P = 0>
static DecodeStatus decodeUImmOperand(MCInst &Inst, uint64_t Imm,
                                      int64_t Address,
                                      const MCDisassembler *Decoder) {
  assert(isUInt<N>(Imm) && "Invalid immediate");
  Inst.addOperand(MCOperand::createImm(Imm + P));
  return MCDisassembler::Success;
}

template <unsigned N, unsigned S = 0>
static DecodeStatus decodeSImmOperand(MCInst &Inst, uint64_t Imm,
                                      int64_t Address,
                                      const MCDisassembler *Decoder) {
  assert(isUInt<N>(Imm) && "Invalid immediate");
  // Shift left Imm <S> bits, then sign-extend the number in the bottom <N+S>
  // bits.
  Inst.addOperand(MCOperand::createImm(SignExtend64<N + S>(Imm << S)));
  return MCDisassembler::Success;
}

#include "AccaGenDisassemblerTables.inc"

DecodeStatus AccaDisassembler::getInstruction(MCInst &MI, uint64_t &Size,
                                                   ArrayRef<uint8_t> Bytes,
                                                   uint64_t Address,
                                                   raw_ostream &CS) const {
  uint32_t Insn;
  DecodeStatus Result;

  // We want to read exactly 4 bytes of data because all Acca instructions
  // are fixed 32 bits.
  if (Bytes.size() < 4) {
    Size = 0;
    return MCDisassembler::Fail;
  }

  Insn = support::endian::read32le(Bytes.data());
  // Calling the auto-generated decoder function.
  Result = decodeInstruction(DecoderTableAcca32, MI, Insn, Address, this, STI);
  Size = 4;

  return Result;
}
