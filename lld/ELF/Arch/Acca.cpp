//===- Acca.cpp -----------------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "InputFiles.h"
#include "Symbols.h"
#include "Target.h"
#include "lld/Common/ErrorHandler.h"
#include "llvm/BinaryFormat/ELF.h"
#include "llvm/Support/Endian.h"
#include "llvm/Support/MathExtras.h"

using namespace llvm;
using namespace llvm::object;
using namespace llvm::support::endian;
using namespace llvm::ELF;
using namespace lld;
using namespace lld::elf;

namespace {
class Acca final : public TargetInfo {
public:
  RelExpr getRelExpr(RelType type, const Symbol &s,
                     const uint8_t *loc) const override;
  void relocate(uint8_t *loc, const Relocation &rel,
                uint64_t val) const override;
  bool inBranchRange(RelType type, uint64_t src,
                     uint64_t dst) const override;
};
} // namespace

RelExpr Acca::getRelExpr(RelType type, const Symbol &s,
                         const uint8_t *loc) const {
  switch (type) {
  case R_ACCA_REL22:
  case R_ACCA_REL13:
  case R_ACCA_REL64_D0:
  case R_ACCA_REL64_D1:
  case R_ACCA_REL64_D2:
  case R_ACCA_REL64_D3:
  case R_ACCA_REL22_BYTE:
    return R_PC;
  default:
    error(getErrorLocation(loc) + "unknown relocation (" + Twine(type) +
          ") against symbol " + toString(s));
    return R_NONE;
  }
};

void Acca::relocate(uint8_t *loc, const Relocation &rel, uint64_t val) const {
  uint32_t instr = read32le(loc);

  switch (rel.type) {
  case R_ACCA_REL22:
    checkAlignment(loc, val, 4, rel);
    val = static_cast<int64_t>(val - 4) >> 2;
    checkInt(loc, val, 22, rel);
    write32le(loc, (instr & ~0x3fffff) | (val & 0x3fffff));
    break;
  case R_ACCA_REL13:
    checkAlignment(loc, val, 4, rel);
    val = static_cast<int64_t>(val - 4) >> 2;
    checkInt(loc, val, 13, rel);
    write32le(loc, (instr & ~0x1fff) | (val & 0x1fff));
    break;
  case R_ACCA_REL64_D0:
  case R_ACCA_REL64_D1:
  case R_ACCA_REL64_D2:
  case R_ACCA_REL64_D3:
    checkAlignment(loc, val, 4, rel);
    val = (static_cast<int64_t>(val - 4) >> 2) >> ((rel.type - R_ACCA_REL64_D0) * 16);
    write32le(loc, (instr & ~0xffff) | (val & 0xffff));
    break;
  case R_ACCA_REL22_BYTE:
    val = static_cast<int64_t>(val - 4);
    checkInt(loc, val, 22, rel);
    write32le(loc, (instr & ~0x3fffff) | (val & 0x3fffff));
    break;
  default:
    llvm_unreachable("unknown relocation");
  }
};

bool Acca::inBranchRange(RelType type, uint64_t src, uint64_t dst) const {
  uint64_t relative = (dst - src) - 4;
  switch (type) {
  case R_ACCA_REL22:
    return isInt<22>(relative >> 2);
  case R_ACCA_REL13:
    return isInt<13>(relative >> 2);
  case R_ACCA_REL22_BYTE:
    return isInt<22>(relative);
  default:
    return false;
  }
}

TargetInfo *elf::getAccaTargetInfo() {
  static Acca target;
  return &target;
}
