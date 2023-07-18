// AccaAsmParser.cpp - Parse Acca assembly to MCInst instructions -----------=//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "MCTargetDesc/AccaInstPrinter.h"
#include "MCTargetDesc/AccaMCExpr.h"
#include "MCTargetDesc/AccaMCTargetDesc.h"
#include "TargetInfo/AccaTargetInfo.h"
#include "llvm/ADT/StringSwitch.h"
#include "llvm/MC/MCContext.h"
#include "llvm/MC/MCExpr.h"
#include "llvm/MC/MCInstrInfo.h"
#include "llvm/MC/MCParser/MCAsmLexer.h"
#include "llvm/MC/MCParser/MCAsmParser.h"
#include "llvm/MC/MCParser/MCTargetAsmParser.h"
#include "llvm/MC/MCRegister.h"
#include "llvm/MC/MCStreamer.h"
#include "llvm/MC/MCSubtargetInfo.h"
#include "llvm/MC/MCValue.h"
#include "llvm/MC/TargetRegistry.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/MathExtras.h"

using namespace llvm;

#define DEBUG_TYPE "acca-asm-parser"

namespace {
class AccaAsmParser : public MCTargetAsmParser {
  struct Inst {
    unsigned Opc;
    AccaMCExpr::VariantKind VK;
    Inst(unsigned Opc,
         AccaMCExpr::VariantKind VK = AccaMCExpr::VK_NONE):
      Opc(Opc), VK(VK) {}
  };
  using InstSeq = SmallVector<Inst>;

  bool parseRegister(MCRegister &RegNo, SMLoc &StartLoc,
                     SMLoc &EndLoc) override;
  OperandMatchResultTy tryParseRegister(MCRegister &RegNo, SMLoc &StartLoc,
                                        SMLoc &EndLoc) override;

  bool ParseInstruction(ParseInstructionInfo &Info, StringRef Name,
                        SMLoc NameLoc, OperandVector &Operands) override;

  bool ParseDirective(AsmToken DirectiveID) override {
    return true;
  }

  bool MatchAndEmitInstruction(SMLoc IDLoc, unsigned &Opcode,
                               OperandVector &Operands, MCStreamer &Out,
                               uint64_t &ErrorInfo,
                               bool MatchingInlineAsm) override;

  unsigned checkTargetMatchPredicate(MCInst &Inst) override;

  bool generateImmOutOfRangeError(OperandVector &Operands, uint64_t ErrorInfo,
                      int64_t Lower, int64_t Upper,
                      Twine Msg = "immediate must be an integer in the range");

  /// Helper for processing MC instructions that have been successfully matched
  /// by MatchAndEmitInstruction.
  bool processInstruction(MCInst &Inst, SMLoc IDLoc, OperandVector &Operands,
                          MCStreamer &Out);

// Auto-generated instruction matching functions.
#define GET_ASSEMBLER_HEADER
#include "AccaGenAsmMatcher.inc"

  SMLoc getLoc() const {
    return getParser().getTok().getLoc();
  }

  OperandMatchResultTy parseRegister(OperandVector &Operands);
  OperandMatchResultTy parseImmediate(OperandVector &Operands);
  OperandMatchResultTy parseOperandWithModifier(OperandVector &Operands);
  OperandMatchResultTy parseSImmBROperand(OperandVector &Operands);

  bool parseOperand(OperandVector &Operands, StringRef Mnemonic);

public:
  enum AccaMatchResultTy {
    Match_Dummy = FIRST_TARGET_MATCH_RESULT_TY,
#define GET_OPERAND_DIAGNOSTIC_TYPES
#include "AccaGenAsmMatcher.inc"
#undef GET_OPERAND_DIAGNOSTIC_TYPES
  };

  static bool classifySymbolRef(const MCExpr *Expr,
                                AccaMCExpr::VariantKind &Kind);

  AccaAsmParser(const MCSubtargetInfo &STI, MCAsmParser &Parser,
                const MCInstrInfo &MII, const MCTargetOptions &Options)
      : MCTargetAsmParser(Options, STI, MII) {
    // Initialize the set of available features.
    setAvailableFeatures(ComputeAvailableFeatures(STI.getFeatureBits()));
  }
};

// Instances of this class represent a parsed Acca machine instruction.
class AccaOperand : public MCParsedAsmOperand {
  enum class KindTy {
    Token,
    Register,
    Immediate,
  } Kind;

  struct RegOp {
    MCRegister RegNum;
  };

  struct ImmOp {
    const MCExpr *Val;
  };

  SMLoc StartLoc, EndLoc;
  union {
    StringRef Tok;
    struct RegOp Reg;
    struct ImmOp Imm;
  };

public:
  AccaOperand(KindTy K) : MCParsedAsmOperand(), Kind(K) {}

  bool isToken() const override {
    return Kind == KindTy::Token;
  }
  bool isReg() const override {
    return Kind == KindTy::Register;
  }
  bool isImm() const override {
    return Kind == KindTy::Immediate;
  }
  bool isMem() const override {
    return false;
  }
  void setReg(MCRegister PhysReg) {
    Reg.RegNum = PhysReg;
  }

  SMLoc getStartLoc() const override {
    return StartLoc;
  }
  SMLoc getEndLoc() const override {
    return EndLoc;
  }

  unsigned getReg() const override {
    assert(Kind == KindTy::Register && "Invalid type access!");
    return Reg.RegNum.id();
  }

  const MCExpr *getImm() const {
    assert(Kind == KindTy::Immediate && "Invalid type access!");
    return Imm.Val;
  }

  StringRef getToken() const {
    assert(Kind == KindTy::Token && "Invalid type access!");
    return Tok;
  }

  void print(raw_ostream &OS) const override {
    auto RegName = [](MCRegister Reg) {
      if (Reg)
        return AccaInstPrinter::getRegisterName(Reg);
      return "noreg";
    };

    switch (Kind) {
    case KindTy::Immediate:
      OS << *getImm();
      break;
    case KindTy::Register:
      OS << "<register " << RegName(getReg()) << ">";
      break;
    case KindTy::Token:
      OS << "'" << getToken() << "'";
      break;
    }
  }

  void addExpr(MCInst &Inst, const MCExpr *Expr) const {
    if (const auto *CE = dyn_cast<MCConstantExpr>(Expr))
      Inst.addOperand(MCOperand::createImm(CE->getValue()));
    else
      Inst.addOperand(MCOperand::createExpr(Expr));
  }

  // Used by the TableGen Code.
  void addRegOperands(MCInst &Inst, unsigned N) const {
    assert(N == 1 && "Invalid number of operands!");
    Inst.addOperand(MCOperand::createReg(getReg()));
  }
  void addImmOperands(MCInst &Inst, unsigned N) const {
    assert(N == 1 && "Invalid number of operands!");
    addExpr(Inst, getImm());
  }

  static bool evaluateConstantImm(const MCExpr *Expr, int64_t &Imm,
                                  AccaMCExpr::VariantKind &VK) {
    if (const auto *LE = dyn_cast<AccaMCExpr>(Expr)) {
      VK = LE->getKind();
      return false;
    }

    if (const auto *CE = dyn_cast<MCConstantExpr>(Expr)) {
      Imm = CE->getValue();
      return true;
    }

    return false;
  }

  template <unsigned N, unsigned S = 0, int P = 0> bool isUImm() const {
    if (!isImm())
      return false;

    int64_t Imm;
    AccaMCExpr::VariantKind VK = AccaMCExpr::VK_NONE;
    bool IsConstantImm = evaluateConstantImm(getImm(), Imm, VK);
    return IsConstantImm && isShiftedUInt<N, S>(Imm - P) &&
           VK == AccaMCExpr::VK_NONE;
  }

  template <unsigned N, unsigned S = 0, int P = 0> bool isSImm() const {
    if (!isImm())
      return false;

    int64_t Imm;
    AccaMCExpr::VariantKind VK = AccaMCExpr::VK_NONE;
    bool IsConstantImm = evaluateConstantImm(getImm(), Imm, VK);
    return IsConstantImm && isShiftedInt<N, S>(Imm - P) &&
           VK == AccaMCExpr::VK_NONE;
  }

  bool isCondCode() const {
    if (!isImm())
      return false;

    int64_t Imm;
    AccaMCExpr::VariantKind VK = AccaMCExpr::VK_NONE;
    bool IsConstantImm = evaluateConstantImm(getImm(), Imm, VK);
    return IsConstantImm && isShiftedUInt<4, 0>(Imm) &&
           VK == AccaMCExpr::VK_NONE && (Imm >= 0 && Imm <= 9);
  };

  bool isSImm13() const {
    return isSImm<13>();
  };

  bool isSImm22() const {
    return isSImm<22>();
  };

  bool isUImm10() const {
    return isUImm<10>();
  };

  bool isUImm11() const {
    return isUImm<11>();
  };

  bool isUImm16() const {
    return isUImm<16>();
  };

  bool isUImm22() const {
    return isUImm<22>();
  };

  bool isUImm2() const {
    return isUImm<2>();
  };

  bool isUImm3() const {
    return isUImm<3>();
  };

  bool isUImm6() const {
    return isUImm<6>();
  };

  bool isUImm7() const {
    return isUImm<7>();
  };

  bool isTImm64() const {
    return isSImm<64>();
  };

  bool isSImm13BROperand() const {
    if (!isImm())
      return false;

    int64_t Imm;
    AccaMCExpr::VariantKind VK = AccaMCExpr::VK_NONE;
    bool IsConstantImm = evaluateConstantImm(getImm(), Imm, VK);
    bool IsValidKind = VK == AccaMCExpr::VK_NONE ||
                       VK == AccaMCExpr::VK_CALL ||
                       VK == AccaMCExpr::VK_CALL_PLT;
    return IsConstantImm
               ? isShiftedInt<13, 2>(Imm) && IsValidKind
               : AccaAsmParser::classifySymbolRef(getImm(), VK) &&
                     IsValidKind;
  };

  bool isSImm22BROperand() const {
    if (!isImm())
      return false;

    int64_t Imm;
    AccaMCExpr::VariantKind VK = AccaMCExpr::VK_NONE;
    bool IsConstantImm = evaluateConstantImm(getImm(), Imm, VK);
    bool IsValidKind = VK == AccaMCExpr::VK_NONE ||
                       VK == AccaMCExpr::VK_CALL ||
                       VK == AccaMCExpr::VK_CALL_PLT;
    return IsConstantImm
               ? isShiftedInt<22, 2>(Imm) && IsValidKind
               : AccaAsmParser::classifySymbolRef(getImm(), VK) &&
                     IsValidKind;
  };

  static std::unique_ptr<AccaOperand> createToken(StringRef Str, SMLoc S) {
    auto Op = std::make_unique<AccaOperand>(KindTy::Token);
    Op->Tok = Str;
    Op->StartLoc = S;
    Op->EndLoc = S;
    return Op;
  }

  static std::unique_ptr<AccaOperand> createReg(unsigned RegNo, SMLoc S,
                                                SMLoc E) {
    auto Op = std::make_unique<AccaOperand>(KindTy::Register);
    Op->Reg.RegNum = RegNo;
    Op->StartLoc = S;
    Op->EndLoc = E;
    return Op;
  }

  static std::unique_ptr<AccaOperand> createImm(const MCExpr *Val, SMLoc S,
                                                SMLoc E) {
    auto Op = std::make_unique<AccaOperand>(KindTy::Immediate);
    Op->Imm.Val = Val;
    Op->StartLoc = S;
    Op->EndLoc = E;
    return Op;
  }
};
} // namespace

#define GET_REGISTER_MATCHER
#define GET_SUBTARGET_FEATURE_NAME
#define GET_MATCHER_IMPLEMENTATION
#define GET_MNEMONIC_SPELL_CHECKER
#include "AccaGenAsmMatcher.inc"

bool AccaAsmParser::
parseRegister(MCRegister &RegNo, SMLoc &StartLoc, SMLoc &EndLoc) {
  return Error(getLoc(), "invalid register number");
};

OperandMatchResultTy AccaAsmParser::
tryParseRegister(MCRegister &RegNo, SMLoc &StartLoc, SMLoc &EndLoc) {
  llvm_unreachable("unimplemented");
};

bool AccaAsmParser::
classifySymbolRef(const MCExpr *Expr, AccaMCExpr::VariantKind &Kind) {
  Kind = AccaMCExpr::VK_NONE;

  if (const AccaMCExpr *RE = dyn_cast<AccaMCExpr>(Expr)) {
    Kind = RE->getKind();
    Expr = RE->getSubExpr();
  }

  MCValue Res;
  if (Expr->evaluateAsRelocatable(Res, nullptr, nullptr))
    return Res.getRefKind() == AccaMCExpr::VK_NONE;
  return false;
}

OperandMatchResultTy AccaAsmParser::
parseRegister(OperandVector &Operands) {
  if (getLexer().getKind() != AsmToken::Identifier)
    return MatchOperand_NoMatch;

  StringRef Name = getLexer().getTok().getIdentifier();
  MCRegister RegNo = MatchRegisterName(Name);
  if (RegNo == Acca::NoRegister)
    RegNo = MatchRegisterAltName(Name);
  if (RegNo == Acca::NoRegister)
    return MatchOperand_NoMatch;

  SMLoc S = getLoc();
  SMLoc E = SMLoc::getFromPointer(S.getPointer() + Name.size());
  getLexer().Lex();
  Operands.push_back(AccaOperand::createReg(RegNo, S, E));

  return MatchOperand_Success;
}

OperandMatchResultTy AccaAsmParser::
parseImmediate(OperandVector &Operands) {
  SMLoc S = getLoc();
  SMLoc E;
  const MCExpr *Res;

  switch (getLexer().getKind()) {
  default:
    return MatchOperand_NoMatch;
  case AsmToken::LParen:
  case AsmToken::Dot:
  case AsmToken::Minus:
  case AsmToken::Plus:
  case AsmToken::Exclaim:
  case AsmToken::Tilde:
  case AsmToken::Integer:
  case AsmToken::String:
  case AsmToken::Identifier:
    if (getParser().parseExpression(Res, E))
      return MatchOperand_ParseFail;
    break;
  case AsmToken::Percent:
    return parseOperandWithModifier(Operands);
  }

  Operands.push_back(AccaOperand::createImm(Res, S, E));
  return MatchOperand_Success;
}

OperandMatchResultTy AccaAsmParser::
parseOperandWithModifier(OperandVector &Operands) {
  SMLoc S = getLoc();
  SMLoc E;

  if (getLexer().getKind() != AsmToken::Percent) {
    Error(getLoc(), "expected '%' for operand modifier");
    return MatchOperand_ParseFail;
  }

  getParser().Lex(); // Eat '%'

  if (getLexer().getKind() != AsmToken::Identifier) {
    Error(getLoc(), "expected valid identifier for operand modifier");
    return MatchOperand_ParseFail;
  }
  StringRef Identifier = getParser().getTok().getIdentifier();
  AccaMCExpr::VariantKind VK =
      AccaMCExpr::getVariantKindForName(Identifier);
  if (VK == AccaMCExpr::VK_INVALID) {
    Error(getLoc(), "unrecognized operand modifier");
    return MatchOperand_ParseFail;
  }

  getParser().Lex(); // Eat the identifier
  if (getLexer().getKind() != AsmToken::LParen) {
    Error(getLoc(), "expected '('");
    return MatchOperand_ParseFail;
  }
  getParser().Lex(); // Eat '('

  const MCExpr *SubExpr;
  if (getParser().parseParenExpression(SubExpr, E)) {
    return MatchOperand_ParseFail;
  }

  const MCExpr *ModExpr = AccaMCExpr::create(SubExpr, VK, getContext());
  Operands.push_back(AccaOperand::createImm(ModExpr, S, E));
  return MatchOperand_Success;
}

OperandMatchResultTy AccaAsmParser::
parseSImmBROperand(OperandVector &Operands) {
  SMLoc S = getLoc();
  const MCExpr *Res;

  if (getLexer().getKind() == AsmToken::Percent)
    return parseOperandWithModifier(Operands);

  if (getLexer().getKind() != AsmToken::Identifier)
    return MatchOperand_NoMatch;

  StringRef Identifier;
  if (getParser().parseIdentifier(Identifier))
    return MatchOperand_ParseFail;

  SMLoc E = SMLoc::getFromPointer(S.getPointer() + Identifier.size());

  MCSymbol *Sym = getContext().getOrCreateSymbol(Identifier);
  Res = MCSymbolRefExpr::create(Sym, MCSymbolRefExpr::VK_None, getContext());
  Res = AccaMCExpr::create(Res, AccaMCExpr::VK_CALL, getContext());
  Operands.push_back(AccaOperand::createImm(Res, S, E));
  return MatchOperand_Success;
}

bool AccaAsmParser::
parseOperand(OperandVector &Operands, StringRef Mnemonic) {
  // Check if the current operand has a custom associated parser, if so, try to
  // custom parse the operand, or fallback to the general approach.
  OperandMatchResultTy Result =
      MatchOperandParserImpl(Operands, Mnemonic, /*ParseForAllFeatures=*/true);
  if (Result == MatchOperand_Success)
    return false;
  if (Result == MatchOperand_ParseFail)
    return true;

  if (parseRegister(Operands) == MatchOperand_Success ||
      parseImmediate(Operands) == MatchOperand_Success)
    return false;

  // Finally we have exhausted all options and must declare defeat.
  Error(getLoc(), "unknown operand");
  return true;
};

unsigned AccaAsmParser::checkTargetMatchPredicate(MCInst &Inst) {
  unsigned Opc = Inst.getOpcode();
  switch (Opc) {
  default:
    break;
  }

  return Match_Success;
}

bool AccaAsmParser::
ParseInstruction(ParseInstructionInfo &Info, StringRef Name, SMLoc NameLoc,
                 OperandVector &Operands) {
  size_t Start = 0, Next = Name.find('.');
  StringRef Head = Name.slice(Start, Next);

  bool AcceptsCondCode = StringSwitch<bool>(Head)
    .Cases("soc", "sof", "jmpa", "jmpr", "cjmpa", "cjmpr", "calla", "callr",
      true)
    .Default(false);
  bool RequiresCondCode = StringSwitch<bool>(Head)
    .Cases("soc", "sof", "cjmpa", "cjmpr", true)
    .Default(false);
  bool AcceptsSize = StringSwitch<bool>(Head)
    .Cases("pushs", "pushp", "pops", "popp", "lds", "ldp", "sts", "stp", true)
    .Cases("copy", "add", "sub", "div", "and", "or", "xor", "shl", "shr", true)
    .Cases("rot", "neg", "bswap", "soc", "sof", "cjmpa", "cjmpr", true)
    .Default(false);

  // First operand in MCInst is instruction mnemonic.
  Operands.push_back(AccaOperand::createToken(Head, NameLoc));

  // handle condition codes
  if (RequiresCondCode && Next == StringRef::npos) {
    std::string Msg = "missing condition code";
    return Error(NameLoc, Msg);
  }
  if (AcceptsCondCode && Next != StringRef::npos) {
    Start = Next;
    Next = Name.find('.', Start + 1);
    Head = Name.slice(Start + 1, Next);

    SMLoc SuffixLoc = SMLoc::getFromPointer(NameLoc.getPointer() +
                                            (Head.data() - Name.data()));
    uint8_t CondCode = 15;
    if (CondCode == 15) {
      std::string Msg = "invalid condition code";
      return Error(SuffixLoc, Msg);
    }
    Operands.push_back(AccaOperand::createToken(".", SuffixLoc));
    Operands.push_back(AccaOperand::createImm(MCConstantExpr::create(CondCode, getContext()), NameLoc, NameLoc));
  }

  // If there are no more operands, then finish.
  if (parseOptionalToken(AsmToken::EndOfStatement))
    return false;

  // Parse first operand.
  if (parseOperand(Operands, Name))
    return true;

  // Parse until end of statement, consuming commas between operands.
  while (parseOptionalToken(AsmToken::Comma))
    if (parseOperand(Operands, Name))
      return true;

  // Parse end of statement and return successfully.
  if (parseOptionalToken(AsmToken::EndOfStatement))
    return false;

  SMLoc Loc = getLexer().getLoc();
  getParser().eatToEndOfStatement();
  return Error(Loc, "unexpected token");
};

bool AccaAsmParser::
generateImmOutOfRangeError(OperandVector &Operands, uint64_t ErrorInfo,
                           int64_t Lower, int64_t Upper, Twine Msg) {
  SMLoc ErrorLoc = ((AccaOperand &)*Operands[ErrorInfo]).getStartLoc();
  return Error(ErrorLoc, Msg + " [" + Twine(Lower) + ", " + Twine(Upper) + "]");
};

bool AccaAsmParser::
processInstruction(MCInst &Inst, SMLoc IDLoc, OperandVector &Operands,
                   MCStreamer &Out) {
  Inst.setLoc(IDLoc);
  switch (Inst.getOpcode()) {
  default:
    break;
  }
  Out.emitInstruction(Inst, getSTI());
  return false;
};

bool AccaAsmParser::
MatchAndEmitInstruction(SMLoc IDLoc, unsigned &Opcode,
                        OperandVector &Operands,
                        MCStreamer &Out,
                        uint64_t &ErrorInfo,
                        bool MatchingInlineAsm) {
  MCInst Inst;
  FeatureBitset MissingFeatures;

  auto Result = MatchInstructionImpl(Operands, Inst, ErrorInfo, MissingFeatures,
                                     MatchingInlineAsm);
  switch (Result) {
  default:
    break;
  case Match_Success:
    return processInstruction(Inst, IDLoc, Operands, Out);
  case Match_MissingFeature: {
    assert(MissingFeatures.any() && "Unknown missing features!");
    bool FirstFeature = true;
    std::string Msg = "instruction requires the following:";
    for (unsigned I = 0, E = MissingFeatures.size(); I != E; ++I) {
      if (MissingFeatures[I]) {
        Msg += FirstFeature ? " " : ", ";
        Msg += getSubtargetFeatureName(I);
        FirstFeature = false;
      }
    }
    return Error(IDLoc, Msg);
  }
  case Match_MnemonicFail: {
    FeatureBitset FBS = ComputeAvailableFeatures(getSTI().getFeatureBits());
    std::string Suggestion = AccaMnemonicSpellCheck(
        ((AccaOperand &)*Operands[0]).getToken(), FBS, 0);
    return Error(IDLoc, "unrecognized instruction mnemonic" + Suggestion);
  }
  case Match_InvalidOperand: {
    SMLoc ErrorLoc = IDLoc;
    if (ErrorInfo != ~0ULL) {
      if (ErrorInfo >= Operands.size())
        return Error(ErrorLoc, "too few operands for instruction");

      ErrorLoc = ((AccaOperand &)*Operands[ErrorInfo]).getStartLoc();
      if (ErrorLoc == SMLoc())
        ErrorLoc = IDLoc;
    }
    return Error(ErrorLoc, "invalid operand for instruction");
  }
  }

  // Handle the case when the error message is of specific type
  // other than the generic Match_InvalidOperand, and the
  // corresponding operand is missing.
  if (Result > FIRST_TARGET_MATCH_RESULT_TY) {
    SMLoc ErrorLoc = IDLoc;
    if (ErrorInfo != ~0ULL && ErrorInfo >= Operands.size())
      return Error(ErrorLoc, "too few operands for instruction");
  }

  switch (Result) {
  default:
    break;
  case Match_InvalidSImm13:
    return generateImmOutOfRangeError(Operands, ErrorInfo, /*Lower=*/-(1 << 12),
                                      /*Upper=*/(1 << 12) - 1);
  case Match_InvalidSImm22:
    return generateImmOutOfRangeError(Operands, ErrorInfo, /*Lower=*/-(1 << 21),
                                      /*Upper=*/(1 << 21) - 1);
  case Match_InvalidUImm10:
    return generateImmOutOfRangeError(Operands, ErrorInfo, /*Lower=*/0,
                                      /*Upper=*/(1 << 10) - 1);
  case Match_InvalidUImm11:
    return generateImmOutOfRangeError(Operands, ErrorInfo, /*Lower=*/0,
                                      /*Upper=*/(1 << 11) - 1);
  case Match_InvalidUImm16:
    return generateImmOutOfRangeError(Operands, ErrorInfo, /*Lower=*/0,
                                      /*Upper=*/(1 << 16) - 1);
  case Match_InvalidUImm2:
    return generateImmOutOfRangeError(Operands, ErrorInfo, /*Lower=*/0,
                                      /*Upper=*/(1 << 2) - 1);
  case Match_InvalidUImm22:
    return generateImmOutOfRangeError(Operands, ErrorInfo, /*Lower=*/0,
                                      /*Upper=*/(1 << 22) - 1);
  case Match_InvalidUImm3:
    return generateImmOutOfRangeError(Operands, ErrorInfo, /*Lower=*/0,
                                      /*Upper=*/(1 << 3) - 1);
  case Match_InvalidUImm6:
    return generateImmOutOfRangeError(Operands, ErrorInfo, /*Lower=*/0,
                                      /*Upper=*/(1 << 6) - 1);
  case Match_InvalidUImm7:
    return generateImmOutOfRangeError(Operands, ErrorInfo, /*Lower=*/0,
                                      /*Upper=*/(1 << 7) - 1);
  case Match_InvalidSImm13BR:
    return generateImmOutOfRangeError(
        Operands, ErrorInfo, /*Lower=*/-(1 << 14), /*Upper=*/(1 << 14) - 4,
        "operand must be a symbol with modifier (e.g. %plt), a bare symbol"
        "name, or an immediate and must be a multiple of 4 in the range");
  case Match_InvalidSImm22BR:
    return generateImmOutOfRangeError(
        Operands, ErrorInfo, /*Lower=*/-(1 << 23), /*Upper=*/(1 << 23) - 4,
        "operand must be a symbol with modifier (e.g. %plt), a bare symbol"
        "name, or an immediate and must be a multiple of 4 in the range");
  case Match_InvalidTImm64:
    llvm_unreachable("this should be impossible");
  }
  llvm_unreachable("Unknown match type detected!");
}

extern "C" LLVM_EXTERNAL_VISIBILITY void LLVMInitializeAccaAsmParser() {
  RegisterMCAsmParser<AccaAsmParser> Parser(getTheAccaTarget());
}
