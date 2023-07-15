//===-- AccaInstrInfo.td - Target Description for Acca Target -----------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file describes the Acca instructions in TableGen format.
//
//===----------------------------------------------------------------------===//

//===----------------------------------------------------------------------===//
// Instruction format superclass
//===----------------------------------------------------------------------===//

include "AccaInstrFormats.td"

// the same registers are used for i8, i16, i32, and i64, so we get truncate
// and extend for free
foreach bigsize = 0-3 in {
  defvar smallsizes = !filter(i, [0, 1, 2, 3], !lt(i, bigsize));
  foreach smallsize = smallsizes in {
    defvar bigvalsize = !cond(!eq(bigsize, 0) : i8, !eq(bigsize, 1) : i16, !eq(bigsize, 2) : i32, true : i64);
    defvar smallvalsize = !cond(!eq(smallsize, 0) : i8, !eq(smallsize, 1) : i16, !eq(smallsize, 2) : i32, true : i64);
    defvar bigclass = !cond(!eq(bigsize, 0) : I8Regs, !eq(bigsize, 1) : I16Regs, !eq(bigsize, 2) : I32Regs, true : I64Regs);
    defvar smallclass = !cond(!eq(smallsize, 0) : I8Regs, !eq(smallsize, 1) : I16Regs, !eq(smallsize, 2) : I32Regs, true : I64Regs);
    defvar opsize = !cond(!eq(smallsize, 0) : "byte", !eq(smallsize, 1) : "doublebyte", !eq(smallsize, 2) : "quadbyte", true : "word");
    defvar subreg_idx = !cast<SubRegIndex>(!strconcat("subreg_", opsize));
    def : Pat<(bigvalsize (anyext smallclass:$val)), (INSERT_SUBREG (bigvalsize (IMPLICIT_DEF)), smallclass:$val, subreg_idx)>;
    def : Pat<(smallvalsize (trunc bigclass:$val)), (EXTRACT_SUBREG bigclass:$val, subreg_idx)>;
  }
}

def SDT_AccaCall : SDTypeProfile<0, -1, [SDTCisVT<0, i64>]>;
def SDT_CallSeqStart : SDCallSeqStart<[SDTCisVT<0, i32>,
                                       SDTCisVT<1, i32>]>;
def SDT_CallSeqEnd : SDCallSeqEnd<[SDTCisVT<0, i32>,
                                   SDTCisVT<1, i32>]>;

def callseq_start : SDNode<"ISD::CALLSEQ_START", SDT_CallSeqStart,
                           [SDNPHasChain, SDNPOutGlue]>;
def callseq_end : SDNode<"ISD::CALLSEQ_END", SDT_CallSeqEnd,
                         [SDNPHasChain, SDNPOptInGlue, SDNPOutGlue]>;

def acca_call : SDNode<"AccaISD::CALL", SDT_AccaCall,
                            [SDNPHasChain, SDNPOptInGlue, SDNPOutGlue,
                             SDNPVariadic]>;
def acca_tail : SDNode<"AccaISD::TAIL", SDT_AccaCall,
                            [SDNPHasChain, SDNPOptInGlue, SDNPOutGlue,
                             SDNPVariadic]>;
def RETFLAG : SDNode<"AccaISD::RET_FLAG", SDTNone,
                     [SDNPHasChain, SDNPOptInGlue, SDNPVariadic]>;

//
// push/pop
//

defm PUSHS : PushPop1<0b110111, "pushs", 0>;
defm PUSHP : PushPop2<0b110110, "pushp", 0>;
defm POPS  : PushPop1<0b110101, "pops",  1>;
defm POPP  : PushPop2<0b110100, "popp",  1>;

//
// load/store
//

defm LDS : LoadStore1<0b110011, "lds", 0>;
defm LDP : LoadStore2<0b110010, "ldp", 0>;
defm STS : LoadStore1<0b110001, "sts", 1>;
defm STP : LoadStore2<0b110000, "stp", 1>;

// single load patterns

def : Pat<(i8 (load I64Regs:$Rb)), (LDS_byte I64Regs:$Rb)>;
def : Pat<(i16 (load I64Regs:$Rb)), (LDS_doublebyte I64Regs:$Rb)>;
def : Pat<(i32 (load I64Regs:$Rb)), (LDS_quadbyte I64Regs:$Rb)>;
def : Pat<(i64 (load I64Regs:$Rb)), (LDS_word I64Regs:$Rb)>;

// single load anyext patterns

def : Pat<(i16 (extloadi8 I64Regs:$Rb)), (INSERT_SUBREG (i16 (IMPLICIT_DEF)), (LDS_byte I64Regs:$Rb), subreg_byte)>;
def : Pat<(i32 (extloadi8 I64Regs:$Rb)), (INSERT_SUBREG (i32 (IMPLICIT_DEF)), (LDS_byte I64Regs:$Rb), subreg_byte)>;
def : Pat<(i64 (extloadi8 I64Regs:$Rb)), (INSERT_SUBREG (i64 (IMPLICIT_DEF)), (LDS_byte I64Regs:$Rb), subreg_byte)>;
def : Pat<(i32 (extloadi16 I64Regs:$Rb)), (INSERT_SUBREG (i32 (IMPLICIT_DEF)), (LDS_doublebyte I64Regs:$Rb), subreg_doublebyte)>;
def : Pat<(i64 (extloadi16 I64Regs:$Rb)), (INSERT_SUBREG (i64 (IMPLICIT_DEF)), (LDS_doublebyte I64Regs:$Rb), subreg_doublebyte)>;
def : Pat<(i64 (extloadi32 I64Regs:$Rb)), (INSERT_SUBREG (i64 (IMPLICIT_DEF)), (LDS_quadbyte I64Regs:$Rb), subreg_quadbyte)>;

//
// load immediate
//

def LDI : AccaInst<
  (outs I64Regs:$Rd),
  (ins uimm16:$val, uimm6:$shift, uimm2:$clear),
  "ldi $Rd, $val, $shift, $clear",
  []
> {
  bits<4> Rd;
  bits<16> val;
  bits<6> shift;
  bits<2> clear;

  let isMoveImm = 1;

  let opcode{5-2} = 0b1110;
  let opcode{1-0} = clear;
  let Inst{3-0} = Rd;
  let Inst{9-4} = shift;
  let Inst{25-10} = val;
}

def LDI_noshift_clearall : AccaInst<
  (outs I64Regs:$Rd),
  (ins uimm16:$val),
  "ldi $Rd, $val, 0, 3",
  [(set I64Regs:$Rd, uimm16:$val)]
> {
  bits<4> Rd;
  bits<16> val;

  let isMoveImm = 1;

  let opcode{5-2} = 0b1110;
  let opcode{1-0} = 0b11;
  let Inst{3-0} = Rd;
  let Inst{9-4} = 0b000000;
  let Inst{25-10} = val;
}

def PseudoLDI_byte       : Pseudo<(outs I8Regs:$Rd),  (ins timm64:$val), []>;
def PseudoLDI_doublebyte : Pseudo<(outs I16Regs:$Rd), (ins timm64:$val), []>;
def PseudoLDI_quadbyte   : Pseudo<(outs I32Regs:$Rd), (ins timm64:$val), []>;
def PseudoLDI_word       : Pseudo<(outs I64Regs:$Rd), (ins timm64:$val), []>;

//
// load relative
//

def LDR : AccaInst<
  (outs I64Regs:$Rd),
  (ins simm22:$val),
  "ldr $Rd, $val",
  []
> {
  bits<4> Rd;
  bits<22> val;

  let isMoveImm = 1;

  let opcode = 0b001100;
  let Inst{21-0} = val;
  let Inst{25-22} = Rd;
}

//
// copy
//

def COPY_byte       : Copy<0b00, I8Regs,  i8,  "b">;
def COPY_doublebyte : Copy<0b01, I16Regs, i16, "d">;
def COPY_quadbyte   : Copy<0b10, I32Regs, i32, "q">;
def COPY_word       : Copy<0b11, I64Regs, i64, "w">;

// `copy` can be used with `ldi`

//
// add/sub
//

defm ADD : AddSub<0b101001, 0b1011, "add", add>;
defm SUB : AddSub<0b101000, 0b1001, "sub", sub>;

//
// mul
//

foreach srcsize = 0-3 in {
  foreach dstsize = 0-3 in {
    foreach issigned = 0-1 in {
      foreach setflags = 0-1 in {
        defvar srcvalsize = !cond(!eq(srcsize, 0) : i8, !eq(srcsize, 1) : i16, !eq(srcsize, 2) : i32, true : i64);
        defvar srcvalclass = !cond(!eq(srcsize, 0) : I8Regs, !eq(srcsize, 1) : I16Regs, !eq(srcsize, 2) : I32Regs, true : I64Regs);
        defvar dstvalclass = !cond(!eq(dstsize, 0) : I8Regs, !eq(dstsize, 1) : I16Regs, !eq(dstsize, 2) : I32Regs, true : I64Regs);
        defvar issignedstr = !if(issigned, "signed", "unsigned");
        defvar setflagsstr = !if(setflags, "setflags", "nosetflags");
        def MUL_#srcsize#_#dstsize#_#issignedstr#_#setflagsstr : Mul<
          srcsize,
          dstsize,
          srcvalclass,
          dstvalclass,
          !if(!and(!and(!eq(srcsize, dstsize), issigned), !not(setflags)),
            [(set srcvalsize:$Rd, (mul srcvalsize:$lhs, srcvalsize:$rhs))],
            []
          ),
          setflags,
          issigned
        >;
      }
    }
  }
}

//
// div
//

defm DIV_byte       : Div<0b00, I8Regs,  i8,  "b">;
defm DIV_doublebyte : Div<0b01, I16Regs, i16, "d">;
defm DIV_quadbyte   : Div<0b10, I32Regs, i32, "q">;
defm DIV_word       : Div<0b11, I64Regs, i64, "w">;

//
// and/or/xor
//

defm AND : AndOrXor<0b101011, 0b10000, "and", and>;
defm OR  : AndOrXor<0b010000, 0b01111, "or",  or>;
defm XOR : AndOrXor<0b001110, 0b01110, "xor", xor>;

foreach setflags = 0-1 in {
  defvar setflagsstr = !if(setflags, "setflags", "nosetflags");

  foreach size = 0-3 in {
    defvar regclass = !cond(!eq(size, 0) : I8Regs, !eq(size, 1) : I16Regs, !eq(size, 2) : I32Regs, true : I64Regs);
    defvar sizechar = !cond(!eq(size, 0) : "b", !eq(size, 1) : "d", !eq(size, 2) : "q", true : "w");
    defvar opsize = !cond(!eq(size, 0) : "byte", !eq(size, 1) : "doublebyte", !eq(size, 2) : "quadbyte", true : "word");
    defvar xor_op = !cast<Instruction>(!strconcat("XOR_", opsize, "_nonnull_imm_arith_", setflagsstr));
    defvar xor_val = !if(!eq(size, 0), 0xff, 0x7ff);
    def NOT_#opsize#_#setflagsstr :
      Pseudo<(outs regclass:$Rd), (ins regclass:$Ra),
             [(set regclass:$Rd, (not regclass:$Ra))],
             !strconcat("not{.", sizechar, "} $Rd, $Ra, ",
               !if(setflags, "1", "0"))>,
      PseudoInstExpansion<(xor_op regclass:$Rd, regclass:$Ra, xor_val, 0)>;
  }
}

//
// shl/shr/rot
//

defm SHL : ShlShrRot<0b011010, 0b011011, "shl", 0, shl>;
defm SHR : ShlShrRot<0b011000, 0b011001, "shr", 1, srl>; // placeholder opnode
defm ROT : ShlShrRot<0b010110, 0b010111, "rot", 0, rotr>;

// patterns for sext and zext, implemented with shl/shr
foreach bigsize = 0-3 in {
  defvar smallsizes = !filter(i, [0, 1, 2, 3], !lt(i, bigsize));
  foreach smallsize = smallsizes in {
    defvar bigvalsize = !cond(!eq(bigsize, 0) : i8, !eq(bigsize, 1) : i16, !eq(bigsize, 2) : i32, true : i64);
    defvar smallvalsize = !cond(!eq(smallsize, 0) : i8, !eq(smallsize, 1) : i16, !eq(smallsize, 2) : i32, true : i64);
    defvar bigclass = !cond(!eq(bigsize, 0) : I8Regs, !eq(bigsize, 1) : I16Regs, !eq(bigsize, 2) : I32Regs, true : I64Regs);
    defvar smallclass = !cond(!eq(smallsize, 0) : I8Regs, !eq(smallsize, 1) : I16Regs, !eq(smallsize, 2) : I32Regs, true : I64Regs);
    defvar smallbits = !cond(!eq(smallsize, 0) : 8, !eq(smallsize, 1) : 16, !eq(smallsize, 2) : 32, true : 64);
    defvar bigopsize = !cond(!eq(bigsize, 0) : "byte", !eq(bigsize, 1) : "doublebyte", !eq(bigsize, 2) : "quadbyte", true : "word");
    defvar smallopsize = !cond(!eq(smallsize, 0) : "byte", !eq(smallsize, 1) : "doublebyte", !eq(smallsize, 2) : "quadbyte", true : "word");
    defvar subreg_idx = !cast<SubRegIndex>(!strconcat("subreg_", smallopsize));
    defvar shrop_unsigned = !cast<Instruction>(!strconcat("SHR_", bigopsize, "_nonnull_imm_noarith_nosetflags"));
    defvar shrop_signed = !cast<Instruction>(!strconcat("SHR_", bigopsize, "_nonnull_imm_arith_nosetflags"));
    defvar shlop = !cast<Instruction>(!strconcat("SHL_", bigopsize, "_nonnull_imm_noarith_nosetflags"));
    def : Pat<(bigvalsize (sext smallclass:$val)),
              (shrop_signed
                (shlop
                  (INSERT_SUBREG
                    (bigvalsize (IMPLICIT_DEF)),
                    smallclass:$val,
                    subreg_idx
                  ),
                  smallbits),
                smallbits)>;
    def : Pat<(bigvalsize (zext smallclass:$val)),
              (shrop_unsigned
                (shlop
                  (INSERT_SUBREG
                    (bigvalsize (IMPLICIT_DEF)),
                    smallclass:$val,
                    subreg_idx
                  ),
                  smallbits),
                smallbits)>;
  }
}

//
// neg/bswap
//

defm NEG   : NegBswap<0b010101, "neg",   ineg>;
defm BSWAP : NegBswap<0b010100, "bswap", bswap>;

//
// soc
//

foreach size = 0-3 in {
  foreach borrow = 0-1 in {
    defvar borrowstr = !if(borrow, "borrow", "noborrow");
    defvar opsize = !cond(!eq(size, 0) : "byte", !eq(size, 1) : "doublebyte", !eq(size, 2) : "quadbyte", true : "word");
    defvar sizechar = !cond(!eq(size, 0) : "b", !eq(size, 1) : "d", !eq(size, 2) : "q", true : "w");
    defvar regclass = !cond(!eq(size, 0) : I8Regs, !eq(size, 1) : I16Regs, !eq(size, 2) : I32Regs, true : I64Regs);

    def SOC_#opsize#_#borrowstr : AccaInst<
      (outs regclass:$Rd),
      (ins regclass:$lhs, regclass:$rhs, ccode:$cond),
      !strconcat("soc.$cond{.", sizechar, "} $Rd, $lhs, $rhs, ", !if(borrow, "1", "0")),
      []
    > {
      bits<4> Rd;
      bits<4> lhs;
      bits<4> rhs;
      bits<4> cond;

      let mayLoad = 0;
      let mayStore = 0;
      let hasSideEffects = 0;

      let Uses = !if(borrow, [CZOS], []);
      let isCompare = 1;

      let opcode = 0b111100;
      let Inst{3-0} = rhs;
      let Inst{7-4} = lhs;
      let Inst{11-8} = Rd;
      let Inst{13-12} = size;
      let Inst{14} = borrow;
      let Inst{18-15} = cond;
    }
  }
}

//
// sof
//

foreach size = 0-3 in {
  defvar opsize = !cond(!eq(size, 0) : "byte", !eq(size, 1) : "doublebyte", !eq(size, 2) : "quadbyte", true : "word");
  defvar sizechar = !cond(!eq(size, 0) : "b", !eq(size, 1) : "d", !eq(size, 2) : "q", true : "w");
  defvar regclass = !cond(!eq(size, 0) : I8Regs, !eq(size, 1) : I16Regs, !eq(size, 2) : I32Regs, true : I64Regs);

  def SOF_#opsize : AccaInst<
    (outs regclass:$Rd),
    (ins ccode:$cond),
    !strconcat("soc.$cond{.", sizechar, "} $Rd"),
    []
  > {
    bits<4> Rd;
    bits<4> cond;

    let mayLoad = 0;
    let mayStore = 0;
    let hasSideEffects = 0;

    let opcode = 0b111101;
    let Inst{3-0} = Rd;
    let Inst{5-4} = size;
    let Inst{9-6} = cond;
  }
}

//
// jmpa/jmpr
//

let isTerminator = 1 in
  defm JMP : JmpCall<0b010011, 0b010010, 0b010001, "jmp", 0>;

def : Pat<(br bb:$addr), (JMP_immrel_nocond bb:$addr)>;

//
// cjmpa/cjmpr
//

defm CJMP_byte       : Cjmp<0b00, I8Regs /*, "b"*/>;
defm CJMP_doublebyte : Cjmp<0b01, I16Regs/*, "d"*/>;
defm CJMP_quadbyte   : Cjmp<0b10, I32Regs/*, "q"*/>;
defm CJMP_word       : Cjmp<0b11, I64Regs/*, "w"*/>;

//
// calla/callr
//

let Defs = [R15], isCall = 1 in
  defm CALL : JmpCall<0b001010, 0b001001, 0b001000, "call", 1>;

//
// ret/eret/udf/dbg
//

let isTerminator = 1, isBarrier = 1 in {
  let isReturn = 1, mayLoad = 0, mayStore = 0 in {
    let Uses = [R15] in def RET : NoOperands<0b000111, "ret", [(RETFLAG)]>;
    let hasSideEffects = 1 in def ERET : NoOperands<0b000110, "eret", []>;
  }
  let hasSideEffects = 1, isTrap = 1, mayLoad = 0, mayStore = 0 in {
    def UDF : NoOperands<0b000000, "udf", []>;
    def DBG : NoOperands<0b000010, "dbg", []>;
  }
}

//
// exc
//

def EXC : AccaInst<
  (outs),
  (ins uimm16:$val),
  "exc $val",
  []
> {
  bits<16> val;

  let isBarrier = 1;
  let hasSideEffects = 1;
  let isTrap = 1;

  let opcode = 0b000011;
  let Inst{15-0} = val;
}

//
// nop
//

let mayLoad = 0, mayStore = 0, hasSideEffects = 0 in
  def NOP : NoOperands<0b000001, "nop",  []>;

//
// ldm/stm
//

def LDM : AccaInst<
  (outs I64Regs:$Rd),
  (ins uimm22:$mreg),
  "ldm $Rd, $mreg",
  []
> {
  bits<4> Rd;
  bits<22> mreg;

  let hasSideEffects = 1;

  let opcode = 0b000100;
  let Inst{21-0} = mreg;
  let Inst{25-22} = Rd;
}

def STM : AccaInst<
  (outs),
  (ins I64Regs:$Rd, uimm22:$mreg),
  "stm $mreg, $Rd",
  []
> {
  bits<4> Rd;
  bits<22> mreg;

  let hasSideEffects = 1;

  let opcode = 0b000101;
  let Inst{21-0} = mreg;
  let Inst{25-22} = Rd;
}

//
// codegen-only pseudo-instructions for CALL and TAIL
//

let isCall = 1, Defs = [R15] in
  def PseudoCALL : Pseudo<(outs), (ins simm22_symbol:$addr)>;

def : Pat<(acca_call tglobaladdr:$addr), (PseudoCALL tglobaladdr:$addr)>;
def : Pat<(acca_call texternalsym:$addr), (PseudoCALL texternalsym:$addr)>;

let isCall = 1, Defs = [R15] in {
  def PseudoCALLABS : Pseudo<(outs), (ins I64Regs:$addr_reg),
                             [(acca_call I64Regs:$addr_reg)]>,
                           PseudoInstExpansion<(CALL_abs_nocond I64Regs:$addr_reg)>;
  def PseudoCALLREGREL : Pseudo<(outs), (ins I64Regs:$addr_reg),
                                [(acca_call I64Regs:$addr_reg)]>,
                           PseudoInstExpansion<(CALL_regrel_nocond I64Regs:$addr_reg)>;
}

let isCall = 1, isTerminator = 1, isReturn = 1, isBarrier = 1 in
  def PseudoTAIL : Pseudo<(outs), (ins simm22_symbol:$addr)>;

def : Pat<(acca_tail (iPTR tglobaladdr:$addr)),
          (PseudoTAIL tglobaladdr:$addr)>;
def : Pat<(acca_tail (iPTR texternalsym:$addr)),
          (PseudoTAIL texternalsym:$addr)>;

let isCall = 1, isTerminator = 1, isReturn = 1, isBarrier = 1 in {
  def PseudoJMP_TAIL : Pseudo<(outs), (ins simm22_symbol:$addr)>,
                    PseudoInstExpansion<(JMP_immrel_nocond simm22_br:$addr)>;
  def PseudoJMPABS_TAIL : Pseudo<(outs), (ins I64Regs:$addr)>,
                    PseudoInstExpansion<(JMP_abs_nocond I64Regs:$addr)>;
  def PseudoJMPREGREL_TAIL : Pseudo<(outs), (ins I64Regs:$addr)>,
                    PseudoInstExpansion<(JMP_regrel_nocond I64Regs:$addr)>;
}

//
// codegen-only pseudo-instructions for stack adjustment for calls
//

// Pessimistically assume the stack pointer will be clobbered
let Defs = [R13], Uses = [R13] in {
  def ADJCALLSTACKDOWN : Pseudo<(outs), (ins i32imm:$amt1, i32imm:$amt2),
                                [(callseq_start timm:$amt1, timm:$amt2)]>;
  def ADJCALLSTACKUP   : Pseudo<(outs), (ins i32imm:$amt1, i32imm:$amt2),
                                [(callseq_end timm:$amt1, timm:$amt2)]>;
}

//
// patterns for `select`
//
foreach size = 0-3 in {
  defvar regclass = !cond(!eq(size, 0) : I8Regs, !eq(size, 1) : I16Regs, !eq(size, 2) : I32Regs, true : I64Regs);
  defvar sizechar = !cond(!eq(size, 0) : "b", !eq(size, 1) : "d", !eq(size, 2) : "q", true : "w");
  defvar opsize = !cond(!eq(size, 0) : "byte", !eq(size, 1) : "doublebyte", !eq(size, 2) : "quadbyte", true : "word");
  defvar shiftbits = !cond(!eq(size, 0) : 7, !eq(size, 1) : 15, !eq(size, 2) : 31, true : 63);

  defvar not_op = !cast<Instruction>(!strconcat("NOT_", opsize, "_nosetflags"));
  defvar and_op = !cast<Instruction>(!strconcat("AND_", opsize, "_nonnull_reg_nosetflags"));
  defvar or_op  = !cast<Instruction>(!strconcat("OR_",  opsize, "_nonnull_reg_nosetflags"));
  defvar shl_op = !cast<Instruction>(!strconcat("SHL_", opsize, "_nonnull_imm_noarith_nosetflags"));
  defvar shr_signed_op = !cast<Instruction>(!strconcat("SHR_", opsize, "_nonnull_imm_arith_nosetflags"));

  def : Pat<(select regclass:$cond, regclass:$trueval, regclass:$falseval),
            (or_op
              (and_op
                (shr_signed_op (shl_op $cond, shiftbits), shiftbits),
                $trueval),
              (and_op
                (not_op (shr_signed_op (shl_op $cond, shiftbits), shiftbits)),
                $falseval))>;
}

//
// patterns for `setcc`
//
foreach size = 0-3 in {
  defvar regclass = !cond(!eq(size, 0) : I8Regs, !eq(size, 1) : I16Regs, !eq(size, 2) : I32Regs, true : I64Regs);
  defvar opsize = !cond(!eq(size, 0) : "byte", !eq(size, 1) : "doublebyte", !eq(size, 2) : "quadbyte", true : "word");

  defvar soc_op = !cast<Instruction>(!strconcat("SOC_", opsize, "_noborrow"));

  // the carry bit means that the LHS is unsigned-less-than the RHS
  def : Pat<(setult regclass:$lhs, regclass:$rhs),
            (soc_op regclass:$lhs, regclass:$rhs, COND_C)>;
  // unsigned-greater-than is just the same as above, but flip LHS and RHS
  def : Pat<(setugt regclass:$lhs, regclass:$rhs),
            (soc_op regclass:$rhs, regclass:$lhs, COND_C)>;

  // to check for unsigned-greater-than-or-equal, we just check for the
  // opposite of unsigned-less-than. i.e. we check that the carry bit
  // is *not* set.
  def : Pat<(setuge regclass:$lhs, regclass:$rhs),
            (soc_op regclass:$lhs, regclass:$rhs, COND_NC)>;
  // unsigned-less-than-or-equal is just the same as above, but flip LHS and RHS
  def : Pat<(setule regclass:$lhs, regclass:$rhs),
            (soc_op regclass:$rhs, regclass:$lhs, COND_NC)>;

  // the virtual less-than bit means that the LHS is signed-less-than the RHS
  def : Pat<(setlt regclass:$lhs, regclass:$rhs),
            (soc_op regclass:$lhs, regclass:$rhs, COND_L)>;
  // signed-grater-than is just the same as above, but flip LHS and RHS
  def : Pat<(setgt regclass:$lhs, regclass:$rhs),
            (soc_op regclass:$rhs, regclass:$lhs, COND_L)>;

  // to check for signed-greater-than-or-equal, we just check for the
  // opposite of signed-less-than. i.e. we check that the virtual less-than bit
  // is *not* set.
  def : Pat<(setge regclass:$lhs, regclass:$rhs),
            (soc_op regclass:$lhs, regclass:$rhs, COND_NL)>;
  // signed-less-than-or-equal is just the same as above, but flip LHS and RHS
  def : Pat<(setle regclass:$lhs, regclass:$rhs),
            (soc_op regclass:$rhs, regclass:$lhs, COND_NL)>;

  // the zero bit means that the LHS is equal to the RHS
  def : Pat<(seteq regclass:$lhs, regclass:$rhs),
            (soc_op regclass:$lhs, regclass:$rhs, COND_Z)>;

  // logically, for inequality, we check for the opposite of equality
  // i.e. the zero bit is *not* set
  def : Pat<(setne regclass:$lhs, regclass:$rhs),
            (soc_op regclass:$lhs, regclass:$rhs, COND_NZ)>;
}

//
// patterns for `brcond`
//
foreach size = 0-3 in {
  defvar regclass = !cond(!eq(size, 0) : I8Regs, !eq(size, 1) : I16Regs, !eq(size, 2) : I32Regs, true : I64Regs);
  defvar regtype = !cond(!eq(size, 0) : i8, !eq(size, 1) : i16, !eq(size, 2) : i32, true : i64);
  defvar opsize = !cond(!eq(size, 0) : "byte", !eq(size, 1) : "doublebyte", !eq(size, 2) : "quadbyte", true : "word");
  defvar bitcount = !cond(!eq(size, 0) : 8, !eq(size, 1) : 16, !eq(size, 2) : 32, true : 64);
  defvar immzero = !if(!eq(size, 3), (LDI_noshift_clearall 0), (EXTRACT_SUBREG (LDI_noshift_clearall 0), !cast<SubRegIndex>(!strconcat("subreg_", opsize))));

  defvar cjmp_op = !cast<Instruction>(!strconcat("CJMP_", opsize, "_immrel"));

  def : Pat<(brcond regclass:$test, bb:$addr),
            (cjmp_op COND_NZ, bb:$addr, regclass:$test, immzero)>;
}
