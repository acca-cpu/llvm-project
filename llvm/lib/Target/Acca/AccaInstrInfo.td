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
        defvar issignedstr = !if(issigned, "signed", "unsigned");
        defvar setflagsstr = !if(setflags, "setflags", "nosetflags");
        def MUL_#srcsize#_#dstsize#_#issignedstr#_#setflagsstr : Mul<
          srcsize,
          dstsize,
          !cond(!eq(srcsize, 0) : I8Regs, !eq(srcsize, 1) : I16Regs, !eq(srcsize, 2) : I32Regs, true : I64Regs),
          !cond(!eq(dstsize, 0) : I8Regs, !eq(dstsize, 1) : I16Regs, !eq(dstsize, 2) : I32Regs, true : I64Regs),
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

defm AND : AndOrXor<0b100001, 0b100000, "and", and>;
defm OR  : AndOrXor<0b011111, 0b011110, "or",  or>;
defm XOR : AndOrXor<0b011101, 0b011100, "xor", xor>;

//
// shl/shr/rot
//

defm SHL : ShlShrRot<0b011010, 0b011011, "shl", 0, shl>;
defm SHR : ShlShrRot<0b011000, 0b011001, "shr", 1, srl>; // placeholder opnode
defm ROT : ShlShrRot<0b010110, 0b010111, "rot", 0, rotr>;

//
// neg/bswap
//

defm NEG   : NegBswap<0b010101, "neg",   ineg>;
defm BSWAP : NegBswap<0b010100, "bswap", bswap>;

//
// jmpa/jmpr
//

defm JMP : JmpCall<0b010011, 0b010010, 0b010001, "jmp">;

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
  defm CALL : JmpCall<0b001010, 0b001001, 0b001000, "call">;

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

  let isTerminator = 1;
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
