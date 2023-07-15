//===-- AccaInstrFormats.td - Acca Instruction Formats ---*- tablegen -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

class AccaInst<dag outs, dag ins, string asmstr, list<dag> pattern, InstrItinClass itin = NoItinerary> : Instruction {
  field bits<32> Inst;

  let Namespace = "Acca";
  let Size = 4;

  bits<6> opcode;
  let Inst{31-26} = opcode; // the top 6 bits are the opcode

  dag OutOperandList = outs;
  dag InOperandList = ins;
  let AsmString = asmstr;
  let Pattern = pattern;

  let DecoderNamespace = "Acca";
  field bits<32> SoftFail = 0;

  let Itinerary = itin;
}

//
// assembly operands
//

class UImm<int width> : AsmOperandClass {
  let Name = "UImm" # width;
  let DiagnosticType = "InvalidUImm" # width;
}

class SImm<int width> : AsmOperandClass {
  let Name = "SImm" # width;
  let DiagnosticType = "InvalidSImm" # width;
}

class TImm<int width> : AsmOperandClass {
  let Name = "TImm" # width;
  let DiagnosticType = "InvalidTImm" # width;
}

def UImm2Operand : UImm<2>;
def UImm3Operand : UImm<3>;
def UImm6Operand : UImm<6>;
def UImm7Operand : UImm<7>;
def UImm10Operand : UImm<10>;
def UImm11Operand : UImm<11>;
def UImm16Operand : UImm<16>;
def UImm22Operand : UImm<22>;

def SImm13Operand : SImm<13>;
def SImm22Operand : SImm<22>;

def TImm64Operand : TImm<64>;

def uimm2 : Operand<i64>, ImmLeaf<i64, [{ return Imm >= 0 && Imm < 4; }]> {
  let ParserMatchClass = UImm2Operand;
}

def uimm3 : Operand<i64>, ImmLeaf<i64, [{ return Imm >= 0 && Imm < 8; }]> {
  let ParserMatchClass = UImm3Operand;
}

def uimm6 : Operand<i64>, ImmLeaf<i64, [{ return Imm >= 0 && Imm < 64; }]> {
  let ParserMatchClass = UImm6Operand;
}

def uimm7 : Operand<i64>, ImmLeaf<i64, [{ return Imm >= 0 && Imm < 128; }]> {
  let ParserMatchClass = UImm7Operand;
}

def uimm11 : Operand<i64>, ImmLeaf<i64, [{ return Imm >= 0 && Imm < 2048; }]> {
  let ParserMatchClass = UImm11Operand;
}

def uimm16 : Operand<i64>, ImmLeaf<i64, [{ return Imm >= 0 && Imm < 65536; }]> {
  let ParserMatchClass = UImm16Operand;
}

def uimm22 : Operand<i64>, ImmLeaf<i64, [{ return Imm >= 0 && Imm < 4194304; }]> {
  let ParserMatchClass = UImm22Operand;
}

def simm13 : Operand<i64>, ImmLeaf<i64, [{ return Imm >= -4096 && Imm < 4096; }]> {
  let ParserMatchClass = SImm13Operand;
}

def simm13_br : Operand<OtherVT> {
  let ParserMatchClass = SImm13Operand;
}

def simm22 : Operand<i64>, ImmLeaf<i64, [{ return Imm >= -2097152 && Imm < 2097152; }]> {
  let ParserMatchClass = SImm22Operand;
}

def simm22_br : Operand<OtherVT> {
  let ParserMatchClass = SImm22Operand;
}

def simm22_symbol : Operand<i64> {
  let ParserMatchClass = SImm22Operand;
}

def timm64 : Operand<i64>, TImmLeaf<i64, [{ return true; }]> {
  let ParserMatchClass = TImm64Operand;
}

def CondCode : AsmOperandClass {
  let Name = "CondCode";
  let DiagnosticType = "InvalidCondCode";
}

// 4-bit immediate
def ccode : Operand<i8> {
  let PrintMethod = "printCondCode";
  let ParserMatchClass = CondCode;
}

defvar COND_C  = 0b0000;
defvar COND_NC = 0b0001;
defvar COND_Z  = 0b0010;
defvar COND_NZ = 0b0011;
defvar COND_O  = 0b0100;
defvar COND_NO = 0b0101;
defvar COND_S  = 0b0110;
defvar COND_NS = 0b0111;
defvar COND_L  = 0b1000;
defvar COND_NL = 0b1001;
defvar COND_NONE = 0b1111;

class ShiftedImm<ValueType Ty, Operand base, Operand shift> : Operand<Ty>, ComplexPattern<Ty, 2, "SelectShiftedImm", [imm]> {
  let PrintMethod = "printShiftedImm";
  let EncoderMethod = "getShiftedImmOpValue";
  //let ParserMatchClass = AddShiftedImmOperand;
  let MIOperandInfo = (ops base, shift);
}

//
// instruction formats
//

multiclass BasePushPop1<bits<6> op, string asmname, bit isPop, bits<2> size, RegisterClass regtype> {
  def _nonnull : AccaInst<
    !if(isPop, (outs regtype:$Ra), (outs)),
    !if(isPop, (ins), (ins regtype:$Ra)),
    !strconcat(asmname, " $Ra"),
    []
  > {
    bits<4> Ra;

    let opcode = op;
    let Uses = [R13];
    let Defs = [R13];
    let mayLoad = isPop;
    let mayStore = !not(isPop);
    let hasSideEffects = 0;

    let Inst{3-0} = Ra;
    let Inst{4} = 0;
    let Inst{6-5} = size;
  }

  def _null : AccaInst<
    (outs),
    (ins),
    !strconcat(asmname, " null"),
    []
  > {
    let opcode = op;
    let Uses = [R13];
    let Defs = [R13];
    let mayLoad = isPop;
    let mayStore = !not(isPop);
    let hasSideEffects = 0;

    let Inst{3-0} = 0b1111;
    let Inst{4} = 1;
    let Inst{6-5} = size;
  }
}

multiclass PushPop1<bits<6> op, string asmname, bit isStore> {
  defm _byte       : BasePushPop1<op, !strconcat(asmname, "{.b}"), isStore, 0b00, I8Regs>;
  defm _doublebyte : BasePushPop1<op, !strconcat(asmname, "{.d}"), isStore, 0b01, I16Regs>;
  defm _quadbyte   : BasePushPop1<op, !strconcat(asmname, "{.q}"), isStore, 0b10, I32Regs>;
  defm _word       : BasePushPop1<op, !strconcat(asmname, "{.w}"), isStore, 0b11, I64Regs>;
}

multiclass BasePushPop2<bits<6> op, string asmname, bit isPop, bits<2> size, RegisterClass regtype> {
  def _nonnull : AccaInst<
    !if(isPop, (outs regtype:$Ra, regtype:$Rb), (outs)),
    !if(isPop, (ins), (ins regtype:$Ra, regtype:$Rb)),
    !strconcat(asmname, " $Ra, $Rb"),
    []
  > {
    bits<4> Ra;
    bits<4> Rb;

    let opcode = op;
    let Uses = [R13];
    let Defs = [R13];
    let mayLoad = isPop;
    let mayStore = !not(isPop);
    let hasSideEffects = 0;

    let Inst{3-0} = Rb;
    let Inst{4} = 0;
    let Inst{8-5} = Ra;
    let Inst{9} = 0;
    let Inst{11-10} = size;
  }

  def _null_nonnull : AccaInst<
    !if(isPop, (outs regtype:$Rb), (outs)),
    !if(isPop, (ins), (ins regtype:$Rb)),
    !strconcat(asmname, " null, $Rb"),
    []
  > {
    bits<4> Rb;

    let opcode = op;
    let Uses = [R13];
    let Defs = [R13];
    let mayLoad = isPop;
    let mayStore = !not(isPop);
    let hasSideEffects = 0;

    let Inst{3-0} = Rb;
    let Inst{4} = 0;
    let Inst{8-5} = 0b1111;
    let Inst{9} = 1;
    let Inst{11-10} = size;
  }

  def _nonnull_null : AccaInst<
    !if(isPop, (outs regtype:$Ra), (outs)),
    !if(isPop, (ins), (ins regtype:$Ra)),
    !strconcat(asmname, " $Ra, null"),
    []
  > {
    bits<4> Ra;

    let opcode = op;
    let Uses = [R13];
    let Defs = [R13];
    let mayLoad = isPop;
    let mayStore = !not(isPop);
    let hasSideEffects = 0;

    let Inst{3-0} = 0b1111;
    let Inst{4} = 1;
    let Inst{8-5} = Ra;
    let Inst{9} = 0;
    let Inst{11-10} = size;
  }

  def _null : AccaInst<
    (outs),
    (ins),
    !strconcat(asmname, " null, null"),
    []
  > {
    let opcode = op;
    let Uses = [R13];
    let Defs = [R13];
    let mayLoad = isPop;
    let mayStore = !not(isPop);
    let hasSideEffects = 0;

    let Inst{3-0} = 0b1111;
    let Inst{4} = 1;
    let Inst{8-5} = 0b1111;
    let Inst{9} = 1;
    let Inst{11-10} = size;
  }
}

multiclass PushPop2<bits<6> op, string asmname, bit isStore> {
  defm _byte       : BasePushPop2<op, !strconcat(asmname, "{.b}"), isStore, 0b00, I8Regs>;
  defm _doublebyte : BasePushPop2<op, !strconcat(asmname, "{.d}"), isStore, 0b01, I16Regs>;
  defm _quadbyte   : BasePushPop2<op, !strconcat(asmname, "{.q}"), isStore, 0b10, I32Regs>;
  defm _word       : BasePushPop2<op, !strconcat(asmname, "{.w}"), isStore, 0b11, I64Regs>;
}

class BaseLoadStore1<bits<6> op, string asmname, bit isStore, bits<2> size, RegisterClass regtype, ValueType valtype> : AccaInst<
  !if(isStore, (outs), (outs regtype:$Ra)),
  !if(isStore, (ins I64Regs:$Ra, regtype:$Rb), (ins I64Regs:$Rb)),
  !strconcat(asmname, " $Ra, $Rb"),
  !if(isStore,
    [(store regtype:$Rb, I64Regs:$Ra)],
    []
  )
> {
  bits<4> Ra;
  bits<4> Rb;

  let opcode = op;
  let mayLoad = !not(isStore);
  let mayStore = isStore;
  let hasSideEffects = 0;

  let Inst{3-0} = Rb;
  let Inst{7-4} = Ra;
  let Inst{9-8} = size;
}

multiclass LoadStore1<bits<6> op, string asmname, bit isStore> {
  def _byte       : BaseLoadStore1<op, !strconcat(asmname, "{.b}"), isStore, 0b00, I8Regs,  i8>;
  def _doublebyte : BaseLoadStore1<op, !strconcat(asmname, "{.d}"), isStore, 0b01, I16Regs, i16>;
  def _quadbyte   : BaseLoadStore1<op, !strconcat(asmname, "{.q}"), isStore, 0b10, I32Regs, i32>;
  def _word       : BaseLoadStore1<op, !strconcat(asmname, "{.w}"), isStore, 0b11, I64Regs, i64>;
}

class BaseLoadStore2<bits<6> op, string asmname, bit isStore, bits<2> size, RegisterClass regtype> : AccaInst<
  !if(isStore, (outs), (outs regtype:$Ra, regtype:$Rb)),
  !if(isStore, (ins I64Regs:$Ra, regtype:$Rb, regtype:$Rc), (ins I64Regs:$Rc)),
  !strconcat(asmname, " $Ra, $Rb, $Rc"),
  []
> {
  bits<4> Ra;
  bits<4> Rb;
  bits<4> Rc;

  let opcode = op;
  let mayLoad = !not(isStore);
  let mayStore = isStore;
  let hasSideEffects = 0;

  let Inst{3-0} = Rc;
  let Inst{7-4} = Rb;
  let Inst{11-8} = Ra;
  let Inst{13-12} = size;
}

multiclass LoadStore2<bits<6> op, string asmname, bit isStore> {
  def _byte       : BaseLoadStore2<op, !strconcat(asmname, "{.b}"), isStore, 0b00, I8Regs>;
  def _doublebyte : BaseLoadStore2<op, !strconcat(asmname, "{.d}"), isStore, 0b01, I16Regs>;
  def _quadbyte   : BaseLoadStore2<op, !strconcat(asmname, "{.q}"), isStore, 0b10, I32Regs>;
  def _word       : BaseLoadStore2<op, !strconcat(asmname, "{.w}"), isStore, 0b11, I64Regs>;
}

class Copy<bits<2> size, RegisterClass regtype, ValueType valtype, string asmsuffix> : AccaInst<
  (outs regtype:$Rd),
  (ins regtype:$Rs),
  !strconcat("copy{.", asmsuffix, "} $Rd, $Rs"),
  [/*(set regtype:$Rd, regtype:$Rs)*/]
> {
  bits<4> Rd;
  bits<4> Rs;

  let mayLoad = 0;
  let mayStore = 0;
  let hasSideEffects = 0;

  let isMoveReg = true;

  let opcode = 0b101010;
  let Inst{3-0} = Rs;
  let Inst{7-4} = Rd;
  let Inst{9-8} = size;
}

multiclass BaseAddSub<bits<6> regop, bits<4> immop, bits<2> size, RegisterClass regtype, ValueType valtype, SDPatternOperator opnode, string asmname> {
  foreach carry = 0-1 in {
    foreach setflags = 0-1 in {
      defvar carrystr = !if(carry, "carry", "nocarry");
      defvar carryint = !if(carry, "1", "0");
      defvar setflagsstr = !if(setflags, "setflags", "nosetflags");
      defvar setflagsint = !if(setflags, "1", "0");

      def _nonnull_reg_#carrystr#_#setflagsstr : AccaInst<
        (outs regtype:$Rd),
        (ins regtype:$lhs, regtype:$rhs),
        !strconcat(asmname, " $Rd, $lhs, $rhs, ", carryint, ", ", setflagsint),
        [(set regtype:$Rd, (opnode regtype:$lhs, regtype:$rhs))]
      > {
        bits<4> Rd;
        bits<4> lhs;
        bits<4> rhs;

        let mayLoad = 0;
        let mayStore = 0;
        let hasSideEffects = 0;

        let Uses = !if(carry, [CZOS], []);
        let Defs = !if(setflags, [CZOS], []);
        let isCompare = setflags;

        let opcode = regop;
        let Inst{3-0} = rhs;
        let Inst{7-4} = lhs;
        let Inst{11-8} = Rd;
        let Inst{12} = 0;
        let Inst{13} = setflags;
        let Inst{14} = carry;
        let Inst{16-15} = size;
      }

      def _null_reg_#carrystr#_#setflagsstr : AccaInst<
        (outs),
        (ins regtype:$lhs, regtype:$rhs),
        !strconcat(asmname, " null, $lhs, $rhs, ", carryint, ", ", setflagsint),
        []
      > {
        bits<4> lhs;
        bits<4> rhs;

        let mayLoad = 0;
        let mayStore = 0;
        let hasSideEffects = 0;

        let Uses = !if(carry, [CZOS], []);
        let Defs = !if(setflags, [CZOS], []);
        let isCompare = setflags;

        let opcode = regop;
        let Inst{3-0} = rhs;
        let Inst{7-4} = lhs;
        let Inst{11-8} = 0b1111;
        let Inst{12} = 1;
        let Inst{13} = setflags;
        let Inst{14} = carry;
        let Inst{16-15} = size;
      }

      foreach arith = 0-1 in {
        defvar arithstr = !if(arith, "arith", "noarith");
        defvar arithint = !if(arith, "1", "0");

        def _nonnull_imm_#carrystr#_#setflagsstr#_#arithstr : AccaInst<
          (outs regtype:$Rd),
          (ins regtype:$lhs, uimm11:$rhs, uimm3:$shift),
          !strconcat(asmname, " $Rd, $lhs, $rhs, $shift, ", arithint, ", ", carryint, ", ", setflagsint),
          []
        > {
          bits<4> Rd;
          bits<4> lhs;
          bits<11> rhs;
          bits<3> shift;

          let Pattern = !if(!and(!not(carry), !not(arith)),
            [(set regtype:$Rd, (opnode regtype:$lhs, (ShiftedImm<valtype, uimm11, uimm3> uimm11:$rhs, uimm3:$shift)))],
            []
          );

          let mayLoad = 0;
          let mayStore = 0;
          let hasSideEffects = 0;

          let Uses = !if(carry, [CZOS], []);
          let Defs = !if(setflags, [CZOS], []);
          let isCompare = setflags;

          let opcode{5-2} = immop;
          let opcode{1-0} = size;
          let Inst{10-0} = rhs;
          let Inst{13-11} = shift;
          let Inst{14} = arith;
          let Inst{18-15} = lhs;
          let Inst{22-19} = Rd;
          let Inst{23} = 0;
          let Inst{24} = setflags;
          let Inst{25} = carry;
        }

        def _null_imm_#carrystr#_#setflagsstr#_#arithstr : AccaInst<
          (outs),
          (ins regtype:$lhs, uimm11:$rhs, uimm3:$shift),
          !strconcat(asmname, " null, $lhs, $rhs, $shift, ", arithint, ", ", carryint, ", ", setflagsint),
          []
        > {
          bits<4> Rd;
          bits<4> lhs;
          bits<11> rhs;
          bits<3> shift;

          let mayLoad = 0;
          let mayStore = 0;
          let hasSideEffects = 0;

          let Uses = !if(carry, [CZOS], []);
          let Defs = !if(setflags, [CZOS], []);
          let isCompare = setflags;

          let opcode{5-2} = immop;
          let opcode{1-0} = size;
          let Inst{10-0} = rhs;
          let Inst{13-11} = shift;
          let Inst{14} = arith;
          let Inst{18-15} = lhs;
          let Inst{22-19} = 0b1111;
          let Inst{23} = 1;
          let Inst{24} = setflags;
          let Inst{25} = carry;
        }
      }
    }
  }
}

multiclass AddSub<bits<6> regop, bits<4> immop, string asmname, SDPatternOperator opnode> {
  defm _byte       : BaseAddSub<regop, immop, 0b00, I8Regs,  i8,  opnode, !strconcat(asmname, "{.b}")>;
  defm _doublebyte : BaseAddSub<regop, immop, 0b01, I16Regs, i16, opnode, !strconcat(asmname, "{.d}")>;
  defm _quadbyte   : BaseAddSub<regop, immop, 0b10, I32Regs, i32, opnode, !strconcat(asmname, "{.q}")>;
  defm _word       : BaseAddSub<regop, immop, 0b11, I64Regs, i64, opnode, !strconcat(asmname, "{.w}")>;
}

class Mul<bits<2> srcsize, bits<2> dstsize, RegisterClass srctype, RegisterClass dsttype, list<dag> pattern, bit setflags, bit issigned> : AccaInst<
  (outs dsttype:$Rd),
  (ins srctype:$lhs, srctype:$rhs),
  !strconcat("mul $Rd, $lhs, $rhs, ", !if(issigned, "1", "0"), ", ", !if(setflags, "1", "0")),
  pattern
> {
  bits<4> Rd;
  bits<4> lhs;
  bits<4> rhs;
  bit issigned;

  let mayLoad = 0;
  let mayStore = 0;
  let hasSideEffects = 0;

  let Defs = !if(setflags, [CZOS], []);

  let opcode = 0b100011;
  let Inst{3-0} = rhs;
  let Inst{7-4} = lhs;
  let Inst{11-8} = Rd;
  let Inst{12} = setflags;
  let Inst{13} = issigned;
  let Inst{15-14} = dstsize;
  let Inst{17-16} = srcsize;
}

multiclass Div<bits<2> size, RegisterClass regtype, ValueType valtype, string asmsuffix> {
  foreach setflags = 0-1 in {
    foreach issigned = 0-1 in {
      defvar setflagsstr = !if(setflags, "setflags", "nosetflags");
      defvar issignedstr = !if(issigned, "signed", "unsigned");

      def _#setflagsstr#_#issignedstr : AccaInst<
        (outs regtype:$Rd, regtype: $Rr),
        (ins regtype:$lhs, regtype:$rhs),
        !strconcat("div{.", asmsuffix, "} $Rd, $Rr, $lhs, $rhs, ", !if(issigned, "1", "0"), ", ", !if(setflags, "1", "0")),
        []
      > {
        bits<4> Rd;
        bits<4> Rr;
        bits<4> lhs;
        bits<4> rhs;

        let mayLoad = 0;
        let mayStore = 0;
        let hasSideEffects = 0;

        let Defs = !if(setflags, [CZOS], []);

        let Pattern = !if(issigned,
          [(set regtype:$Rd, regtype:$Rr, (sdivrem regtype:$lhs, regtype:$rhs))],
          [(set regtype:$Rd, regtype:$Rr, (udivrem regtype:$lhs, regtype:$rhs))]
        );

        let opcode = 0b100010;
        let Inst{3-0} = rhs;
        let Inst{7-4} = lhs;
        let Inst{11-8} = Rr;
        let Inst{15-12} = Rd;
        let Inst{16} = setflags;
        let Inst{17} = issigned;
        let Inst{19-18} = size;
      }
    }
  }
}

multiclass BaseAndOrXor<bits<6> regop, bits<5> immop, string asmname, bits<2> size, RegisterClass regtype, ValueType valtype, SDPatternOperator opnode> {
  foreach setflags = 0-1 in {
    defvar setflagsstr = !if(setflags, "setflags", "nosetflags");
    defvar setflagsint = !if(setflags, "1", "0");

    def _nonnull_reg_#setflagsstr : AccaInst<
      (outs regtype:$Rd),
      (ins regtype:$lhs, regtype:$rhs),
      !strconcat(asmname, " $Rd, $lhs, $rhs, ", setflagsint),
      [(set regtype:$Rd, (opnode regtype:$lhs, regtype:$rhs))]
    > {
      bits<4> Rd;
      bits<4> lhs;
      bits<4> rhs;
      bit setflags;

      let mayLoad = 0;
      let mayStore = 0;
      let hasSideEffects = 0;

      let Defs = !if(setflags, [CZOS], []);
      let isCompare = setflags;

      let opcode = regop;
      let Inst{3-0} = rhs;
      let Inst{7-4} = lhs;
      let Inst{11-8} = Rd;
      let Inst{12} = 0;
      let Inst{13} = setflags;
      let Inst{15-14} = size;
    }

    def _null_reg_#setflagsstr : AccaInst<
      (outs),
      (ins regtype:$lhs, regtype:$rhs),
      !strconcat(asmname, " null, $lhs, $rhs, ", setflagsint),
      []
    > {
      bits<4> lhs;
      bits<4> rhs;
      bit setflags;

      let mayLoad = 0;
      let mayStore = 0;
      let hasSideEffects = 0;

      let Defs = !if(setflags, [CZOS], []);
      let isCompare = setflags;

      let opcode = regop;
      let Inst{3-0} = rhs;
      let Inst{7-4} = lhs;
      let Inst{11-8} = 0b1111;
      let Inst{12} = 1;
      let Inst{13} = setflags;
      let Inst{15-14} = size;
    }

    foreach arith = 0-1 in {
      defvar arithstr = !if(arith, "arith", "noarith");
      defvar arithint = !if(arith, "1", "0");

      def _nonnull_imm_#arithstr#_#setflagsstr : AccaInst<
        (outs regtype:$Rd),
        (ins regtype:$lhs, uimm11:$rhs, uimm3:$shift),
        !strconcat(asmname, " $Rd, $lhs, $rhs, $shift, ", arithint, ", ", setflagsint),
        [(set regtype:$Rd, (opnode regtype:$lhs, (ShiftedImm<valtype, uimm11, uimm3> uimm11:$rhs, uimm3:$shift)))]
      > {
        bits<4> Rd;
        bits<4> lhs;
        bits<11> rhs;
        bits<3> shift;
        bit setflags;

        let mayLoad = 0;
        let mayStore = 0;
        let hasSideEffects = 0;

        let Defs = !if(setflags, [CZOS], []);
        let isCompare = setflags;

        let opcode{5-1} = immop;
        let opcode{0} = arith;
        let Inst{2-0} = shift;
        let Inst{13-3} = rhs;
        let Inst{17-14} = lhs;
        let Inst{21-18} = Rd;
        let Inst{22} = 0;
        let Inst{23} = setflags;
        let Inst{25-24} = size;
      }

      def _null_imm_#arithstr#_#setflagsstr : AccaInst<
        (outs),
        (ins regtype:$lhs, uimm11:$rhs, uimm3:$shift),
        !strconcat(asmname, " null, $lhs, $rhs, $shift, ", arithint, ", ", setflagsint),
        []
      > {
        bits<4> lhs;
        bits<11> rhs;
        bits<3> shift;
        bit setflags;

        let mayLoad = 0;
        let mayStore = 0;
        let hasSideEffects = 0;

        let Defs = !if(setflags, [CZOS], []);
        let isCompare = setflags;

        let opcode{5-1} = immop;
        let opcode{0} = arith;
        let Inst{2-0} = shift;
        let Inst{13-3} = rhs;
        let Inst{17-14} = lhs;
        let Inst{21-18} = 0b1111;
        let Inst{22} = 1;
        let Inst{23} = setflags;
        let Inst{25-24} = size;
      }
    }
  }
}

multiclass AndOrXor<bits<6> regop, bits<5> immop, string asmname, SDPatternOperator opnode> {
  defm _byte       : BaseAndOrXor<regop, immop, !strconcat(asmname, "{.b}"), 0b00, I8Regs,  i8,  opnode>;
  defm _doublebyte : BaseAndOrXor<regop, immop, !strconcat(asmname, "{.d}"), 0b01, I16Regs, i16, opnode>;
  defm _quadbyte   : BaseAndOrXor<regop, immop, !strconcat(asmname, "{.q}"), 0b10, I32Regs, i32, opnode>;
  defm _word       : BaseAndOrXor<regop, immop, !strconcat(asmname, "{.w}"), 0b11, I64Regs, i64, opnode>;
}

multiclass BaseShlShrRot<bits<6> regop, bits<6> immop, string asmname, bit arithflag, bits<2> size, RegisterClass regtype, ValueType valtype, SDPatternOperator opnode> {
  foreach setflags = 0-1 in {
    foreach arith = 0-1 in {
      if !or(!eq(arith, 0), arithflag) then {
        defvar arithstr = !if(arith, "arith", "noarith");
        defvar arithint = !if(arith, "1", "0");
        defvar setflagsstr = !if(setflags, "setflags", "nosetflags");
        defvar setflagsint = !if(setflags, "1", "0");

        def _nonnull_reg_#arithstr#_#setflagsstr : AccaInst<
          (outs regtype:$Rd),
          (ins regtype:$lhs, regtype:$rhs),
          !strconcat(asmname, " $Rd, $lhs, $rhs, ", !if(arithflag, !strconcat(arithint, ", "), ""), setflagsint),
          []
        > {
          bits<4> Rd;
          bits<4> lhs;
          bits<4> rhs;

          let Pattern = !if(arithflag,
            !if(arith,
              [(set regtype:$Rd, (sra regtype:$lhs, regtype:$rhs))],
              [(set regtype:$Rd, (srl regtype:$lhs, regtype:$rhs))]
            ),
            [(set regtype:$Rd, (opnode regtype:$lhs, regtype:$rhs))]
          );

          let mayLoad = 0;
          let mayStore = 0;
          let hasSideEffects = 0;

          let Defs = !if(setflags, [CZOS], []);

          let opcode = regop;
          let Inst{3-0} = rhs;
          let Inst{7-4} = lhs;
          let Inst{11-8} = Rd;
          let Inst{12} = 0;
          let Inst{13} = setflags;

          let Inst{14} = !if(arithflag, arith, size{0});
          let Inst{15} = !if(arithflag, size{0}, size{1});
          let Inst{16} = !if(arithflag, size{1}, 0);
        }

        def _nonnull_imm_#arithstr#_#setflagsstr : AccaInst<
          (outs regtype:$Rd),
          (ins regtype:$lhs, uimm7:$rhs),
          !strconcat(asmname, " $Rd, $lhs, $rhs, ", !if(arithflag, !strconcat(arithint, ", "), ""), setflagsint),
          []
        > {
          bits<4> Rd;
          bits<4> lhs;
          bits<7> rhs;

          let Pattern = !if(arithflag,
            !if(arith,
              [(set regtype:$Rd, (sra regtype:$lhs, uimm7:$rhs))],
              [(set regtype:$Rd, (srl regtype:$lhs, uimm7:$rhs))]
            ),
            [(set regtype:$Rd, (opnode regtype:$lhs, uimm7:$rhs))]
          );

          let mayLoad = 0;
          let mayStore = 0;
          let hasSideEffects = 0;

          let Defs = !if(setflags, [CZOS], []);

          let opcode = immop;
          let Inst{6-0} = rhs;
          let Inst{10-7} = lhs;
          let Inst{14-11} = Rd;
          let Inst{15} = 0;
          let Inst{16} = setflags;

          let Inst{17} = !if(arithflag, arith, size{0});
          let Inst{18} = !if(arithflag, size{0}, size{1});
          let Inst{19} = !if(arithflag, size{1}, 0);
        }

        def _null_reg_#arithstr#_#setflagsstr : AccaInst<
          (outs),
          (ins regtype:$lhs, regtype:$rhs),
          !strconcat(asmname, " null, $lhs, $rhs, ", !if(arithflag, !strconcat(arithint, ", "), ""), setflagsint),
          []
        > {
          bits<4> lhs;
          bits<4> rhs;

          let mayLoad = 0;
          let mayStore = 0;
          let hasSideEffects = 0;

          let Defs = !if(setflags, [CZOS], []);

          let opcode = regop;
          let Inst{3-0} = rhs;
          let Inst{7-4} = lhs;
          let Inst{11-8} = 0b1111;
          let Inst{12} = 1;
          let Inst{13} = setflags;

          let Inst{14} = !if(arithflag, arith, size{0});
          let Inst{15} = !if(arithflag, size{0}, size{1});
          let Inst{16} = !if(arithflag, size{1}, 0);
        }

        def _null_imm_#arithstr#_#setflagsstr : AccaInst<
          (outs),
          (ins regtype:$lhs, uimm7:$rhs),
          !strconcat(asmname, " null, $lhs, $rhs, ", !if(arithflag, !strconcat(arithint, ", "), ""), setflagsint),
          []
        > {
          bits<4> lhs;
          bits<7> rhs;

          let mayLoad = 0;
          let mayStore = 0;
          let hasSideEffects = 0;

          let Defs = !if(setflags, [CZOS], []);

          let opcode = immop;
          let Inst{6-0} = rhs;
          let Inst{10-7} = lhs;
          let Inst{14-11} = 0b1111;
          let Inst{15} = 1;
          let Inst{16} = setflags;

          let Inst{17} = !if(arithflag, arith, size{0});
          let Inst{18} = !if(arithflag, size{0}, size{1});
          let Inst{19} = !if(arithflag, size{1}, 0);
        }
      }
    }
  }
}

multiclass ShlShrRot<bits<6> regop, bits<6> immop, string asmname, bit arithflag, SDPatternOperator opnode> {
  defm _byte       : BaseShlShrRot<regop, immop, !strconcat(asmname, "{.b}"), arithflag, 0b00, I8Regs,  i8,  opnode>;
  defm _doublebyte : BaseShlShrRot<regop, immop, !strconcat(asmname, "{.d}"), arithflag, 0b01, I16Regs, i16, opnode>;
  defm _quadbyte   : BaseShlShrRot<regop, immop, !strconcat(asmname, "{.q}"), arithflag, 0b10, I32Regs, i32, opnode>;
  defm _word       : BaseShlShrRot<regop, immop, !strconcat(asmname, "{.w}"), arithflag, 0b11, I64Regs, i64, opnode>;
}

class BaseNegBswap<bits<6> op, string asmname, bits<2> size, RegisterClass regtype, ValueType valtype, SDPatternOperator opnode, bit setflags> : AccaInst<
  (outs regtype:$Rd),
  (ins regtype:$Ra),
  !strconcat(asmname, " $Rd, $Ra, ", !if(setflags, "1", "0")),
  [(set regtype:$Rd, (opnode regtype:$Ra))]
> {
  bits<4> Rd;
  bits<4> Ra;

  let mayLoad = 0;
  let mayStore = 0;
  let hasSideEffects = 0;

  let Defs = !if(setflags, [CZOS], []);

  let opcode = op;
  let Inst{3-0} = Ra;
  let Inst{7-4} = Rd;
  let Inst{8} = setflags;
  let Inst{10-9} = size;
}

multiclass NegBswap<bits<6> op, string asmname, SDPatternOperator opnode> {
  foreach setflags = 0-1 in {
    defvar setflagsstr = !if(setflags, "setflags", "nosetflags");

    def _byte_#setflagsstr       : BaseNegBswap<op, !strconcat(asmname, "{.b}"), 0b00, I8Regs,  i8,  opnode, setflags>;
    def _doublebyte_#setflagsstr : BaseNegBswap<op, !strconcat(asmname, "{.d}"), 0b01, I16Regs, i16, opnode, setflags>;
    def _quadbyte_#setflagsstr   : BaseNegBswap<op, !strconcat(asmname, "{.q}"), 0b10, I32Regs, i32, opnode, setflags>;
    def _word_#setflagsstr       : BaseNegBswap<op, !strconcat(asmname, "{.w}"), 0b11, I64Regs, i64, opnode, setflags>;
  }
}

multiclass JmpCall<bits<6> absop, bits<6> regrelop, bits<6> immrelop, string asmname, bit isCall> {
  let isBranch = 1, mayLoad = 0, mayStore = 0 in {
    let isIndirectBranch = 1 in {
      def _abs_nocond : AccaInst<
        (outs),
        (ins I64Regs:$Ra),
        !strconcat(asmname, "a $Ra"),
        []
      > {
        bits<4> Ra;

        let opcode = absop;
        let Inst{3-0} = Ra;
        let Inst{7-4} = 0b1111;
      }

      def _abs_cond : AccaInst<
        (outs),
        (ins ccode:$cond, I64Regs:$Ra),
        !strconcat(asmname, "a.$cond $Ra"),
        []
      > {
        bits<3> cond;
        bits<4> Ra;

        let Uses = [CZOS];

        let opcode = absop;
        let Inst{3-0} = Ra;
        let Inst{6-4} = cond;
        let Inst{7} = 0;
      }

      def _regrel_nocond : AccaInst<
        (outs),
        (ins I64Regs:$Ra),
        !strconcat(asmname, "r $Ra"),
        []
      > {
        bits<4> Ra;

        let opcode = regrelop;
        let Inst{3-0} = Ra;
        let Inst{7-4} = 0b1111;
      }

      def _regrel_cond : AccaInst<
        (outs),
        (ins ccode:$cond, I64Regs:$Ra),
        !strconcat(asmname, "r.$cond $Ra"),
        []
      > {
        bits<3> cond;
        bits<4> Ra;

        let Uses = [CZOS];

        let opcode = regrelop;
        let Inst{3-0} = Ra;
        let Inst{6-4} = cond;
        let Inst{7} = 0;
      }
    }

    def _immrel_nocond : AccaInst<
      (outs),
      (ins !if(isCall, simm22, simm22_br):$addr),
      !strconcat(asmname, "r $addr"),
      []
    > {
      bits<22> addr;

      let opcode = immrelop;
      let Inst{21-0} = addr;
      let Inst{25-22} = 0b1111;
    }

    def _immrel_cond : AccaInst<
      (outs),
      (ins ccode:$cond, !if(isCall, simm22, simm22_br):$addr),
      !strconcat(asmname, "r.$cond $addr"),
      []
    > {
      bits<3> cond;
      bits<22> addr;

      let Uses = [CZOS];

      let opcode = immrelop;
      let Inst{21-0} = addr;
      let Inst{24-22} = cond;
      let Inst{25} = 0;
    }
  }
}

multiclass Cjmp<bits<2> size, RegisterClass regtype/*, string asmsuffix*/> {
  defvar sizestr = !cond(!eq(size, 0) : "b", !eq(size, 1) : "d", !eq(size, 2) : "q", true : "w");

  let isBranch = 1 in {
    let isIndirectBranch = 1 in {
      def _abs : AccaInst<
        (outs),
        (ins ccode:$cond, I64Regs:$Raddr, regtype:$lhs, regtype:$rhs),
        !strconcat("cjmpa.$cond.", sizestr, " $Raddr, $lhs, $rhs"),
        []
      > {
        bits<4> cond;
        bits<4> Raddr;
        bits<4> lhs;
        bits<4> rhs;

        let opcode = 0b001111;
        let Inst{3-0} = rhs;
        let Inst{7-4} = lhs;
        let Inst{11-8} = Raddr;
        let Inst{13-12} = size;
        let Inst{17-14} = cond;
      }

      def _regrel : AccaInst<
        (outs),
        (ins ccode:$cond, I64Regs:$Raddr, regtype:$lhs, regtype:$rhs),
        !strconcat("cjmpr.$cond.", sizestr, " $Raddr, $lhs, $rhs"),
        []
      > {
        bits<4> cond;
        bits<4> Raddr;
        bits<4> lhs;
        bits<4> rhs;

        let opcode = 0b001101;
        let Inst{3-0} = rhs;
        let Inst{7-4} = lhs;
        let Inst{11-8} = Raddr;
        let Inst{13-12} = size;
        let Inst{17-14} = cond;
      }
    }

    def _immrel : AccaInst<
      (outs),
      (ins ccode:$cond, simm13_br:$addr, regtype:$lhs, regtype:$rhs),
      !strconcat("cjmpr.$cond.", sizestr, " $addr, $lhs, $rhs"),
      []
    > {
      bits<4> cond;
      bits<13> addr;
      bits<4> lhs;
      bits<4> rhs;

      let opcode{5-1} = 0b11111;
      let opcode{0} = cond{3};
      let Inst{12-0} = addr;
      let Inst{16-13} = rhs;
      let Inst{20-17} = lhs;
      let Inst{22-21} = size;
      let Inst{25-23} = cond{2-0};
    }
  }
}

class NoOperands<bits<6> op, string asmname, list<dag> pattern> : AccaInst<
  (outs),
  (ins),
  asmname,
  pattern
> {
  let opcode = op;
}

// Pseudo instructions
class Pseudo<dag outs, dag ins, list<dag> pattern = [], string asmstr = "">
    : AccaInst<outs, ins, asmstr, pattern> {
  let isPseudo = 1;
  let isCodeGenOnly = 1;
}
