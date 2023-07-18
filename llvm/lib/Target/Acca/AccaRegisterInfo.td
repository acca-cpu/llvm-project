//===-- AccaRegisterInfo.td - Acca Register defs ---------*- tablegen -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

//===----------------------------------------------------------------------===//
//  Declarations that describe the Acca register file
//===----------------------------------------------------------------------===//

class AccaReg<bits<4> enc, string n, list<string> alt = []> : Register<n> {
  let Namespace = "Acca";
  let HWEncoding{3-0} = enc;
  let AltNames = alt;
}

let Namespace = "Acca" in {
  def subreg_byte : SubRegIndex<8>;
  def subreg_doublebyte : SubRegIndex<16>;
  def subreg_quadbyte : SubRegIndex<32>;
}

foreach i = 0-15 in {
  def R#i#B : AccaReg<i, "r"#i#"b", !cond(
    !eq(i, 13) : ["r13b", "rspb"],
    !eq(i, 14) : ["r14b", "rfpb"],
    !eq(i, 15) : ["r15b", "rlrb"],
    true : []
  )>, DwarfRegNum<[i]>;

  def R#i#D : AccaReg<i, "r"#i#"d", !cond(
    !eq(i, 13) : ["r13d", "rspd"],
    !eq(i, 14) : ["r14d", "rfpd"],
    !eq(i, 15) : ["r15d", "rlrd"],
    true : []
  )>, DwarfRegNum<[i]> {
    let SubRegs = [!cast<AccaReg>("R"#i#"B")];
    let SubRegIndices = [subreg_byte];
  }

  def R#i#Q : AccaReg<i, "r"#i#"q", !cond(
    !eq(i, 13) : ["r13q", "rspq"],
    !eq(i, 14) : ["r14q", "rfpq"],
    !eq(i, 15) : ["r15q", "rlrq"],
    true : []
  )>, DwarfRegNum<[i]> {
    let SubRegs = [!cast<AccaReg>("R"#i#"D")];
    let SubRegIndices = [subreg_doublebyte];
  }

  def R#i : AccaReg<i, "r"#i, !cond(
    !eq(i, 13) : ["r13w", "rsp", "rspw"],
    !eq(i, 14) : ["r14w", "rfp", "rfpw"],
    !eq(i, 15) : ["r15w", "rlr", "rlrw"],
    true : ["r"#i#"w"]
  )>, DwarfRegNum<[i]> {
    let SubRegs = [!cast<AccaReg>("R"#i#"Q")];
    let SubRegIndices = [subreg_quadbyte];
  }
}

def CZOS : AccaReg<0, "czos">;

// list caller-saved registers before callee-saved registers.
def I8Regs : RegisterClass<"Acca", [i8], 8, (add (sequence "R%uB", 9, 12),
                                                 (sequence "R%uB", 0,  6),
                                                 R15B, R7B, R8B, R14B, R13B)>;

def I16Regs : RegisterClass<"Acca", [i16], 16, (add (sequence "R%uD", 9, 12),
                                                    (sequence "R%uD", 0,  6),
                                                    R15D, R7D, R8D, R14D, R13D)>;

def I32Regs : RegisterClass<"Acca", [i32], 32, (add (sequence "R%uQ", 9, 12),
                                                    (sequence "R%uQ", 0,  6),
                                                    R15Q, R7Q, R8Q, R14Q, R13Q)>;

def I64Regs : RegisterClass<"Acca", [i64], 64, (add (sequence "R%u", 9, 12),
                                                    (sequence "R%u", 0,  6),
                                                    R15, R7, R8, R14, R13)>;

def CCR : RegisterClass<"Acca", [i8], 8, (add CZOS)> {
  // the condition code register can neither be copied nor allocated
  let CopyCost = -1;
  let isAllocatable = 0;
}
