//===-- Acca.td - Describe the Acca Target Machine -------*- tablegen -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
//
//===----------------------------------------------------------------------===//

//===----------------------------------------------------------------------===//
// Target-independent interfaces which we are implementing
//===----------------------------------------------------------------------===//

include "llvm/Target/Target.td"

//===----------------------------------------------------------------------===//
// Register File, Calling Conv, Instruction Descriptions
//===----------------------------------------------------------------------===//

include "AccaRegisterInfo.td"
include "AccaCallingConv.td"
include "AccaSchedule.td"
include "AccaInstrInfo.td"

def AccaInstrInfo : InstrInfo;

//def AccaAsmParser : AsmParser;

class Proc<string Name, list<SubtargetFeature> Features>
 : Processor<Name, NoItineraries, Features>;

def : Proc<"generic", []>;

//===----------------------------------------------------------------------===//
// Assembly parser
//===----------------------------------------------------------------------===//

def GenericAsmParserVariant : AsmParserVariant {
  int Variant = 0;
  string Name = "generic";
  string BreakCharacters = ".";
}

//===----------------------------------------------------------------------===//
// Target Declaration
//===----------------------------------------------------------------------===//

def Acca : Target {
  let InstructionSet = AccaInstrInfo;
  let AssemblyParserVariants = [GenericAsmParserVariant];
}

//===----------------------------------------------------------------------===//
// Named operands for LDM/STM
//===----------------------------------------------------------------------===//

include "AccaSystemOperands.td"
