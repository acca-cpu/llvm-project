//===- AccaSystemOperands.td -------------------------------*- tablegen -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file defines the symbolic operands permitted for various kinds of
// Acca system instructions.
//
//===----------------------------------------------------------------------===//

include "llvm/TableGen/SearchableTable.td"

//===----------------------------------------------------------------------===//
// LDM/STM (load/store machine register) instruction options.
//===----------------------------------------------------------------------===//

class SysReg<string name, bits<22> encoding> : SearchableTable {
  let SearchableFields = ["Name", "Encoding"];
  let EnumValueField = "Encoding";

  string Name = name;
  string AltName = name;
  bits<22> Encoding = encoding;
  bit Readable = ?;
  bit Writeable = ?;
}

class RWSysReg<string name, bits<22> encoding> : SysReg<name, encoding> {
  let Readable = 1;
  let Writeable = 1;
}

class ROSysReg<string name, bits<22> encoding> : SysReg<name, encoding> {
  let Readable = 1;
  let Writeable = 0;
}

class WOSysReg<string name, bits<22> encoding> : SysReg<name, encoding> {
  let Readable = 0;
  let Writeable = 1;
}

//===----------------------
// Read-only regs
//===----------------------

def : ROSysReg<"einfo", 0b0000000000000000000100>;
def : ROSysReg<"eaddr", 0b0000000000000000000101>;

//===----------------------
// Write-only regs
//===----------------------

//===----------------------
// Read-write regs
//===----------------------

def : RWSysReg<"flags",   0b0000000000000000000000>;
def : RWSysReg<"elr",     0b0000000000000000000001>;
def : RWSysReg<"esp",     0b0000000000000000000010>;
def : RWSysReg<"eflags",  0b0000000000000000000011>;
def : RWSysReg<"evtable", 0b0000000000000000000110>;
def : RWSysReg<"ectable", 0b0000000000000000000111>;
