//===-- AccaCallingConv.td - Calling Conventions Acca --*- tablegen -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This describes the calling convention for the Acca architecture.
//
//===----------------------------------------------------------------------===//

let Entry = 1 in
def CC_Acca : CallingConv<[
  // SRet is handled by either returning in `r1:r0` or allocating space on the
  // stack at the tip of the stack upon entry.
  CCIfSRet<CCCustom<"CC_Acca_Assign_SRet">>,

  // all integer types are promoted to i64
  // and then we try assigning to a parameter register
  CCIfType<[i1, i8, i16, i32], CCPromoteToType<i64>>,
  CCIfType<[i64], CCAssignToReg<[R0, R1, R2, R3, R4, R5, R6]>>,

  // otherwise, if we're full, pass them on the stack
  CCAssignToStack<0, 0>
]>;

let Entry = 1 in
def RetCC_Acca : CallingConv<[
  CCIfType<[i1, i8, i16, i32], CCPromoteToType<i64>>,
  CCIfType<[i64], CCAssignToReg<[R0]>>
]>;

def CSR_Acca : CalleeSavedRegs<(add R7, R8, R13, R14, R15)>;
