//===-- AccaISelLowering.h - Acca DAG Lowering Interface ------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file defines the interfaces that Acca uses to lower LLVM code into a
// selection DAG.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_ACCA_ACCAISELLOWERING_H
#define LLVM_LIB_TARGET_ACCA_ACCAISELLOWERING_H

#include "Acca.h"
#include "llvm/CodeGen/TargetLowering.h"

namespace llvm {
  class AccaSubtarget;

  namespace AccaISD {
    enum NodeType : unsigned {
      FIRST_NUMBER = ISD::BUILTIN_OP_END,
      RET_FLAG,
    };
  }

  class AccaTargetLowering : public TargetLowering {
    const AccaSubtarget *Subtarget;
  public:
    AccaTargetLowering(const TargetMachine &TM, const AccaSubtarget &Subtarget);

    SDValue LowerOperation(SDValue Op, SelectionDAG &DAG) const override;

    SDValue LowerFormalArguments(SDValue Chain, CallingConv::ID CallConv,
                                 bool isVarArg,
                                 const SmallVectorImpl<ISD::InputArg> &Ins,
                                 const SDLoc &DL, SelectionDAG &DAG,
                                 SmallVectorImpl<SDValue> &InVals
                                 ) const override;

    SDValue LowerCall(CallLoweringInfo &CLI,
                      SmallVectorImpl<SDValue> &InVals) const override;

    SDValue LowerReturn(SDValue Chain, CallingConv::ID CallConv, bool IsVarArg,
                        const SmallVectorImpl<ISD::OutputArg> &Outs,
                        const SmallVectorImpl<SDValue> &OutVals,
                        const SDLoc &DL, SelectionDAG &DAG) const override;

    Register getRegisterByName(const char* RegName, LLT Ty,
                               const MachineFunction &MF) const override;

    bool useSoftFloat() const override;

  private:
    SDValue LowerSIGN_EXTEND(SDValue Op, SelectionDAG &DAG) const;
    SDValue LowerZERO_EXTEND(SDValue Op, SelectionDAG &DAG) const;
  };
}

#endif
