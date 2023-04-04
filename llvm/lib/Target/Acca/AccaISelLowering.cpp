//===-- AccaISelLowering.cpp - Acca DAG Lowering Implementation ---------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file implements the interfaces that Acca uses to lower LLVM code into a
// selection DAG.
//
//===----------------------------------------------------------------------===//

#include "AccaCallingConvention.h"
#include "AccaISelLowering.h"
//#include "MCTargetDesc/AccaMCExpr.h"
#include "AccaMachineFunctionInfo.h"
#include "AccaRegisterInfo.h"
#include "AccaTargetMachine.h"
#include "AccaTargetObjectFile.h"
#include "llvm/ADT/StringExtras.h"
#include "llvm/ADT/StringSwitch.h"
#include "llvm/CodeGen/CallingConvLower.h"
#include "llvm/CodeGen/MachineFrameInfo.h"
#include "llvm/CodeGen/MachineFunction.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/CodeGen/MachineRegisterInfo.h"
#include "llvm/CodeGen/SelectionDAG.h"
#include "llvm/CodeGen/SelectionDAGNodes.h"
#include "llvm/CodeGen/TargetLoweringObjectFileImpl.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Module.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/KnownBits.h"

using namespace llvm;

static bool CC_Acca_Assign_SRet(unsigned &ValNo, MVT &ValVT,
                                MVT &LocVT, CCValAssign::LocInfo &LocInfo,
                                ISD::ArgFlagsTy &ArgFlags, CCState &State) {
  llvm_unreachable("unimplemented");
}

#include "AccaGenCallingConv.inc"

AccaTargetLowering::AccaTargetLowering(const TargetMachine &TM, const AccaSubtarget &STI)
  : TargetLowering(TM), Subtarget(&STI) {
  setBooleanContents(ZeroOrOneBooleanContent);

  // set up register classes
  addRegisterClass(MVT::i8 , &Acca::I8RegsRegClass );
  addRegisterClass(MVT::i16, &Acca::I16RegsRegClass);
  addRegisterClass(MVT::i32, &Acca::I32RegsRegClass);
  addRegisterClass(MVT::i64, &Acca::I64RegsRegClass);

  // Acca doesn't have i1 loads/stores
  for (MVT VT : MVT::integer_valuetypes())
    setLoadExtAction(ArrayRef<unsigned int>({ ISD::NON_EXTLOAD, ISD::SEXTLOAD, ISD::ZEXTLOAD }), VT, MVT::i1, Promote);

  // Acca doesn't have sext_inreg, replace them with shl/sra
  setOperationAction(ISD::SIGN_EXTEND_INREG, MVT::i32, Expand);
  setOperationAction(ISD::SIGN_EXTEND_INREG, MVT::i16, Expand);
  setOperationAction(ISD::SIGN_EXTEND_INREG, MVT::i8 , Expand);
  setOperationAction(ISD::SIGN_EXTEND_INREG, MVT::i1 , Expand);

  setOperationAction(ISD::ANY_EXTEND, MVT::i1 , Custom);
  setOperationAction(ISD::ANY_EXTEND, MVT::i8 , Custom);
  setOperationAction(ISD::ANY_EXTEND, MVT::i16, Custom);
  setOperationAction(ISD::ANY_EXTEND, MVT::i32, Custom);
  setOperationAction(ISD::ANY_EXTEND, MVT::i64, Custom);

  setOperationAction(ISD::ZERO_EXTEND, MVT::i1 , Custom);
  setOperationAction(ISD::ZERO_EXTEND, MVT::i8 , Custom);
  setOperationAction(ISD::ZERO_EXTEND, MVT::i16, Custom);
  setOperationAction(ISD::ZERO_EXTEND, MVT::i32, Custom);
  setOperationAction(ISD::ZERO_EXTEND, MVT::i64, Custom);

  // Acca has no REM or DIV operations (only combined DIVREM).
  setOperationAction(ISD::UREM, MVT::i8 , Expand);
  setOperationAction(ISD::UREM, MVT::i16, Expand);
  setOperationAction(ISD::UREM, MVT::i32, Expand);
  setOperationAction(ISD::UREM, MVT::i64, Expand);

  setOperationAction(ISD::SREM, MVT::i8 , Expand);
  setOperationAction(ISD::SREM, MVT::i16, Expand);
  setOperationAction(ISD::SREM, MVT::i32, Expand);
  setOperationAction(ISD::SREM, MVT::i64, Expand);

  setOperationAction(ISD::SDIV, MVT::i8 , Expand);
  setOperationAction(ISD::SDIV, MVT::i16, Expand);
  setOperationAction(ISD::SDIV, MVT::i32, Expand);
  setOperationAction(ISD::SDIV, MVT::i64, Expand);

  setOperationAction(ISD::UDIV, MVT::i8 , Expand);
  setOperationAction(ISD::UDIV, MVT::i16, Expand);
  setOperationAction(ISD::UDIV, MVT::i32, Expand);
  setOperationAction(ISD::UDIV, MVT::i64, Expand);

  // Acca has no SELECT, SETCC, or SELECT_CC.
  setOperationAction(ISD::SELECT, MVT::i8 , Expand);
  setOperationAction(ISD::SELECT, MVT::i16, Expand);
  setOperationAction(ISD::SELECT, MVT::i32, Expand);
  setOperationAction(ISD::SELECT, MVT::i64, Expand);

  setOperationAction(ISD::SETCC, MVT::i8 , Expand);
  setOperationAction(ISD::SETCC, MVT::i16, Expand);
  setOperationAction(ISD::SETCC, MVT::i32, Expand);
  setOperationAction(ISD::SETCC, MVT::i64, Expand);

  setOperationAction(ISD::SELECT_CC, MVT::i8 , Expand);
  setOperationAction(ISD::SELECT_CC, MVT::i16, Expand);
  setOperationAction(ISD::SELECT_CC, MVT::i32, Expand);
  setOperationAction(ISD::SELECT_CC, MVT::i64, Expand);

  // Acca doesn't have BRCOND or BR_JT, it has BR_CC.
  setOperationAction(ISD::BRCOND, MVT::Other, Expand);
  setOperationAction(ISD::BR_JT, MVT::Other, Expand);
  //setOperationAction(ISD::BR_CC, MVT::i32, Custom);

  // Acca does not currently support atomics
  setMaxAtomicSizeInBitsSupported(0);

  setOperationAction(ISD::TRAP, MVT::Other, Legal);
  setOperationAction(ISD::DEBUGTRAP, MVT::Other, Legal);

  // stack allocation is done in 64-bit multiples for now
  // TODO: optimize this to allow allocating smaller than 64-bit stack slots
  setOperationAction(ISD::DYNAMIC_STACKALLOC, MVT::i8 , Promote);
  setOperationAction(ISD::DYNAMIC_STACKALLOC, MVT::i16, Promote);
  setOperationAction(ISD::DYNAMIC_STACKALLOC, MVT::i32, Promote);
  setOperationAction(ISD::DYNAMIC_STACKALLOC, MVT::i64, Custom);

  setMaxDivRemBitWidthSupported(64);

  setStackPointerRegisterToSaveRestore(Acca::R13);

  setMinFunctionAlignment(Align(4));

  computeRegisterProperties(Subtarget->getRegisterInfo());
};

bool AccaTargetLowering::useSoftFloat() const {
  return true;
}

SDValue AccaTargetLowering::
LowerOperation(SDValue Op, SelectionDAG &DAG) const {
  switch (Op.getOpcode()) {
    case ISD::SIGN_EXTEND:
      return LowerSIGN_EXTEND(Op, DAG);
    case ISD::ZERO_EXTEND:
    case ISD::ANY_EXTEND:
      return LowerZERO_EXTEND(Op, DAG);

    default:
      llvm_unreachable("Invalid/unreachable custom lowering operation");
  }
}

SDValue AccaTargetLowering::
LowerSIGN_EXTEND(SDValue Op, SelectionDAG &DAG) const {
  llvm_unreachable("unimplemented");
}

SDValue AccaTargetLowering::
LowerZERO_EXTEND(SDValue Op, SelectionDAG &DAG) const {
  llvm_unreachable("unimplemented");
}

SDValue AccaTargetLowering::
LowerFormalArguments(SDValue Chain, CallingConv::ID CallConv, bool IsVarArg,
                     const SmallVectorImpl<ISD::InputArg> &Ins,
                     const SDLoc &DL, SelectionDAG &DAG,
                     SmallVectorImpl<SDValue> &InVals) const {
  MachineFunction &MF = DAG.getMachineFunction();

  if (IsVarArg) {
    llvm_unreachable("vararg not implemented yet");
  }

  // Analyze arguments according to CC_Acca.
  SmallVector<CCValAssign, 16> ArgLocs;
  CCState CCInfo(CallConv, IsVarArg, DAG.getMachineFunction(), ArgLocs,
                 *DAG.getContext());
  CCInfo.AnalyzeFormalArguments(Ins, CC_Acca);

  for (unsigned i = 0, e = ArgLocs.size(); i != e; ++i) {
    CCValAssign &VA = ArgLocs[i];

    if (VA.isRegLoc()) {
      // This argument is passed in a register.
      // All integer arguments have been promoted to i64.

      // Create a virtual register for the promoted live-in value.
      Register VReg = MF.addLiveIn(VA.getLocReg(),
                                   getRegClassFor(VA.getLocVT()));
      SDValue Arg = DAG.getCopyFromReg(Chain, DL, VReg, VA.getLocVT());

      // The caller promoted the argument, so insert an Assert?ext SDNode so we
      // won't promote the value again in this function.
      switch (VA.getLocInfo()) {
      case CCValAssign::SExt:
        Arg = DAG.getNode(ISD::AssertSext, DL, VA.getLocVT(), Arg,
                          DAG.getValueType(VA.getValVT()));
        break;
      case CCValAssign::ZExt:
        Arg = DAG.getNode(ISD::AssertZext, DL, VA.getLocVT(), Arg,
                          DAG.getValueType(VA.getValVT()));
        break;
      default:
        break;
      }

      // Truncate the register down to the argument type.
      if (VA.isExtInLoc())
        Arg = DAG.getNode(ISD::TRUNCATE, DL, VA.getValVT(), Arg);

      InVals.push_back(Arg);
    } else {
      // The registers are exhausted. This argument was passed on the stack.
      assert(VA.isMemLoc());

      int FI = MF.getFrameInfo().CreateFixedObject(
        VA.getValVT().getSizeInBits() / 8,
        VA.getLocMemOffset(),
        true
      );
      InVals.push_back(
          DAG.getLoad(VA.getValVT(), DL, Chain,
                      DAG.getFrameIndex(FI, getPointerTy(MF.getDataLayout())),
                      MachinePointerInfo::getFixedStack(MF, FI)));
    }
  }

  return Chain;
};

SDValue AccaTargetLowering::
LowerCall(CallLoweringInfo &CLI, SmallVectorImpl<SDValue> &InVals) const {
  llvm_unreachable("unimplemented");
};

SDValue AccaTargetLowering::
LowerReturn(SDValue Chain, CallingConv::ID CallConv, bool IsVarArg,
            const SmallVectorImpl<ISD::OutputArg> &Outs,
            const SmallVectorImpl<SDValue> &OutVals,
            const SDLoc &DL, SelectionDAG &DAG) const {
  SmallVector<CCValAssign, 16> RVLocs;

  CCState CCInfo(CallConv, IsVarArg, DAG.getMachineFunction(), RVLocs,
                 *DAG.getContext());
  CCInfo.AnalyzeReturn(Outs, RetCC_Acca);

  SDValue Flag;
  SmallVector<SDValue, 4> RetOps(1, Chain);

  // Copy the result values into the output registers.
  for (unsigned i = 0, e = RVLocs.size(); i < e; ++i) {
    SDValue OutVal = OutVals[i];
    CCValAssign &VA = RVLocs[i];

    assert(VA.isRegLoc() && "Can only return in registers!");

    // Integer return values must be sign or zero extended by the callee.
    switch (VA.getLocInfo()) {
    case CCValAssign::Full: break;
    case CCValAssign::SExt:
      OutVal = DAG.getNode(ISD::SIGN_EXTEND, DL, VA.getLocVT(), OutVal);
      break;
    case CCValAssign::ZExt:
      OutVal = DAG.getNode(ISD::ZERO_EXTEND, DL, VA.getLocVT(), OutVal);
      break;
    case CCValAssign::AExt:
      OutVal = DAG.getNode(ISD::ANY_EXTEND, DL, VA.getLocVT(), OutVal);
      break;
    default:
      llvm_unreachable("Unknown loc info!");
    }

    Chain = DAG.getCopyToReg(Chain, DL, VA.getLocReg(), OutVal, Flag);

    // Guarantee that all emitted copies are stuck together with flags.
    Flag = Chain.getValue(1);
    RetOps.push_back(DAG.getRegister(VA.getLocReg(), VA.getLocVT()));
  }

  RetOps[0] = Chain;  // Update chain.

  // Add the flag if we have it.
  if (Flag.getNode())
    RetOps.push_back(Flag);

  return DAG.getNode(AccaISD::RET_FLAG, DL, MVT::Other, RetOps);
};

#define GET_REGISTER_MATCHER
#include "AccaGenAsmMatcher.inc"

Register AccaTargetLowering::
getRegisterByName(const char* RegName, LLT Ty,
                  const MachineFunction &MF) const {
  Register Reg = MatchRegisterName(RegName);

  if (Reg)
    return Reg;

  report_fatal_error(Twine("Invalid register name \"" + StringRef(RegName) +
                           "\"."));
};
