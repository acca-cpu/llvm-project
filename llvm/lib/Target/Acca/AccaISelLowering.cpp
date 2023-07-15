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

#include "AccaBaseInfo.h"
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
#include "llvm/CodeGen/ISDOpcodes.h"
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

  // Acca has no SELECT_CC.
  setOperationAction(ISD::SELECT_CC, MVT::i8 , Expand);
  setOperationAction(ISD::SELECT_CC, MVT::i16, Expand);
  setOperationAction(ISD::SELECT_CC, MVT::i32, Expand);
  setOperationAction(ISD::SELECT_CC, MVT::i64, Expand);

  // Acca doesn't have BR_CC or BR_JT, it has BRCOND.
  setOperationAction(ISD::BR_CC, MVT::i8 , Expand);
  setOperationAction(ISD::BR_CC, MVT::i16, Expand);
  setOperationAction(ISD::BR_CC, MVT::i32, Expand);
  setOperationAction(ISD::BR_CC, MVT::i64, Expand);
  setOperationAction(ISD::BR_JT, MVT::Other, Expand);
  //setOperationAction(ISD::BRCOND, MVT::i32, Custom);

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
    default:
      llvm_unreachable("Invalid/unreachable custom lowering operation");
  }
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

static Align getPrefTypeAlign(EVT VT, SelectionDAG &DAG) {
  return DAG.getDataLayout().getPrefTypeAlign(
      VT.getTypeForEVT(*DAG.getContext()));
}

static SDValue convertValVTToLocVT(SelectionDAG &DAG, SDValue Val,
                                   const CCValAssign &VA, const SDLoc &DL,
                                   const AccaSubtarget &Subtarget) {
  EVT LocVT = VA.getLocVT();

  switch (VA.getLocInfo()) {
  default:
    llvm_unreachable("Unexpected CCValAssign::LocInfo");
  case CCValAssign::Full:
    if (VA.getValVT().isFixedLengthVector() && LocVT.isScalableVector())
      llvm_unreachable("Acca doesn't support vectors");
    break;
  case CCValAssign::AExt:
    Val = DAG.getNode(ISD::ANY_EXTEND, DL, LocVT, Val);
    break;
  case CCValAssign::BCvt:
    Val = DAG.getNode(ISD::BITCAST, DL, LocVT, Val);
    break;
  }
  return Val;
}

// Convert Val to a ValVT. Should not be called for CCValAssign::Indirect
// values.
static SDValue convertLocVTToValVT(SelectionDAG &DAG, SDValue Val,
                                   const CCValAssign &VA, const SDLoc &DL,
                                   const AccaSubtarget &Subtarget) {
  switch (VA.getLocInfo()) {
  default:
    llvm_unreachable("Unexpected CCValAssign::LocInfo");
  case CCValAssign::Full:
    if (VA.getValVT().isFixedLengthVector() && VA.getLocVT().isScalableVector())
      llvm_unreachable("Acca doesn't support vectors");
    break;
  case CCValAssign::AExt:
    Val = DAG.getNode(ISD::TRUNCATE, DL, VA.getValVT(), Val);
    break;
  case CCValAssign::Trunc:
    Val = DAG.getNode(ISD::ANY_EXTEND, DL, VA.getValVT(), Val);
    break;
  case CCValAssign::BCvt:
    Val = DAG.getNode(ISD::BITCAST, DL, VA.getValVT(), Val);
    break;
  }
  return Val;
}

// Lower a call to a callseq_start + CALL + callseq_end chain, and add input
// and output parameter nodes.
SDValue AccaTargetLowering::
LowerCall(CallLoweringInfo &CLI, SmallVectorImpl<SDValue> &InVals) const {
  SelectionDAG &DAG = CLI.DAG;
  SDLoc &DL = CLI.DL;
  SmallVectorImpl<ISD::OutputArg> &Outs = CLI.Outs;
  SmallVectorImpl<SDValue> &OutVals = CLI.OutVals;
  SmallVectorImpl<ISD::InputArg> &Ins = CLI.Ins;
  SDValue Chain = CLI.Chain;
  SDValue Callee = CLI.Callee;
  CallingConv::ID CallConv = CLI.CallConv;
  bool IsVarArg = CLI.IsVarArg;
  EVT PtrVT = getPointerTy(DAG.getDataLayout());
  bool &IsTailCall = CLI.IsTailCall;
  MVT GRLenVT = MVT::i64;

  MachineFunction &MF = DAG.getMachineFunction();

  // Analyze the operands of the call, assigning locations to each operand.
  SmallVector<CCValAssign> ArgLocs;
  CCState ArgCCInfo(CallConv, IsVarArg, MF, ArgLocs, *DAG.getContext());

  ArgCCInfo.AnalyzeCallOperands(Outs, CC_Acca);

  // Check if it's really possible to do a tail call.
  // TODO: for now, just don't do tail calls.
  if (IsTailCall)
    IsTailCall = false;

  if (IsTailCall)
    (void)0;
  else if (CLI.CB && CLI.CB->isMustTailCall())
    report_fatal_error("failed to perform tail call elimination on a call "
                       "site marked musttail");

  // Get a count of how many bytes are to be pushed on the stack.
  unsigned NumBytes = ArgCCInfo.getNextStackOffset();

  // Create local copies for byval args.
  SmallVector<SDValue> ByValArgs;
  for (unsigned i = 0, e = Outs.size(); i != e; ++i) {
    ISD::ArgFlagsTy Flags = Outs[i].Flags;
    if (!Flags.isByVal())
      continue;

    SDValue Arg = OutVals[i];
    unsigned Size = Flags.getByValSize();
    Align Alignment = Flags.getNonZeroByValAlign();

    int FI =
        MF.getFrameInfo().CreateStackObject(Size, Alignment, /*isSS=*/false);
    SDValue FIPtr = DAG.getFrameIndex(FI, getPointerTy(DAG.getDataLayout()));
    SDValue SizeNode = DAG.getConstant(Size, DL, GRLenVT);

    Chain = DAG.getMemcpy(Chain, DL, FIPtr, Arg, SizeNode, Alignment,
                          /*IsVolatile=*/false,
                          /*AlwaysInline=*/false, /*isTailCall=*/IsTailCall,
                          MachinePointerInfo(), MachinePointerInfo());
    ByValArgs.push_back(FIPtr);
  }

  if (!IsTailCall)
    Chain = DAG.getCALLSEQ_START(Chain, NumBytes, 0, CLI.DL);

  // Copy argument values to their designated locations.
  SmallVector<std::pair<Register, SDValue>> RegsToPass;
  SmallVector<SDValue> MemOpChains;
  SDValue StackPtr;
  for (unsigned i = 0, j = 0, e = ArgLocs.size(); i != e; ++i) {
    CCValAssign &VA = ArgLocs[i];
    SDValue ArgValue = OutVals[i];
    ISD::ArgFlagsTy Flags = Outs[i].Flags;

    // Promote the value if needed.
    // For now, only handle fully promoted and indirect arguments.
    if (VA.getLocInfo() == CCValAssign::Indirect) {
      // Store the argument in a stack slot and pass its address.
      Align StackAlign =
          std::max(getPrefTypeAlign(Outs[i].ArgVT, DAG),
                   getPrefTypeAlign(ArgValue.getValueType(), DAG));
      TypeSize StoredSize = ArgValue.getValueType().getStoreSize();
      // If the original argument was split and passed by reference, we need to
      // store the required parts of it here (and pass just one address).
      unsigned ArgIndex = Outs[i].OrigArgIndex;
      unsigned ArgPartOffset = Outs[i].PartOffset;
      assert(ArgPartOffset == 0);
      // Calculate the total size to store. We don't have access to what we're
      // actually storing other than performing the loop and collecting the
      // info.
      SmallVector<std::pair<SDValue, SDValue>> Parts;
      while (i + 1 != e && Outs[i + 1].OrigArgIndex == ArgIndex) {
        SDValue PartValue = OutVals[i + 1];
        unsigned PartOffset = Outs[i + 1].PartOffset - ArgPartOffset;
        SDValue Offset = DAG.getIntPtrConstant(PartOffset, DL);
        EVT PartVT = PartValue.getValueType();

        StoredSize += PartVT.getStoreSize();
        StackAlign = std::max(StackAlign, getPrefTypeAlign(PartVT, DAG));
        Parts.push_back(std::make_pair(PartValue, Offset));
        ++i;
      }
      SDValue SpillSlot = DAG.CreateStackTemporary(StoredSize, StackAlign);
      int FI = cast<FrameIndexSDNode>(SpillSlot)->getIndex();
      MemOpChains.push_back(
          DAG.getStore(Chain, DL, ArgValue, SpillSlot,
                       MachinePointerInfo::getFixedStack(MF, FI)));
      for (const auto &Part : Parts) {
        SDValue PartValue = Part.first;
        SDValue PartOffset = Part.second;
        SDValue Address =
            DAG.getNode(ISD::ADD, DL, PtrVT, SpillSlot, PartOffset);
        MemOpChains.push_back(
            DAG.getStore(Chain, DL, PartValue, Address,
                         MachinePointerInfo::getFixedStack(MF, FI)));
      }
      ArgValue = SpillSlot;
    } else {
      ArgValue = convertValVTToLocVT(DAG, ArgValue, VA, DL, *Subtarget);
    }

    // Use local copy if it is a byval arg.
    if (Flags.isByVal())
      ArgValue = ByValArgs[j++];

    if (VA.isRegLoc()) {
      // Queue up the argument copies and emit them at the end.
      RegsToPass.push_back(std::make_pair(VA.getLocReg(), ArgValue));
    } else {
      assert(VA.isMemLoc() && "Argument not register or memory");
      assert(!IsTailCall && "Tail call not allowed if stack is used "
                            "for passing parameters");

      // Work out the address of the stack slot.
      if (!StackPtr.getNode())
        StackPtr = DAG.getCopyFromReg(Chain, DL, Acca::R13, PtrVT);
      SDValue Address =
          DAG.getNode(ISD::ADD, DL, PtrVT, StackPtr,
                      DAG.getIntPtrConstant(VA.getLocMemOffset(), DL));

      // Emit the store.
      MemOpChains.push_back(
          DAG.getStore(Chain, DL, ArgValue, Address, MachinePointerInfo()));
    }
  }

  // Join the stores, which are independent of one another.
  if (!MemOpChains.empty())
    Chain = DAG.getNode(ISD::TokenFactor, DL, MVT::Other, MemOpChains);

  SDValue Glue;

  // Build a sequence of copy-to-reg nodes, chained and glued together.
  for (auto &Reg : RegsToPass) {
    Chain = DAG.getCopyToReg(Chain, DL, Reg.first, Reg.second, Glue);
    Glue = Chain.getValue(1);
  }

  // If the callee is a GlobalAddress/ExternalSymbol node, turn it into a
  // TargetGlobalAddress/TargetExternalSymbol node so that legalize won't
  // split it and then direct call can be matched by PseudoCALL.
  if (GlobalAddressSDNode *S = dyn_cast<GlobalAddressSDNode>(Callee)) {
    const GlobalValue *GV = S->getGlobal();
    unsigned OpFlags =
        getTargetMachine().shouldAssumeDSOLocal(*GV->getParent(), GV)
            ? AccaII::MO_CALL
            : AccaII::MO_CALL_PLT;
    Callee = DAG.getTargetGlobalAddress(S->getGlobal(), DL, PtrVT, 0, OpFlags);
  } else if (ExternalSymbolSDNode *S = dyn_cast<ExternalSymbolSDNode>(Callee)) {
    unsigned OpFlags = getTargetMachine().shouldAssumeDSOLocal(
                           *MF.getFunction().getParent(), nullptr)
                           ? AccaII::MO_CALL
                           : AccaII::MO_CALL_PLT;
    Callee = DAG.getTargetExternalSymbol(S->getSymbol(), PtrVT, OpFlags);
  }

  // The first call operand is the chain and the second is the target address.
  SmallVector<SDValue> Ops;
  Ops.push_back(Chain);
  Ops.push_back(Callee);

  // Add argument registers to the end of the list so that they are
  // known live into the call.
  for (auto &Reg : RegsToPass)
    Ops.push_back(DAG.getRegister(Reg.first, Reg.second.getValueType()));

  if (!IsTailCall) {
    // Add a register mask operand representing the call-preserved registers.
    const TargetRegisterInfo *TRI = Subtarget->getRegisterInfo();
    const uint32_t *Mask = TRI->getCallPreservedMask(MF, CallConv);
    assert(Mask && "Missing call preserved mask for calling convention");
    Ops.push_back(DAG.getRegisterMask(Mask));
  }

  // Glue the call to the argument copies, if any.
  if (Glue.getNode())
    Ops.push_back(Glue);

  // Emit the call.
  SDVTList NodeTys = DAG.getVTList(MVT::Other, MVT::Glue);

  if (IsTailCall) {
    MF.getFrameInfo().setHasTailCall();
    return DAG.getNode(AccaISD::TAIL, DL, NodeTys, Ops);
  }

  Chain = DAG.getNode(AccaISD::CALL, DL, NodeTys, Ops);
  DAG.addNoMergeSiteInfo(Chain.getNode(), CLI.NoMerge);
  Glue = Chain.getValue(1);

  // Mark the end of the call, which is glued to the call itself.
  Chain = DAG.getCALLSEQ_END(Chain, NumBytes, 0, Glue, DL);
  Glue = Chain.getValue(1);

  // Assign locations to each value returned by this call.
  SmallVector<CCValAssign> RVLocs;
  CCState RetCCInfo(CallConv, IsVarArg, MF, RVLocs, *DAG.getContext());
  RetCCInfo.AnalyzeCallResult(Ins, CC_Acca);

  // Copy all of the result registers out of their specified physreg.
  for (auto &VA : RVLocs) {
    // Copy the value out.
    SDValue RetValue =
        DAG.getCopyFromReg(Chain, DL, VA.getLocReg(), VA.getLocVT(), Glue);
    // Glue the RetValue to the end of the call sequence.
    Chain = RetValue.getValue(1);
    Glue = RetValue.getValue(2);

    RetValue = convertLocVTToValVT(DAG, RetValue, VA, DL, *Subtarget);

    InVals.push_back(RetValue);
  }

  return Chain;
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

EVT AccaTargetLowering::
getSetCCResultType(const DataLayout &DL, LLVMContext &Context, EVT VT) const {
  if (VT.isVector())
    llvm_unreachable("Acca doesn't support vector types yet");

  // VT may be Other, used to find the "canonical" SetCC type.
  // Acca supports any size boolean type for conditions, so arbitrarily choose
  // i64; if you'd like a reason: match Acca's register width?
  if (!VT.isScalarInteger())
    return MVT::i64;

  // Acca's SETCC equivalent returns a result of the same type as its operands
  return VT;
};
