add_llvm_component_group(Acca)

set(LLVM_TARGET_DEFINITIONS Acca.td)

tablegen(LLVM AccaGenAsmMatcher.inc -gen-asm-matcher)
tablegen(LLVM AccaGenAsmWriter.inc -gen-asm-writer)
tablegen(LLVM AccaGenCallingConv.inc -gen-callingconv)
tablegen(LLVM AccaGenDAGISel.inc -gen-dag-isel)
tablegen(LLVM AccaGenDisassemblerTables.inc -gen-disassembler)
tablegen(LLVM AccaGenInstrInfo.inc -gen-instr-info)
tablegen(LLVM AccaGenMCCodeEmitter.inc -gen-emitter)
tablegen(LLVM AccaGenMCPseudoLowering.inc -gen-pseudo-lowering)
tablegen(LLVM AccaGenRegisterInfo.inc -gen-register-info)
tablegen(LLVM AccaGenSubtargetInfo.inc -gen-subtarget)
tablegen(LLVM AccaGenSystemOperands.inc -gen-searchable-tables)

add_public_tablegen_target(AccaCommonTableGen)

add_llvm_target(AccaCodeGen
  AccaAsmPrinter.cpp
  AccaInstrInfo.cpp
  AccaISelDAGToDAG.cpp
  AccaISelLowering.cpp
  AccaFrameLowering.cpp
  AccaMachineFunctionInfo.cpp
  AccaRegisterInfo.cpp
  AccaSubtarget.cpp
  AccaTargetMachine.cpp
  AccaMCInstLower.cpp
  AccaTargetObjectFile.cpp
  AccaBaseInfo.cpp
  AccaExpandPseudoInsts.cpp

  LINK_COMPONENTS
  AsmPrinter
  CodeGen
  Core
  MC
  SelectionDAG
  AccaDesc
  AccaInfo
  Support
  Target
  TargetParser

  ADD_TO_COMPONENT
  Acca
  )

add_subdirectory(Disassembler)
add_subdirectory(MCTargetDesc)
add_subdirectory(TargetInfo)
