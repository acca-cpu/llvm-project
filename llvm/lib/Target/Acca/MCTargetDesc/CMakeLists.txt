add_llvm_component_library(LLVMAccaDesc
  AccaAsmBackend.cpp
  AccaMCAsmInfo.cpp
  AccaMCTargetDesc.cpp
  AccaTargetStreamer.cpp
  AccaMCCodeEmitter.cpp
  AccaELFObjectWriter.cpp
  AccaInstPrinter.cpp
  AccaMCExpr.cpp
  AccaBaseInfo.cpp

  LINK_COMPONENTS
  MC
  AccaInfo
  Support
  TargetParser

  ADD_TO_COMPONENT
  Acca
  )
