add_llvm_component_library(LLVMAccaAsmParser
  AccaAsmParser.cpp

  LINK_COMPONENTS
  AccaDesc
  AccaInfo
  MC
  MCParser
  Support

  ADD_TO_COMPONENT
  Acca
  )
