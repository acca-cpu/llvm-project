add_llvm_component_library(LLVMAccaDisassembler
  AccaDisassembler.cpp

  LINK_COMPONENTS
  AccaDesc
  AccaInfo
  MC
  MCDisassembler
  Support

  ADD_TO_COMPONENT
  Acca
  )
