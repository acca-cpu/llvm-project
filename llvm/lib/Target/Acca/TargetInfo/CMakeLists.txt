add_llvm_component_library(LLVMAccaInfo
  AccaTargetInfo.cpp

  LINK_COMPONENTS
  MC
  Support

  ADD_TO_COMPONENT
  Acca
  )
