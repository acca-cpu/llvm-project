//===-- AccaTargetMachine.cpp - Define TargetMachine for Acca -----------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
//
//===----------------------------------------------------------------------===//

#include "AccaTargetMachine.h"
#include "Acca.h"
#include "AccaMachineFunctionInfo.h"
#include "AccaTargetObjectFile.h"
#include "TargetInfo/AccaTargetInfo.h"
#include "llvm/CodeGen/Passes.h"
#include "llvm/CodeGen/TargetPassConfig.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/MC/TargetRegistry.h"

using namespace llvm;

extern "C" LLVM_EXTERNAL_VISIBILITY void LLVMInitializeAccaTarget() {
  // Register the target.
  RegisterTargetMachine<AccaTargetMachine> X(getTheAccaTarget());

  PassRegistry &PR = *PassRegistry::getPassRegistry();
  initializeAccaDAGToDAGISelPass(PR);
}

// * little-endian
// * stack aligned to 16 bytes (128 bits)
// * 64-bit pointers
// * 64-bit aligned 64-bit integers (and all other integers are naturally aligned as well)
// * Mach-O symbol mangling
// * CPU supports 8-, 16-, 32-, and 64-bit arithmetic efficiently
static constexpr const char* dataLayoutString = "e-S128-p:64:64-i64:64-m:o-n8:16:32:64";

AccaTargetMachine::AccaTargetMachine(const Target &T, const Triple &TT,
                                     StringRef CPU, StringRef FS,
                                     const TargetOptions &Options,
                                     std::optional<Reloc::Model> RM,
                                     std::optional<CodeModel::Model> CM,
                                     CodeGenOpt::Level OL, bool JIT)
    : LLVMTargetMachine(T, dataLayoutString, TT, CPU, FS, Options,
                        RM.value_or(Reloc::Static),
                        CM.value_or(CodeModel::Small), OL),
      TLOF(std::make_unique<AccaELFTargetObjectFile>()),
      Subtarget(TT, std::string(CPU), std::string(FS), *this)
     {
  initAsmInfo();
}

AccaTargetMachine::~AccaTargetMachine() = default;

const AccaSubtarget *
AccaTargetMachine::getSubtargetImpl(const Function &F) const {
  Attribute CPUAttr = F.getFnAttribute("target-cpu");
  Attribute FSAttr = F.getFnAttribute("target-features");

  std::string CPU =
      CPUAttr.isValid() ? CPUAttr.getValueAsString().str() : TargetCPU;
  std::string FS =
      FSAttr.isValid() ? FSAttr.getValueAsString().str() : TargetFS;

  // FIXME: This is related to the code below to reset the target options,
  // we need to know whether or not the soft float flag is set on the
  // function, so we can enable it as a subtarget feature.
  bool softFloat = F.getFnAttribute("use-soft-float").getValueAsBool();

  if (softFloat)
    FS += FS.empty() ? "+soft-float" : ",+soft-float";

  auto &I = SubtargetMap[CPU + FS];
  if (!I) {
    // This needs to be done before we create a new subtarget since any
    // creation will depend on the TM and the code generation flags on the
    // function that reside in TargetOptions.
    resetTargetOptions(F);
    I = std::make_unique<AccaSubtarget>(TargetTriple, CPU, FS, *this);
  }
  return I.get();
}

namespace {
/// Acca Code Generator Pass Configuration Options.
class AccaPassConfig : public TargetPassConfig {
public:
  AccaPassConfig(AccaTargetMachine &TM, PassManagerBase &PM)
    : TargetPassConfig(TM, PM) {}

  AccaTargetMachine &getAccaTargetMachine() const {
    return getTM<AccaTargetMachine>();
  }

  void addIRPasses() override;
  bool addInstSelector() override;
  void addPreEmitPass() override;
};
} // namespace

TargetPassConfig *AccaTargetMachine::createPassConfig(PassManagerBase &PM) {
  return new AccaPassConfig(*this, PM);
};

void AccaPassConfig::addIRPasses() {
  addPass(createAtomicExpandPass());

  TargetPassConfig::addIRPasses();
}

bool AccaPassConfig::addInstSelector() {
  addPass(createAccaISelDag(getAccaTargetMachine()));
  return false;
}

void AccaPassConfig::addPreEmitPass() {
  // TODO
}

MachineFunctionInfo *
AccaTargetMachine::
createMachineFunctionInfo(BumpPtrAllocator &Allocator, const Function &F,
                          const TargetSubtargetInfo *STI) const {
  return AccaMachineFunctionInfo::create<AccaMachineFunctionInfo>(Allocator,
                                                                    F, STI);
};
