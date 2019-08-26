#ifndef TAUC_LLVM_BACKEND_HPP
#define TAUC_LLVM_BACKEND_HPP
extern "C" {
#include "llvm_backend_api.h"
#include "thread_context.h"
}
#include <llvm/Bitcode/BitcodeWriter.h>
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/InstrTypes.h>
#include <llvm/IR/Instruction.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Type.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/Support/FileSystem.h>
#include <llvm/Support/TargetRegistry.h>
#include <llvm/Target/TargetMachine.h>
#include <llvm/Target/CodeGenCWrappers.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/IR/LegacyPassManager.h>
#include <lld/Common/Driver.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/IRBuilder.h>
#include <unistd.h>

struct LLVMModule {
};

struct LLVMBackend {
  private:
    thread_context* tc;
    llvm::LLVMContext context;

  public:
    static int InitLLVMBackend(LLVMBackend* llvmb, thread_context* tc);
    static void FinLLVMBackend(LLVMBackend* llvmb);

  public:
    llvm_backend_error
    createLLVMModule(mdg_node** start, mdg_node** end, LLVMModule** module);
};

#endif