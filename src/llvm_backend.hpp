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
#include <llvm/MC/MCRegisterInfo.h>
#include <llvm/IR/IRBuilder.h>
#include <unistd.h>

struct LLVMModule {
    std::string name;
    std::string module_str;
};

struct LLVMBackend {
  private:
    thread_context* _tc;
    llvm::LLVMContext _context;
    llvm::IRBuilder<> _builder;
    std::vector<void*> _global_value_store;
    std::vector<void*> _local_value_store;
    std::vector<void**> _null_after_emit;
    llvm::Module* _mod;
    llvm::TargetMachine* _tm;
    ureg _reg_size;
    ureg _mod_startid;
    ureg _mod_endid;
    ureg _private_sym_count;

  public:
    LLVMBackend(thread_context* tc);
    static int InitLLVMBackend(LLVMBackend* llvmb, thread_context* tc);
    static void FinLLVMBackend(LLVMBackend* llvmb);

  public:
    llvm_error setup();

  public:
    llvm_error createLLVMModule(
        mdg_node** start, mdg_node** end, ureg startid, ureg endid,
        ureg private_sym_count, LLVMModule** module);

  private:
    llvm_error addModulesIR(mdg_node** start, mdg_node** end);
    llvm_error addAstBodyIR(ast_body* n);

  private:
    bool isIdInModule(ureg id);

  private:
    void addPrimitive(ureg id, primitive_kind pk);
    void** lookupAstElem(ureg id);
    void storeAstElem(ureg id, void* val);
    llvm_error lookupValue(ureg id, ast_node* n, llvm::Value** v);
    llvm_error lookupType(ureg id, ast_elem* e, llvm::Type** t);
    llvm_error lookupCType(ast_elem* e, llvm::Type** t);
    llvm::Type* lookupPrimitive(primitive_kind pk);
    // val can be NULL
    llvm_error getMdgNodeIR(ast_node* n, llvm::Value** v);
    llvm_error genFunctionIR(sc_func* fn, llvm::Value** val);
    llvm_error genBinaryOpIR(expr_op_binary* b, llvm::Value** v);

  private:
    llvm_error emitModule(const std::string& obj_name);
};

llvm_error
linkLLVMModules(LLVMModule** start, LLVMModule** end, char* output_path);
#endif