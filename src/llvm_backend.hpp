#ifndef TAUC_LLVM_BACKEND_HPP
#define TAUC_LLVM_BACKEND_HPP
extern "C" {
#include "llvm_backend_api.h"
#include "thread_context.h"
}
#include <lld/Common/Driver.h>
#include <llvm/Analysis/TargetLibraryInfo.h>
#include <llvm/Analysis/TargetTransformInfo.h>
#include <llvm/Bitcode/BitcodeWriter.h>
#include <llvm/InitializePasses.h>
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/DiagnosticInfo.h>
#include <llvm/IR/DIBuilder.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/InlineAsm.h>
#include <llvm/IR/InstrTypes.h>
#include <llvm/IR/Instruction.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/PassManager.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/Verifier.h>
#include <llvm/MC/MCRegisterInfo.h>
#include <llvm/MC/SubtargetFeature.h>
#include <llvm/Object/Archive.h>
#include <llvm/Object/ArchiveWriter.h>
#include <llvm/PassRegistry.h>
#include <llvm/Support/FileSystem.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/Support/TargetParser.h>
#include <llvm/Support/TargetRegistry.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/Target/CodeGenCWrappers.h>
#include <llvm/Target/TargetMachine.h>
#include <llvm/Transforms/IPO/AlwaysInliner.h>
#include <llvm/Transforms/IPO.h>
#include <llvm/Transforms/IPO/PassManagerBuilder.h>
#include <llvm/Analysis/CGSCCPassManager.h>
#include <llvm/Passes/PassBuilder.h>
#include <llvm/IR/AssemblyAnnotationWriter.h>
#include <llvm/Transforms/Scalar.h>
#include <llvm/Transforms/Utils.h>
#include <unistd.h>

struct LLVMModule {
    std::string name;
    std::string module_str;
};

struct ControlFlowContext {
    llvm::Value* value;
    ureg value_align;
    llvm::BasicBlock* first_block;
    llvm::BasicBlock* following_block;
    bool continues_afterwards;
};

struct LLVMBackend {

  private:
    thread_context* _tc;
    llvm::LLVMContext _context;
    llvm::IRBuilder<> _builder;
    // must be void* because there is no common ancestor between
    // llvm::Value and llvm::Type
    // TOOD: put primitives in different data structure so these can be
    // values
    std::vector<void*> _global_value_store;
    std::vector<bool> _global_value_init_flags;
    std::vector<void*> _local_value_store;
    std::vector<ureg> _reset_after_emit;
    // we have to avoid pointer invalidation on resize, therefore deque
    std::deque<ControlFlowContext> _control_flow_ctx;
    llvm::Type* _primitive_types[PRIMITIVE_COUNT];
    llvm::Module* _module;
    llvm::TargetMachine* _target_machine;
    llvm::DataLayout* _data_layout;
    llvm::Function* _curr_fn;
    ureg _mod_startid;
    ureg _mod_endid;
    ureg _private_sym_count;

  public:
    LLVMBackend(thread_context* tc);
    ~LLVMBackend();
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
    llvm_error addAstBodyIR(ast_body* n, bool continues_after);
    ControlFlowContext& getTartetCFC(ast_node* target);
    llvm_error addIfBranch(ast_node* branch);

  private:
    bool isIDInModule(ureg id);
    bool isGlobalIDInModule(ureg id);
    static bool isLocalID(ureg id);
    static bool isGlobalID(ureg id);

  private:
    void addPrimitives();
    void** lookupAstElem(ureg id);
    llvm::Value** lookupVariableRaw(ureg id);
    llvm::Function** lookupFunctionRaw(ureg id);
    llvm::Type** lookupTypeRaw(ureg id);
    llvm_error lookupCType(ast_elem* e, llvm::Type** t, ureg* align);
    llvm_error createScopeValue(ast_elem* ctype, ControlFlowContext& ctx);

    // val can be NULL
    llvm_error getAstNodeIR(ast_node* n, bool load, llvm::Value** vl);
    llvm_error genFunctionIR(sc_func* fn, llvm::Function** llfn);
    llvm_error genBinaryOpIR(expr_op_binary* b, llvm::Value** vl);

  private:
    llvm_error emitModule(const std::string& obj_name);
    llvm_error emitModuleIR(const std::string& ll_name);
};

llvm_error
linkLLVMModules(LLVMModule** start, LLVMModule** end, char* output_path);

llvm_error removeObjs(LLVMModule** start, LLVMModule** end);
#endif
