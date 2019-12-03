#ifndef TAUC_LLVM_BACKEND_HPP
#define TAUC_LLVM_BACKEND_HPP
extern "C" {
#include "llvm_backend_api.h"
#include "thread_context.h"
}
#include "llvm/ExecutionEngine/Orc/CompileOnDemandLayer.h"
#include "llvm/ExecutionEngine/Orc/CompileUtils.h"
#include "llvm/ExecutionEngine/Orc/Core.h"
#include "llvm/ExecutionEngine/Orc/ExecutionUtils.h"
#include "llvm/ExecutionEngine/Orc/IRCompileLayer.h"
#include "llvm/ExecutionEngine/Orc/IRTransformLayer.h"
#include "llvm/ExecutionEngine/Orc/JITTargetMachineBuilder.h"
#include "llvm/ExecutionEngine/Orc/LLJIT.h"
#include "llvm/ExecutionEngine/Orc/ObjectTransformLayer.h"
#include "llvm/ExecutionEngine/Orc/OrcError.h"
#include "llvm/ExecutionEngine/Orc/RTDyldObjectLinkingLayer.h"
#include "llvm/ExecutionEngine/Orc/ThreadSafeModule.h"
#include "llvm/ExecutionEngine/SectionMemoryManager.h"
#include "llvm/IR/Mangler.h"
#include <lld/Common/Driver.h>
#include <llvm/Analysis/CGSCCPassManager.h>
#include <llvm/Analysis/TargetLibraryInfo.h>
#include <llvm/Analysis/TargetTransformInfo.h>
#include <llvm/Bitcode/BitcodeWriter.h>
#include <llvm/ExecutionEngine/Orc/LLJIT.h>
#include <llvm/IR/AssemblyAnnotationWriter.h>
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/DIBuilder.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/DiagnosticInfo.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/InlineAsm.h>
#include <llvm/IR/InstrTypes.h>
#include <llvm/IR/Instruction.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/PassManager.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/Verifier.h>
#include <llvm/InitializePasses.h>
#include <llvm/MC/MCRegisterInfo.h>
#include <llvm/MC/SubtargetFeature.h>
#include <llvm/Object/Archive.h>
#include <llvm/Object/ArchiveWriter.h>
#include <llvm/PassRegistry.h>
#include <llvm/Passes/PassBuilder.h>
#include <llvm/Support/FileSystem.h>
#include <llvm/Support/TargetParser.h>
#include <llvm/Support/SmallVectorMemoryBuffer.h>
#include <llvm/Support/BinaryByteStream.h>
#include <llvm/Support/TargetRegistry.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/Target/CodeGenCWrappers.h>
#include <llvm/Target/TargetMachine.h>
#include <llvm/Transforms/IPO.h>
#include <llvm/Transforms/IPO/AlwaysInliner.h>
#include <llvm/Transforms/IPO/PassManagerBuilder.h>
#include <llvm/Transforms/Scalar.h>
#include <llvm/Transforms/Utils.h>
#include <llvm/ADT/STLExtras.h>
#include <unistd.h>

struct PPRunner {
    llvm::orc::ExecutionSession exec_session;
    llvm::orc::RTDyldObjectLinkingLayer obj_link_layer;
    PPRunner();
    ~PPRunner();
};

struct LLVMModule {
    std::string module_str;
    std::string module_obj;
};

struct ControlFlowContext {
    llvm::Value* value;
    ureg value_align;
    llvm::BasicBlock* first_block;
    llvm::BasicBlock* following_block;
    bool continues_afterwards;
};
enum ValueState : char {
    NOT_GENERATED = 0,
    PP_IMPL_ADDED,
    PP_IMPL_DESTROYED,
    PP_STUB_GENERATED,
    PP_STUB_ADDED,
    IMPL_ADDED,
    IMPL_DESTROYED,
    STUB_GENERATED,
    STUB_ADDED,
};

llvm_error processEscapeSymbols(char** str_ptr);

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
    std::vector<ValueState> _global_value_state;
    std::vector<void*> _local_value_store;
    std::vector<ValueState> _local_value_state;
    std::vector<ureg> _reset_after_emit;
    std::vector<ureg> _globals_not_to_free;
    // we have to avoid pointer invalidation on resize, therefore deque
    std::deque<ControlFlowContext> _control_flow_ctx;
    llvm::Type* _primitive_types[PRIMITIVE_COUNT];
    llvm::Module* _module;
    llvm::TargetMachine* _target_machine;
    llvm::DataLayout* _data_layout;
    llvm::Function* _curr_fn;
    llvm::Constant* _paste_func_ptr;
    ControlFlowContext* _curr_fn_control_flow_ctx;
    sc_func* _curr_fn_ast_node;
    ureg _mod_startid;
    ureg _mod_endid;
    mdg_node** _mods_start;
    mdg_node** _mods_end;
    ureg _private_sym_count;
    LLVMModule* _mod_handle;
    llvm::orc::JITDylib* _mod_dylib;
    std::atomic<ureg> _pp_count;
    bool _pp_mode;

  public:
    LLVMBackend(thread_context* tc);
    ~LLVMBackend();
    static int Initialize(LLVMBackend* llvmb, thread_context* tc);
    static void Finalize(LLVMBackend* llvmb);
    llvm_error setup();

  public:
    llvm_error
    initModule(mdg_node** start, mdg_node** end, LLVMModule** module);
    void remapLocalID(ureg old_id, ureg new_id);
    llvm_error genPPRN(pp_resolve_node* pprn);
    void prepareForPPRNResult(pp_resolve_node* pprn);
    llvm_error genPPFunc(const std::string& func_name, ptrlist* resolve_nodes);
    llvm_error runPP(ureg private_sym_count, ptrlist* resolve_nodes);
    llvm_error reserveSymbols(ureg priv_sym_limit, ureg pub_sym_limit);
    llvm_error emit(ureg startid, ureg endid, ureg priv_sym_count);
    pp_resolve_node* lookupPPResolveNode(ureg id);
    void setPPResolveNode(ureg id, pp_resolve_node* pprn);
    void buildPasteHelpers();
    void resetAfterEmit();

  private:
    void addPrimitives();

  private:
    bool isIDInModule(ureg id);
    bool isGlobalIDInModule(ureg id);
    static bool isLocalID(ureg id);
    static bool isGlobalID(ureg id);
    static bool isPPSymbolGlobal(symbol* sym);

  private:
    ControlFlowContext* getTartetCFC(ast_node* target);

    void** lookupAstElem(ureg id);

    ValueState* lookupValueState(ureg id);

    llvm::Type** lookupTypeRaw(ureg id);

    llvm_error buildConstant(ast_elem* ctype, void* data, llvm::Constant** res);

    llvm_error
    buildPrimitiveConstant(primitive_kind pk, void* data, llvm::Constant** res);

    llvm_error
    lookupCType(ast_elem* e, llvm::Type** t, ureg* align, ureg* size);

    llvm_error getFollowingBlock(llvm::BasicBlock** following_block);

  private:
    llvm_error genModules();

    llvm_error
    genAstBody(ast_body* n, bool continues_after, bool* end_reachable = NULL);

    llvm_error genIfBranch(ast_node* branch);

    llvm_error genScopeValue(ast_elem* ctype, ControlFlowContext& ctx);

    llvm_error
    genAstNode(ast_node* n, llvm::Value** vl, llvm::Value** vl_loaded);

    llvm_error genFunction(sc_func* fn, llvm::Value** llfn);

    llvm_error
    genVariable(ast_node* n, llvm::Value** vl, llvm::Value** vl_loaded);

    llvm_error
    genBinaryOp(expr_op_binary* b, llvm::Value** vl, llvm::Value** vl_loaded);

    llvm_error
    genUnaryOp(expr_op_unary* u, llvm::Value** vl, llvm::Value** vl_loaded);

  private:
    llvm_error emitModule();

    llvm_error emitModuleToStream(
        llvm::TargetLibraryInfoImpl* tlii, llvm::raw_pwrite_stream* stream,
        bool emit_asm);

    llvm_error
    emitModuleToFile(llvm::TargetLibraryInfoImpl* tlii, bool emit_asm);

    llvm_error
    emitModuleToPP(llvm::TargetLibraryInfoImpl* tlii, bool write_out_file);

    llvm_error emitModuleIR();
};

llvm_error
linkLLVMModules(LLVMModule** start, LLVMModule** end, char* output_path);

llvm_error removeObjs(LLVMModule** start, LLVMModule** end);
#endif
