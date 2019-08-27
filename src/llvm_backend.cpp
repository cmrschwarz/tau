#include "llvm_backend.hpp"
extern "C" {
#include "thread_context.h"
#include "utils/pool.h"
#include "tauc.h"
int llvm_backend_init_globals()
{
    // InitializeAllTargetInfos();
    LLVMInitializeNativeTarget();
    // InitializeAllTargetMCs();
    llvm::InitializeNativeTargetAsmParser();
    llvm::InitializeNativeTargetAsmPrinter();
    return 0;
}
void llvm_backend_fin_globals()
{
    llvm::llvm_shutdown();
}

llvm_backend* llvm_backend_new(thread_context* tc)
{
    LLVMBackend* b =
        (LLVMBackend*)pool_alloc(&tc->permmem, sizeof(LLVMBackend));
    if (!b) return NULL;
    if (LLVMBackend::InitLLVMBackend(b, tc)) return NULL;
    return b;
}

void llvm_backend_delete(llvm_backend* llvmb)
{
    LLVMBackend::FinLLVMBackend((LLVMBackend*)llvmb);
}

llvm_backend_error llvm_backend_emit_module(
    llvm_backend* llvmb, mdg_node** start, mdg_node** end, ureg startid,
    ureg endid, ureg private_sym_count, llvm_module** mod)
{
    return ((LLVMBackend*)llvmb)
        ->createLLVMModule(
            start, end, startid, endid, private_sym_count, (LLVMModule**)mod);
}

void llvm_free_module(llvm_module* mod)
{
    delete (LLVMModule*)mod;
}
int llvm_link_modules(llvm_module** start, llvm_module** end, char* output_path)
{
    printf("linking {");
    for (LLVMModule** n = (LLVMModule**)start; !ptreq(n, end); n++) {
        printf("%s%s", (**n).module_str.c_str(), ptreq(n + 1, end) ? "" : ", ");
    }
    puts("}");
    llvm_backend_error be =
        linkLLVMModules((LLVMModule**)start, (LLVMModule**)end, output_path);
    if (be) return ERR;
    return OK;
}
}

// CPP
LLVMBackend::LLVMBackend(thread_context* tc)
    : _tc(tc), _context(), _builder(_context),
      _global_value_store(atomic_ureg_load(&TAUC.node_ids), NULL),
      _err(LLVMBE_OK)

{
    std::string err;
    auto target_triple = LLVMGetDefaultTargetTriple();
    auto target = llvm::TargetRegistry::lookupTarget(target_triple, err);
    if (err.length() > 0) { // TODO: properly
        llvm::errs() << err << "\n";
    }
    llvm::TargetOptions opt;
    auto tm = target->createTargetMachine(
        target_triple, LLVMGetHostCPUName(), LLVMGetHostCPUFeatures(), opt,
        llvm::Optional<llvm::Reloc::Model>());
    _reg_size = tm->getMCRegisterInfo()->getRegClass(0).RegsSize;
}

int LLVMBackend::InitLLVMBackend(LLVMBackend* llvmb, thread_context* tc)
{
    void* res = new (llvmb) LLVMBackend(tc); // placement new is noexcept
    if (!res) return ERR;
    if (llvmb->setup()) return ERR;
    return OK;
}
llvm_backend_error LLVMBackend::setup()
{
    for (ureg i = PRIMITIVES[0].type_id;
         i < PRIMITIVES[0].type_id + PRIMITIVE_COUNT; i++) {
        addPrimitive(PRIMITIVES[i].type_id, PRIMITIVES[i].sym.node.pt_kind);
        if (_err) return _err;
    }
    return LLVMBE_OK;
}
void LLVMBackend::FinLLVMBackend(LLVMBackend* llvmb)
{
}

void LLVMBackend::addPrimitive(ureg id, primitive_kind pk)
{
    llvm::Type* t;
    switch (pk) {
        case PT_INT:
        case PT_UINT: t = llvm::Type::getIntNTy(_context, _reg_size); break;
        case PT_BINARY_STRING:
        case PT_STRING: t = llvm::Type::getInt8PtrTy(_context); break;
        case PT_FLOAT: t = llvm::Type::getFloatTy(_context); break;
        case PT_VOID: t = llvm::Type::getVoidTy(_context); break;
        default: assert(false); return;
    }
    _global_value_store[id] = (void*)t;
}
llvm_backend_error LLVMBackend::createLLVMModule(
    mdg_node** start, mdg_node** end, ureg startid, ureg endid,
    ureg private_sym_count, LLVMModule** module)
{
    _err = LLVMBE_OK;
    // create our LLVMModule wrapper thingy for linking
    LLVMModule* m = new LLVMModule();
    printf("generating {");
    for (mdg_node** n = start; n != end; n++) {
        m->module_str += (**n).name;
        fputs((**n).name, stdout);
        if (n + 1 != end) {
            fputs(", ", stdout);
            m->module_str += "&";
        }
    }
    m->name = m->module_str;
    puts("}");
    *module = m;
    // init id space
    _mod_startid = startid;
    _mod_endid = endid;
    _private_sym_count = private_sym_count;
    if (_global_value_store.size() <= endid) {
        _global_value_store.resize(endid + 1, NULL);
    }
    // i really hope this doesn't realloc
    _local_value_store.assign(_private_sym_count, NULL);
    // create actual module
    _mod = new (std::nothrow) llvm::Module(m->name, _context);
    if (!_mod) return LLVMBE_FATAL;
    addModulesIR(start, end);
    if (_err) return _err;
    emitModule(m->name);
    return _err;
}

void LLVMBackend::addModulesIR(mdg_node** start, mdg_node** end)
{
    for (mdg_node** n = start; n != end; n++) {
        aseglist_iterator it;
        aseglist_iterator_begin(&it, &(**n).open_scopes);
        for (open_scope* osc = (open_scope*)aseglist_iterator_next(&it); osc;
             osc = (open_scope*)aseglist_iterator_next(&it)) {
            genMdgNodeIR((ast_node*)osc);
            if (_err) return;
        }
    }
}
void LLVMBackend::addAstBodyIR(ast_body* b)
{
    for (ast_node** n = b->elements; *n; n++) {
        genMdgNodeIR(*n);
        if (_err) return;
    }
}
llvm::Value* LLVMBackend::lookupMdgNodeIR(ureg id, ast_elem* e)
{
    llvm::Value** v;
    if (id >= UREGH_MAX) {
        v = (llvm::Value**)&_local_value_store[id - UREGH_MAX];
    }
    else {
        v = (llvm::Value**)&_global_value_store[id];
    }
    if (*v) return *v;
    // all eleements which are non nodes are primitives
    // those are initilaized on backend setup
    // therefore this cast is fine
    return genMdgNodeIR((ast_node*)e);
}
llvm::Value* LLVMBackend::genMdgNodeIR(ast_node* n)
{
    switch (n->kind) {
        case OSC_MODULE:
        case OSC_EXTEND: addAstBodyIR(&((open_scope*)n)->scp.body); return NULL;
        case SC_FUNC: return genFunctionIR((sc_func*)n);
        case EXPR_OP_BINARY: {
            return NULL;
        }
        default: break;
    }
}

llvm::Function* LLVMBackend::genFunctionIR(sc_func* fn)
{
}

void LLVMBackend::emitModule(const std::string& obj_name)
{
    // TODO
    return;
}
llvm_backend_error
linkLLVMModules(LLVMModule** start, LLVMModule** end, char* output_path)
{
    return LLVMBE_OK;
}