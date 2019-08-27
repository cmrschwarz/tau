#include "llvm_backend.hpp"
extern "C" {
#include "thread_context.h"
#include "utils/pool.h"
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
    : _tc(tc), _context(), _builder(_context)
{
}

int LLVMBackend::InitLLVMBackend(LLVMBackend* llvmb, thread_context* tc)
{
    void* res = new (llvmb) LLVMBackend(tc); // placement new is noexcept
    if (!res) return ERR;
    return OK;
}
void LLVMBackend::FinLLVMBackend(LLVMBackend* llvmb)
{
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
    _mod = new (std::nothrow) llvm::Module(m->module_str, _context);
    if (!_mod) return LLVMBE_FATAL;
    genModulesIR(start, end);
    return _err;
}

void LLVMBackend::genModulesIR(mdg_node** start, mdg_node** end)
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
void LLVMBackend::genMdgNodeIR(ast_node* n)
{
    // TODO
    return;
}
llvm::Function* genFunctionIR(sc_func* fn);
llvm::Value* getExprIR(ast_node* n);

llvm_backend_error
linkLLVMModules(LLVMModule** start, LLVMModule** end, char* output_path)
{
    return LLVMBE_OK;
}