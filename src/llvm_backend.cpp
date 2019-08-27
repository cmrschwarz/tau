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
    ureg endid, llvm_module** mod)
{
    return ((LLVMBackend*)llvmb)
        ->createLLVMModule(start, end, startid, endid, (LLVMModule**)mod);
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

int LLVMBackend::InitLLVMBackend(LLVMBackend* llvmb, thread_context* tc)
{
    llvmb->tc = tc;
    return OK;
}
void LLVMBackend::FinLLVMBackend(LLVMBackend* llvmb)
{
}

llvm_backend_error LLVMBackend::createLLVMModule(
    mdg_node** start, mdg_node** end, ureg startid, ureg endid,
    LLVMModule** module)
{
    this->mod_startid = startid;
    this->mod_endid = endid;
    LLVMModule* m = new LLVMModule();
    printf("generating {");
    for (mdg_node** n = start; n != end; n++) {
        m->module_str += (**n).name;
        fputs((**n).name, stdout);
        if (n + 1 != end) {
            fputs(", ", stdout);
            m->module_str += ", ";
        }
    }
    puts("}");
    *module = m;
    return LLVMBE_OK;
}
llvm_backend_error
linkLLVMModules(LLVMModule** start, LLVMModule** end, char* output_path)
{
    return LLVMBE_OK;
}