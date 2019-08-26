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
    llvm_backend* llvmb, mdg_node** start, mdg_node** end, llvm_module** mod)
{
    return ((LLVMBackend*)llvmb)
        ->createLLVMModule(start, end, (LLVMModule**)mod);
}

void llvm_free_module(llvm_module* mod)
{
}

int llvm_link_modules(llvm_module** start, llvm_module** end, char* output_path)
{
    printf("linking {");
    for (llvm_module** n = start; n != end; n++) {
        printf("%s%s", *(char**)n, (n + 1 == end) ? "" : ", ");
        tfree(*n);
    }
    puts("}");
    return OK;
}
}

int LLVMBackend::InitLLVMBackend(LLVMBackend* llvmb, thread_context* tc)
{
    return OK;
}
void LLVMBackend::FinLLVMBackend(LLVMBackend* llvmb)
{
}

llvm_backend_error LLVMBackend::createLLVMModule(
    mdg_node** start, mdg_node** end, LLVMModule** module)
{
    ureg strsize = 0;
    printf("generating {");
    for (mdg_node** n = start; n != end; n++) {
        printf("%s%s", (**n).name, (n + 1 == end) ? "" : ", ");
        strsize += strlen((**n).name + 2);
    }
    puts("}");
    *(char**)module = (char*)tmalloc(strsize - 1);
    char* i = (char*)*module;
    for (mdg_node** n = start; n != end; n++) {
        strcpy(i, (**n).name);
        i += strlen((**n).name);
        if (n + 1 != end) {
            *i = ',';
            i++;
            *i = ' ';
            i++;
        }
        else {
            *i = '\0';
        }
    }
    return LLVMBE_OK;
}