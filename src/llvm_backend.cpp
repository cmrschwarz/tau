#include "llvm_backend.hpp"
#include "thread_context.h"
extern "C" {
llvm_backend* llvm_backend_new(thread_context* tc)
{
    return 1;
}

void llvm_backend_delete(llvm_backend* l)
{
}

llvm_backend_error
llvm_backend_emit_module(mdg_node** start, mdg_node** end, llvm_module** mod)
{
    return LLVMBE_OK;
}

const char* llvm_module_get_obj_path(llvm_module* mod)
{
    return "";
}
}
