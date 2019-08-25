#ifndef TAUC_LLVM_BACKEND_API_H
#define TAUC_LLVM_BACKEND_API_H

#ifdef __cplusplus
extern "C" {
#endif

typedef void llvm_backend;
typedef void llvm_module;

typedef struct mdg_node_s mdg_node;
typedef struct thread_context_s thread_context;

typedef enum llvm_backend_error {
    LLVMBE_OK = 0,
    LLVMBE_FATAL,
} llvm_backend_error;

llvm_backend* llvm_backend_new(thread_context* tc);
void llvm_backend_delete(llvm_backend* l);
llvm_backend_error
llvm_backend_emit_module(mdg_node** start, mdg_node** end, llvm_module** mod);
const char* llvm_module_get_obj_path(llvm_module* mod);

#ifdef __cplusplus
}
#endif
#endif