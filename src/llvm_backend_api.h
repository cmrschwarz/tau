#ifndef TAUC_LLVM_BACKEND_API_H
#define TAUC_LLVM_BACKEND_API_H

#ifdef __cplusplus
extern "C" {
#endif
#include "utils/types.h"
typedef void llvm_backend;
typedef void llvm_module;

typedef struct mdg_node_s mdg_node;
typedef struct thread_context_s thread_context;

typedef enum llvm_backend_error_e {
    LLVMBE_OK = 0,
    LLVMBE_FATAL,
} llvm_backend_error;

int llvm_backend_init_globals();
void llvm_backend_fin_globals();

llvm_backend* llvm_backend_new(thread_context* tc);

void llvm_backend_delete(llvm_backend* llvmb);

llvm_backend_error llvm_backend_emit_module(
    llvm_backend* llvmb, mdg_node** start, mdg_node** end, ureg startid,
    ureg endid, ureg private_sym_count, llvm_module** mod);

void llvm_free_module(llvm_module* mod);

int llvm_link_modules(
    llvm_module** start, llvm_module** end, char* output_path);

#ifdef __cplusplus
}
#endif
#endif