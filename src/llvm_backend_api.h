#ifndef TAUC_LLVM_BACKEND_API_H
#define TAUC_LLVM_BACKEND_API_H

#ifdef __cplusplus
extern "C" {
#endif
#include "ast.h"
#include "utils/types.h"
#define PRIV_SYMBOL_OFFSET (UREG_MAX / 2)
typedef void llvm_backend;
typedef void llvm_module;

typedef struct mdg_node_s mdg_node;
typedef struct pp_resolve_node_s pp_resolve_node;
typedef struct thread_context_s thread_context;

typedef enum llvm_error_e {
    LLE_OK = 0,
    LLE_FATAL,
} llvm_error;

int llvm_backend_init_globals();
void llvm_backend_fin_globals();

llvm_backend* llvm_backend_new(thread_context* tc);

pp_resolve_node**
llvm_backend_lookup_pp_resolve_node(llvm_backend* llvmb, ureg id);

void llvm_backend_delete(llvm_backend* llvmb);

llvm_error llvm_backend_init_module(
    llvm_backend* llvmb, mdg_node** start, mdg_node** end, llvm_module** mod);

llvm_error llvm_backend_reserve_symbols(
    llvm_backend* llvmb, ureg private_sym_count, ureg public_sym_count);

llvm_error llvm_backend_run_pp(
    llvm_backend* llvmb, ureg private_sym_count, expr_pp* pp_expr);

void llvm_backend_remap_local_id(llvm_backend* llvmb, ureg old_id, ureg new_id);

llvm_error llvm_backend_emit_module(
    llvm_backend* llvmb, ureg startid, ureg endid, ureg priv_sym_count);

void llvm_free_module(llvm_module* mod);

int llvm_link_modules(
    llvm_module** start, llvm_module** end, char* output_path);

int llvm_delete_objs(llvm_module** start, llvm_module** end);

int llvm_initialize_primitive_information();

#ifdef __cplusplus
}
#endif
#endif
