#pragma once

#ifdef __cplusplus
extern "C" {
#endif
#include "ast.h"
#include "utils/types.h"
#include "utils/aseglist.h"
#define PRIV_SYMBOL_OFFSET (UREG_MAX / 2)
typedef void llvm_backend;
typedef void llvm_module;
typedef struct tauc_s tauc;

typedef struct mdg_node_s mdg_node;
typedef struct pp_resolve_node_s pp_resolve_node;
typedef struct thread_context_s thread_context;
typedef struct sbuffer_s ptrlist;

typedef enum llvm_error_e {
    LLE_OK = 0,
    LLE_ERROR,
    LLE_FATAL,
} llvm_error;

int llvm_backend_init_globals(tauc* t);
void llvm_backend_fin_globals();

llvm_backend* llvm_backend_new(thread_context* tc);

void llvm_backend_delete(llvm_backend* llvmb);

llvm_error llvm_backend_init_module(
    llvm_backend* llvmb, mdg_node** start, mdg_node** end, llvm_module** mod);

llvm_error llvm_backend_reserve_symbols(
    llvm_backend* llvmb, ureg private_sym_count, ureg public_sym_count);

llvm_error llvm_backend_run_pp(
    llvm_backend* llvmb, ureg private_sym_count, ptrlist* resolve_nodes);

llvm_error llvm_backend_link_for_pp(bool is_dynamic, char* path);

void llvm_backend_remap_local_id(llvm_backend* llvmb, ureg old_id, ureg new_id);

llvm_error llvm_backend_emit_module(
    llvm_backend* llvmb, ureg startid, ureg endid, ureg priv_sym_count);

void llvm_free_module(llvm_module* mod);

int llvm_link_modules(
    thread_context* tc, llvm_module** start, llvm_module** end,
    ptrlist* link_libs, const char* output_path);

llvm_error llvm_backend_generate_entrypoint(
    llvm_backend* llvmb, sc_func* mainfn, sc_func* startfn, aseglist* ctors,
    aseglist* dtors, ureg startid, ureg endid, ureg private_sym_count);

int llvm_delete_objs(llvm_module** start, llvm_module** end);

int llvm_initialize_primitive_information();

#ifdef __cplusplus
}
#endif
