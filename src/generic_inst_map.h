#pragma once
#include "utils/types.h"
#include "utils/threading.h"
#include "ast_kinds.h"

typedef struct sc_func_generic_inst_s sc_func_generic_inst;
typedef struct sc_struct_generic_inst_s sc_struct_generic_inst;
typedef struct expr_call_s expr_call;
typedef struct ast_elem_s ast_elem;

typedef struct symbol_s symbol;
// gim: generic instance map
typedef struct generic_inst_map_s {
    mutex lock;
    ureg count;
    u8 bitcount;
    ast_node_kind instances_kind;
    symbol** instances;
} generic_inst_map;

int gim_init(generic_inst_map* g, ast_node_kind instances_kind);
void gim_fin(generic_inst_map* g);

void gim_lock(generic_inst_map* g);
void gim_unlock(generic_inst_map* g);

sc_func_generic_inst**
gim_get_func(generic_inst_map* g, ast_elem** args, ureg arg_count);
sc_struct_generic_inst**
gim_get_struct(generic_inst_map* g, ast_elem** args, ureg arg_count);
