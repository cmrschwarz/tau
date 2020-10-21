#pragma once
#include "utils/sbuffer.h"
#include "utils/pool.h"
typedef struct symbol_s symbol;
typedef struct ast_body_s ast_body;
typedef struct ast_node_s ast_node;
typedef struct tauc_s tauc;

int name_mangle(
    tauc* t, ast_node* node, ureg id, sbuffer* buff, pool* output_mem);
