#pragma once
#include "utils/sbuffer.h"
#include "utils/pool.h"
typedef struct symbol_s symbol;
typedef struct ast_body_s ast_body;

char* name_mangle(symbol* sym, sbuffer* buff, pool* output_mem);
