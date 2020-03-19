#pragma once
#include "ast.h"
#include "resolver.h"

resolve_error resolve_generic_struct(
    resolver* r, expr_access* ea, sc_struct_generic* sg, symbol_table* st,
    ureg ppl, ast_elem** value, ast_elem** ctype);
