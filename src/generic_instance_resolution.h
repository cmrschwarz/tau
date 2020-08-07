#pragma once
#include "ast.h"
#include "resolver.h"

resolve_error resolve_generic_struct(
    resolver* r, expr_access* ea, sc_struct_generic* sg, ast_body* parent_body,
    ast_elem** value, ast_elem** ctype);
