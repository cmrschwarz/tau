#pragma once
#include "ast.h"
#include "resolver.h"

resolve_error resolve_generic_struct_access(
    resolver* r, expr_access* ea, ast_body* body, sc_struct_generic* sg,
    ast_elem** value, ast_elem** ctype);

resolve_error resolve_generic_struct_instance(
    resolver* r, sc_struct_generic_inst* sgi, ast_body* body,
    thread_waiting_pprn* waiter);