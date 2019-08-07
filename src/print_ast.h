#pragma once
#include "ast.h"

char* op_to_str(op_type t);
bool is_unary_op_postfix(op_type t);
void print_stmt(stmt* astn, ureg indent);
void print_stmt_nl(stmt* astn, ureg indent);
void print_expr(expr* expr, ureg indent);
