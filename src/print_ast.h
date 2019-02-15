#pragma once
#include "ast.h"

char* op_to_str(op_type t);
bool is_unary_op_postfix(op_type t);
void print_astn(stmt* astn, ureg indent);
void print_expr(astn* expr);