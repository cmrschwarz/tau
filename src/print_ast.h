#pragma once
#include "ast.h"

char* op_to_str(expr_node_type t);
bool is_unary_op_postfix(expr_node_type t);
void print_astn(ast_node* astn, ureg indent);
void print_expr(expr_node* expr);