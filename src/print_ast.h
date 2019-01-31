#pragma once
#include "ast.h"

char* op_to_str(expr_node_type t);
bool is_unary_op_postfix(expr_node_type t);
int print_astn(ast_node* astn);
int print_expr(expr_node* expr);