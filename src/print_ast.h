#pragma once
#include "ast.h"

bool is_unary_op_postfix(operator_kind t);
void print_ast_node_nl(ast_node* n, ureg indent);
void print_ast_node(ast_node* n, ureg indent);
void print_mdg_node(mdg_node* mdg, ureg indent);
