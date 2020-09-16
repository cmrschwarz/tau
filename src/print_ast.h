#pragma once
#include "ast.h"

bool is_unary_op_postfix(operator_kind t);
void print_ast_node_nl(ast_node* n, mdg_node* cmdg, ureg indent);
void print_ast_node(ast_node* n, mdg_node* cmdg, ureg indent);
void print_indent(ureg indent);
void print_mdg_node(mdg_node* mdg, ureg indent);
char* ctype_to_string(
    thread_context* tc, pool* mem, ast_body* context, ast_elem* ctype,
    ureg* str_len);