#pragma once
#include "ast.h"
typedef struct sbuffer_s sbuffer;
typedef enum print_mode_e {
    PM_FULL,
    PM_TYPE,
    PM_DECL,
} print_mode;

bool is_unary_op_postfix(operator_kind t);
int print_ast_elem_nl(
    ast_node* n, ast_body* ctx, print_mode pm, ureg indent, sbuffer* buff);
int print_ast_node(
    ast_node* n, ast_body* ctx, print_mode pm, ureg indent, sbuffer* buff);
int print_ast_elem(
    ast_elem* n, ast_body* ctx, print_mode pm, ureg indent, sbuffer* buff);
int print_indent(ureg indent, sbuffer* buff);
int print_mdg_node(mdg_node* mdg, ureg indent, sbuffer* buff);
char* ast_elem_to_string(
    thread_context* tc, pool* mem, ast_elem* n, ast_body* context,
    print_mode pm, ureg* str_len);