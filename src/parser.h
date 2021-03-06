#pragma once
#include "ast.h"
#include "lexer.h"
#include "utils/list_builder.h"

typedef struct thread_context_s thread_context;
typedef struct job_parse_s job_parse;

typedef enum parse_error_e {
    PE_OK = 0,
    PE_EOEX,
    PE_NO_STMT,
    PE_ERROR,
    PE_FATAL,
    PE_LX_ERROR,
} parse_error;

typedef struct parser_s {
    lexer lx;
    // ! when parsing a paste, this is the file that contained the paste expr
    src_file* current_file;
    mdg_node* current_module;
    bool disable_macro_body_call;
    module_frame* file_root;
    ast_body* paste_block;
    ast_body* paste_parent_body;
    ast_body* paste_parent_shared_body;
    sbuffer body_stack;
} parser;

typedef struct element_occurence_counts_s {
    ureg decl_count;
    ureg usings_count;
    ureg impl_count;
    ureg generic_impl_count;
} element_occurence_counts;
// pp scopes sit below the rt scope, the node ist the pp node, the body is
// repeated
typedef struct body_parse_data_s {
    ast_node* node;
    ast_body* body;
    element_occurence_counts elem_counts;
    element_occurence_counts shared_elem_counts;
} body_parse_data;

int parser_init(parser* p, thread_context* tc);
void parser_fin(parser* p);
parse_error parser_parse_file(parser* p, job_parse* j);
parse_error parser_parse_paste_expr(
    parser* p, expr_pp* epp, ast_body* parent_body,
    ast_body* parent_shared_body);
parse_error parser_parse_paste_stmt(
    parser* p, expr_pp* epp, ast_body* parent_body,
    ast_body* parent_shared_body);

bool ast_node_may_drop_semicolon(ast_node* n);
