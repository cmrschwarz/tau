#ifndef TAUC_PARSER_H
#define TAUC_PARSER_H
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
    ureg ppl;
    lexer lx;
    // ! when parsing a paste, this is the file that contained the paste expr
    src_file* current_file;
    mdg_node* current_module;
    bool disable_macro_body_call;
    osc_extend* file_root;
    bool paste_parent_owns_st;
    ast_body* paste_block;
    symbol_table** paste_parent_symtab;
    sbuffer body_stack; // sounds kinda morbid :)
} parser;

// pp scopes sit below the rt scope, the node ist the pp node, the body is
// repeated
typedef struct body_parse_data_s {
    ast_node* node;
    ast_body* body;
    ureg decl_count;
    ureg usings_count;
    ureg shared_decl_count;
    ureg shared_usings_count;
} body_parse_data;

int parser_init(parser* p, thread_context* tc);
void parser_fin(parser* p);
parse_error parser_parse_file(parser* p, job_parse* j);
parse_error
parser_parse_paste_expr(parser* p, expr_pp* epp, symbol_table* st, ureg ppl);
parse_error parser_parse_paste_stmt(
    parser* p, expr_pp* epp, symbol_table** st, bool owned_st);

bool ast_node_may_drop_semicolon(ast_node* n);
#endif
