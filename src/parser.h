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
    lexer lx;
    mdg_node* current_module;
    sbuffer body_stack; // sounds kinda morbid :)
} parser;

// pp scopes sit below the rt scope, node and body are repeated
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

bool ast_node_may_drop_semicolon(ast_node* n);
#endif
