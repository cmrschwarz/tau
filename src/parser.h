#pragma once
#include "ast.h"
#include "lexer.h"
#include "utils/list_builder.h"

typedef enum parse_error {
    PE_OK = 0,
    PE_EOEX,
    PE_NO_STMT,
    PE_ERROR,
    PE_FATAL,
    PE_LX_ERROR,
} parse_error;

typedef struct parser {
    lexer tk;
    mdg_node* current_module;
    sbuffer body_stack; // sounds kinda morbid :)
} parser;

// pp scopes sit below the rt scope, node and body are repeated
typedef struct body_parse_data {
    ast_node* node;
    body* body;
    ureg decl_count;
    ureg usings_count;
    ureg shared_decl_count;
    ureg shared_usings_count;
} body_parse_data;

int parser_init(parser* p, thread_context* tc);
void parser_fin(parser* p);
typedef struct job_parse job_parse;
parse_error parser_parse_file(parser* p, job_parse* j);

bool ast_node_may_drop_semicolon(ast_node* n);
