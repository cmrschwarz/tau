#pragma once
#include "ast.h"
#include "tokenizer.h"
#include "utils/list_builder.h"

typedef enum parse_error {
    PE_OK = 0,
    PE_INSANE,
    PE_EOEX,
    PE_UNEXPECTED_TOKEN,
    PE_HANDLED,
    PE_TK_ERROR,
    PE_NOT_A_STATEMENT,
} parse_error;

typedef struct parser {
    tokenizer tk;
    list_builder lb;
    scs_module root;
    scope* curr_scope;
    ast_node_type parent_type;
} parser;

int parser_init(parser* p, thread_context* tc);
void parser_fin(parser* p);
parse_error parser_parse_file(parser* p, file* f);