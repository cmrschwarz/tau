#pragma once
#include "ast.h"
#include "tokenizer.h"
#include "utils/list_builder.h"

typedef enum parse_error {
    PE_OK = 0,
    PE_FATAL,
    PE_EOEX,
    PE_UNEXPECTED_TOKEN,
    PE_HANDLED,
    PE_TK_ERROR,
} parse_error;

typedef struct parser {
    tokenizer tk;
    list_builder list_builder;
    osc_module root;
    scope* curr_scope;
    mdg_node* current_module;
} parser;

int parser_init(parser* p, thread_context* tc);
void parser_fin(parser* p);
typedef struct job_parse job_parse;
parse_error parser_parse_file(parser* p, job_parse* j);
module_import* imports;

bool stmt_allowed_to_drop_semicolon(stmt* s);
bool expr_allowed_to_drop_semicolon(expr* e);
