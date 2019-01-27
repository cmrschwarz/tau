#pragma once
#include "tokenizer.h"
#include "ast.h"

typedef struct parser{
    tokenizer tk;
    named_ast_node root;
    named_ast_node* curr_parent;
}parser;

int parser_init(parser* p, thread_context* tc);
void parser_fin(parser* p);
int parser_parse_file(parser* p, file* f);