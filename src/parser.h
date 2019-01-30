#pragma once
#include "tokenizer.h"
#include "ast.h"
#include "utils/list_builder.h"

typedef enum expr_parsing_error{
    EPE_OK = 0,
    EPE_INSANE,
    EPE_EOEX,
    EPE_MISSMATCH,
    EPE_HANDLED,
}expr_parsing_error;


typedef struct parser{
    tokenizer tk;
    list_builder lb;
    named_ast_node root;
    named_ast_node* curr_parent;
}parser;

int parser_init(parser* p, thread_context* tc);
void parser_fin(parser* p);
int parser_parse_file(parser* p, file* f);
expr_parsing_error parse_expression(parser* p, expr_node** en);