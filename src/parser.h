#pragma once
#include "tokenizer.h"
#include "ast.h"
#include "utils/list_builder.h"

typedef enum expr_parse_error{
    EPE_OK = 0,
    EPE_INSANE,
    EPE_EOEX,
    EPE_MISSMATCH,
    EPE_HANDLED,
    EPE_TK_ERROR
}expr_parse_error;


typedef struct parser{
    tokenizer tk;
    list_builder lb;
    named_ast_node root;
    named_ast_node* curr_parent;
}parser;

int parser_init(parser* p, thread_context* tc);
void parser_fin(parser* p);
int parser_parse_file(parser* p, file* f);
expr_parse_error parse_expression(parser* p, expr_node** en);
expr_parse_error parse_expression_p(
    parser* p, ureg prec, expr_node** en, ureg* end, bool fill_src_range
);