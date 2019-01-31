#pragma once
#include "tokenizer.h"
#include "ast.h"
#include "utils/list_builder.h"
#include "iht.h"

typedef enum parse_error{
    PE_OK = 0,
    PE_INSANE,
    PE_EOEX,
    PE_UNEXPECTED_TOKEN,
    PE_HANDLED,
    PE_TK_ERROR
}parse_error;


typedef struct parser{
    tokenizer tk;
    list_builder lb;
    astn_module root;
    named_ast_node* curr_parent;
    iht iht;
}parser;

int parser_init(parser* p, thread_context* tc);
void parser_fin(parser* p);
int parser_parse_file(parser* p, file* f);
parse_error parse_expression(parser* p, expr_node** en);
parse_error parse_expression_p(
    parser* p, ureg prec, expr_node** en, ureg* end, bool fill_src_range
);