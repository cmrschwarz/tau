#pragma once
#include "utils/types.h"
#include "utils/string.h"
#include "src_map.h"
#include "utils/c_extensions.h"

typedef enum PACK_ENUM{
    TT_NUMBER = 0,
    TT_LITERAL = 1,
    TT_BINARY_LITERAL = 2,
    TT_STRING = 3,
    TT_EOF,

    TT_IF,
    TT_ELSE,
    TT_MATCH,
    TT_FOR,
    TT_WHILE,
    TT_LOOP,
    TT_RETURN,
    TT_FUNC,
    TT_PUBLIC,
    TT_STRUCT,
    TT_TRAIT,
    TT_INTERFACE,
    TT_MODULE,
    TT_EXTEND,
    TT_INHERITS,
    TT_IMPLEMENTS,
    TT_ALIAS,
    
    TT_MINUS_EQUALS,
    TT_PLUS_EQUALS,
    TT_STAR_EQUALS,
    TT_EQUALS,
    TT_DOUBLE_EQUALS,
    TT_EXCLAMATION_MARK,
    TT_EXCLAMATION_MARK_EQUALS,
    TT_PERCENT,
    TT_PERCENT_EQUALS,
    TT_SLASH,
    TT_SLASH_EQUALS,
    TT_LESS_THAN,
    TT_LESS_THAN_EQUALS,
    TT_DOUBLE_LESS_THAN,
    TT_DOUBLE_LESS_THAN_EQUALS,
    TT_GREATER_THAN,
    TT_GREATER_THAN_EQUALS,
    TT_DOUBLE_GREATER_THAN,
    TT_DOUBLE_GREATER_THAN_EQUALS,
    TT_AND,
    TT_DOUBLE_AND,
    TT_AND_EQUALS,
    TT_PIPE,
    TT_DOUBLE_PIPE,
    TT_PIPE_EQUALS,
    TT_CARET,
    TT_DOUBLE_CARET,
    TT_CARET_EQUALS,
    TT_TILDE,
    TT_TILDE_EQUALS,

    TT_PLUS,
    TT_DOUBLE_PLUS,
    TT_MINUS,
    TT_DOUBLE_MINUS,
    TT_STAR,
    TT_PAREN_OPEN,
    TT_PAREN_CLOSE,
    TT_BRACKET_OPEN,
    TT_BRACKET_CLOSE,
    TT_BRACE_OPEN,
    TT_BRACE_CLOSE,

    TT_DOLLAR,
    TT_COMMA,
    TT_SEMICOLON,
    TT_HASH,
    TT_DOUBLE_HASH,
    TT_DOT,
    TT_COLON,
    TT_DOUBLE_COLON,
    TT_ARROW,
    TT_LEFT_ARROW,
}token_type;

extern const char* token_strings[255];

typedef struct token{
    token_type type;
    string str;
    ureg start;
    ureg end;
}token;


static inline bool token_has_string(token* t){
    return t->type < 4;
}

void token_print(file* f, token* t);

