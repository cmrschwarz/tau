#pragma once
#include "src_map.h"
#include "utils/c_extensions.h"
#include "utils/string.h"
#include "utils/types.h"

typedef enum PACK_ENUM {
    TT_NUMBER,
    TT_MIN_STR_INDEX = TT_NUMBER,
    TT_LITERAL,
    TT_BINARY_LITERAL,
    TT_STRING,
    TT_MAX_STR_INDEX = TT_STRING,

    TT_KW_MODULE,
    TT_MIN_KW_INDEX = TT_KW_MODULE,
    TT_KW_IMPORT,
    TT_KW_INCLUDE,
    TT_KW_EXTEND,
    TT_KW_REQUIRE,
    TT_KW_CONST,
    TT_KW_MUT,
    TT_KW_SEALED,
    TT_KW_VIRTUAL,
    TT_KW_PUBLIC,
    TT_KW_PROTECTED,
    TT_KW_PRIVATE,
    TT_KW_STATIC,
    TT_KW_AS,
    TT_KW_FUNC,
    TT_KW_STRUCT,
    TT_KW_TRAIT,
    TT_KW_AUTO,
    TT_KW_FOR,
    TT_KW_WHILE,
    TT_KW_DO,
    TT_KW_LOOP,
    TT_KW_FINALLY,
    TT_KW_CONTINUE,
    TT_KW_BREAK,
    TT_KW_RETURN,
    TT_KW_GIVE,
    TT_KW_LABEL,
    TT_KW_USING,
    TT_KW_GOTO,
    TT_KW_IF,
    TT_KW_ELSE,
    TT_KW_MATCH,
    TT_KW_LET,
    TT_KW_COUNT,
    TT_MAX_KW_INDEX = TT_KW_COUNT,

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
    TT_ARROW,
    TT_FAT_ARROW,

    TT_EOF,
    TT_NONE,
} token_type;

extern const char* token_strings[255];

typedef struct token {
    token_type type;
    string str;
    ureg start;
    ureg end;
} token;

static inline bool token_has_string(token* t)
{
    return t->type >= TT_MIN_STR_INDEX && t->type <= TT_MAX_STR_INDEX;
}
static inline bool token_is_keyword(token* t)
{
    return t->type >= TT_MIN_KW_INDEX && t->type <= TT_MAX_KW_INDEX;
}

token_type match_kw(string str);

void token_debug_print(src_file* f, token* t);
