#ifndef TAUC_TOKEN_H
#define TAUC_TOKEN_H
#include "src_map.h"
#include "utils/c_extensions.h"
#include "utils/string.h"
#include "utils/types.h"

typedef struct src_file_s src_file;

typedef enum PACK_ENUM token_kind_e {
    TK_NONE,
    TK_NUMBER,
    TK_MIN_STR_INDEX = TK_NUMBER,
    TK_STRING, // a string enclosed by ""
    TK_BINARY_STRING, // a string enclosed by ''
    TK_IDENTIFIER, // a string not enclosed by any quotation
    TK_MAX_STR_INDEX = TK_IDENTIFIER,

    TK_KW_MODULE,
    TK_MIN_KW_INDEX = TK_KW_MODULE,
    TK_KW_MACRO,
    TK_KW_PASTE,
    TK_KW_IMPORT,
    TK_KW_INCLUDE,
    TK_KW_EXTEND,
    TK_KW_REQUIRE,
    TK_KW_CONST,
    TK_KW_MUT,
    TK_KW_SEALED,
    TK_KW_SELF,
    TK_KW_VIRTUAL,
    TK_KW_PUBLIC,
    TK_KW_PROTECTED,
    TK_KW_PRIVATE,
    TK_KW_STATIC,
    TK_KW_AS,
    TK_KW_FUNC,
    TK_KW_STRUCT,
    TK_KW_TRAIT,
    TK_KW_AUTO,
    TK_KW_LOOP,
    TK_KW_FINALLY,
    TK_KW_CONTINUE,
    TK_KW_BREAK,
    TK_KW_RETURN,
    TK_KW_USING,
    TK_KW_IF,
    TK_KW_ELSE,
    TK_KW_MATCH,
    TK_KW_LET,
    TK_KW_COUNT,
    TK_MAX_KW_INDEX = TK_KW_COUNT,

    TK_MINUS_EQUALS,
    TK_PLUS_EQUALS,
    TK_STAR_EQUALS,
    TK_EQUALS,
    TK_DOUBLE_EQUALS,
    TK_EXCLAMATION_MARK,
    TK_EXCLAMATION_MARK_EQUALS,
    TK_PERCENT,
    TK_PERCENT_EQUALS,
    TK_SLASH,
    TK_SLASH_EQUALS,
    TK_LESS_THAN,
    TK_LESS_THAN_EQUALS,
    TK_DOUBLE_LESS_THAN,
    TK_DOUBLE_LESS_THAN_EQUALS,
    TK_GREATER_THAN,
    TK_GREATER_THAN_EQUALS,
    TK_DOUBLE_GREATER_THAN,
    TK_DOUBLE_GREATER_THAN_EQUALS,
    TK_AND,
    TK_AT,
    TK_DOUBLE_AND,
    TK_AND_EQUALS,
    TK_PIPE,
    TK_DOUBLE_PIPE,
    TK_PIPE_EQUALS,
    TK_CARET,
    TK_DOUBLE_CARET,
    TK_CARET_EQUALS,
    TK_TILDE,
    TK_TILDE_EQUALS,

    TK_PLUS,
    TK_DOUBLE_PLUS,
    TK_MINUS,
    TK_DOUBLE_MINUS,
    TK_STAR,
    TK_PAREN_OPEN,
    TK_PAREN_CLOSE,
    TK_BRACKET_OPEN,
    TK_BRACKET_CLOSE,
    TK_BRACE_OPEN,
    TK_BRACE_CLOSE,

    TK_DOLLAR,
    TK_COMMA,
    TK_SEMICOLON,
    TK_HASH,
    TK_DOT,
    TK_COLON,
    TK_DOUBLE_COLON,
    TK_ARROW,
    TK_FAT_ARROW,

    TK_EOF,
} token_kind;

extern const char* token_strings[255];

typedef struct token_s {
    token_kind kind;
    string str;
    ureg start;
    ureg end;
} token;

static inline bool token_has_string(token* t)
{
    return (t->kind >= TK_MIN_STR_INDEX) && (t->kind <= TK_MAX_STR_INDEX);
}
static inline bool token_is_keyword(token* t)
{
    return t->kind >= TK_MIN_KW_INDEX && t->kind <= TK_MAX_KW_INDEX;
}

token_kind match_kw(string str);

void token_debug_print(src_file* f, token* t);
#endif
