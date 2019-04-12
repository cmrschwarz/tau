#include "stdbool.h"
#include "utils/c_extensions.h"
#include "utils/string.h"

typedef enum PACK_ENUM keyword_id {
    KW_MODULE = 0,
    KW_IMPORT,
    KW_INCLUDE,
    KW_EXTEND,
    KW_REQUIRE,
    KW_CONST,
    KW_MUT,
    KW_SEALED,
    KW_VIRTUAL,
    KW_PUBLIC,
    KW_PROTECTED,
    KW_PRIVATE,
    KW_STATIC,

    KW_AS,
    KW_FUNC,
    KW_STRUCT,
    KW_TRAIT,
    KW_AUTO,
    KW_FOR,
    KW_WHILE,
    KW_DO,
    KW_LOOP,
    KW_FINALLY,
    KW_CONTINUE,
    KW_BREAK,
    KW_RETURN,
    KW_GIVE,
    KW_LABEL,
    KW_ALIAS,
    KW_GOTO,
    KW_IF,
    KW_ELSE,
    KW_MATCH,
    KW_LET,
    KW_COUNT,
    KW_INVALID_KEYWORD,
} keyword_id;

bool kw_equals(keyword_id id, string str);
keyword_id kw_match(string str);
keyword_id kw_match_visibility_or_mutability(string str);

extern char* keyword_strings[];
