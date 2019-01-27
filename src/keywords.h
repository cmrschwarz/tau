#include "utils/c_extensions.h"
#include "stdbool.h"
#include "utils/string.h"

typedef enum PACK_ENUM keyword_id{
    KW_MODULE,
    KW_FUNC,
    KW_STRUCT,
    KW_AUTO,
    KW_FOR,
    KW_WHILE,
    KW_DO,
    KW_LOOP,
    KW_CONTINUE,
    KW_BREAK,
    KW_RETURN,
    KW_LABEL,
    KW_GOTO,
    KW_IF,
    KW_SWITCH,
    KW_LET,
    KW_CONST,
    KW_EXTEND,
    KW_PUBLIC,
    KW_PROTECTED,
    KW_PRIVATE,
    KW_STATIC,
    KW_VIRTUAL,
    KW_COUNT,
    KW_INVALID_KEYWORD,
}keyword_id;

bool kw_equals(keyword_id id, string str);
keyword_id kw_match(string str);

extern char* keyword_strings[];