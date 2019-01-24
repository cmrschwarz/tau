#include "utils/c_extensions.h"
typedef enum PACk_STRUCT keyword_id{
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
}keyword_id;

extern char* keyword_strings[];