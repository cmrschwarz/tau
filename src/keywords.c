#include "keywords.h"
#include "utils/math_utils.h"
#include <string.h>

char* keyword_strings[] = {
    [KW_MODULE] = "module",
    [KW_EXTEND] = "extend",
    [KW_REQUIRE] = "require",
    [KW_IMPORT] = "import",
    [KW_INCLUDE] = "include",
    [KW_CONST] = "const",
    [KW_SEALED] = "sealed",
    [KW_VIRTUAL] = "virtual",
    [KW_STATIC] = "static",
    [KW_AS] = "as",
    [KW_FUNC] = "func",
    [KW_STRUCT] = "struct",
    [KW_TRAIT] = "trait",
    [KW_AUTO] = "auto",
    [KW_FOR] = "for",
    [KW_WHILE] = "while",
    [KW_DO] = "do",
    [KW_LOOP] = "loop",
    [KW_CONTINUE] = "continue",
    [KW_BREAK] = "break",
    [KW_RETURN] = "return",
    [KW_LABEL] = "label",
    [KW_GOTO] = "goto",
    [KW_IF] = "if",
    [KW_SWITCH] = "switch",
    [KW_LET] = "let",
    [KW_CONST] = "const",
    [KW_PUBLIC] = "public",
    [KW_PROTECTED] = "protected",
    [KW_PRIVATE] = "private",
    [KW_INVALID_KEYWORD] = NULL,
};

bool kw_equals(keyword_id id, string str)
{
    return (
        strncmp(keyword_strings[id], str.start, ptrdiff(str.end, str.start)) ==
        0);
}

keyword_id kw_match(string str)
{
    // PERF: you could write a fancy tree match here
    keyword_id i = 0;
    while (keyword_strings[i] != NULL) {
        if (kw_equals(i, str)) return i;
        i++;
    }
    return KW_INVALID_KEYWORD;
}