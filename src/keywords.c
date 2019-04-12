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
    [KW_MUT] = "mut",
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
    [KW_GIVE] = "give",
    [KW_LABEL] = "label",
    [KW_GOTO] = "goto",
    [KW_ALIAS] = "alias",
    [KW_IF] = "if",
    [KW_ELSE] = "else",
    [KW_MATCH] = "match",
    [KW_LET] = "let",
    [KW_CONST] = "const",
    [KW_PUBLIC] = "public",
    [KW_PROTECTED] = "protected",
    [KW_PRIVATE] = "private",
    [KW_FINALLY] = "finally",

    [KW_INVALID_KEYWORD] = NULL,
};

bool kw_equals(keyword_id id, string str)
{
    return (string_cmp_cstr(str, keyword_strings[id]) == 0);
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

keyword_id kw_match_visibility_or_mutability(string str)
{
    char* s = str.start;
    switch (*s) {
        case 'p': {
            s++;
            if (*s == 'u') {
                if (kw_equals(KW_PUBLIC, str)) return KW_PUBLIC;
            }
            else if (*s == 'r') {
                if (kw_equals(KW_PRIVATE, str)) return KW_PRIVATE;
                if (kw_equals(KW_PROTECTED, str)) return KW_PROTECTED;
            }
        } break;
        case 'm': {
            if (kw_equals(KW_MUT, str)) return KW_MUT;
        } break;
        case 'c': {
            if (kw_equals(KW_CONST, str)) return KW_CONST;
        } break;
        default:; // fallthrough
    }
    return KW_INVALID_KEYWORD;
}
