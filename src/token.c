#include "token.h"
#include "file_map.h"
#include "stdio.h"

// clang-format off
const char* token_strings[255] = {
    [TT_KW_MODULE] = "module",
    [TT_KW_EXTEND] = "extend",
    [TT_KW_REQUIRE] = "require",
    [TT_KW_IMPORT] = "import",
    [TT_KW_INCLUDE] = "include",
    [TT_KW_CONST] = "const",
    [TT_KW_MUT] = "mut",
    [TT_KW_SEALED] = "sealed",
    [TT_KW_VIRTUAL] = "virtual",
    [TT_KW_STATIC] = "static",
    [TT_KW_AS] = "as",
    [TT_KW_FUNC] = "func",
    [TT_KW_STRUCT] = "struct",
    [TT_KW_TRAIT] = "trait",
    [TT_KW_AUTO] = "auto",
    [TT_KW_FOR] = "for",
    [TT_KW_WHILE] = "while",
    [TT_KW_DO] = "do",
    [TT_KW_LOOP] = "loop",
    [TT_KW_CONTINUE] = "continue",
    [TT_KW_BREAK] = "break",
    [TT_KW_RETURN] = "return",
    [TT_KW_GIVE] = "give",
    [TT_KW_LABEL] = "label",
    [TT_KW_GOTO] = "goto",
    [TT_KW_USING] = "using",
    [TT_KW_IF] = "if",
    [TT_KW_ELSE] = "else",
    [TT_KW_MATCH] = "match",
    [TT_KW_LET] = "let",
    [TT_KW_CONST] = "const",
    [TT_KW_PUBLIC] = "public",
    [TT_KW_PROTECTED] = "protected",
    [TT_KW_PRIVATE] = "private",
    [TT_KW_FINALLY] = "finally",

    [TT_MINUS_EQUALS] = "-=",
    [TT_PLUS_EQUALS] = "+=",
    [TT_STAR_EQUALS] = "*=",
    [TT_EQUALS] = "=",
    [TT_DOUBLE_EQUALS] = "==",
    [TT_EXCLAMATION_MARK] = "!=",
    [TT_EXCLAMATION_MARK_EQUALS] = "!=",
    [TT_PERCENT] = "%",
    [TT_PERCENT_EQUALS] = "%=",
    [TT_SLASH] = "/",
    [TT_SLASH_EQUALS] = "/=",
    [TT_LESS_THAN] = "<",
    [TT_LESS_THAN_EQUALS] = "<=",
    [TT_DOUBLE_LESS_THAN] = "<<",
    [TT_DOUBLE_LESS_THAN_EQUALS] = "<<=",
    [TT_GREATER_THAN] = ">",
    [TT_GREATER_THAN_EQUALS] = ">=",
    [TT_DOUBLE_GREATER_THAN] = ">>",
    [TT_DOUBLE_GREATER_THAN_EQUALS] = ">>=",
    [TT_AND] = "&",
    [TT_DOUBLE_AND] = "&&",
    [TT_AND_EQUALS] = "&=",
    [TT_PIPE] = "|",
    [TT_DOUBLE_PIPE] = "||",
    [TT_PIPE_EQUALS] = "|=",
    [TT_CARET] = "^",
    [TT_DOUBLE_CARET] = "^^",
    [TT_CARET_EQUALS] = "^=",
    [TT_TILDE] = "~",
    [TT_TILDE_EQUALS] = "~=",

    [TT_PLUS] = "+",
    [TT_DOUBLE_PLUS] = "++",
    [TT_MINUS] = "-",
    [TT_DOUBLE_MINUS] = "--",
    [TT_STAR] = "*",
    [TT_PAREN_OPEN] = "(",
    [TT_PAREN_CLOSE] = ")",
    [TT_BRACKET_OPEN] = "[",
    [TT_BRACKET_CLOSE] = "]",
    [TT_BRACE_OPEN] = "{",
    [TT_BRACE_CLOSE] = "}",

    [TT_DOLLAR] = "$",
    [TT_COMMA] = ",",
    [TT_SEMICOLON] = ";",
    [TT_HASH] = "#",
    [TT_DOUBLE_HASH] = "##",
    [TT_DOT] = ".",
    [TT_COLON] = ":",
    [TT_DOUBLE_COLON] = "::",
    [TT_ARROW] = "->",
    [TT_FAT_ARROW] = "=>",

    [TT_NUMBER] = "expression",
    [TT_LITERAL] = "expression",
    [TT_BINARY_LITERAL] = "expression",
    [TT_STRING] = "expression",
    [TT_EOF] = "EOF",
};
// clang-format on

token_type match_kw(string str)
{
    for (ureg kwid = TT_MIN_KW_INDEX; kwid < TT_MAX_KW_INDEX; kwid++) {
        if (string_eq_cstr(str, token_strings[kwid])) return kwid;
    }
    return TT_NONE;
}

void token_debug_print(src_file* f, token* t)
{
    if (t == NULL) {
        puts("ERROR");
        return;
    }
    switch (t->type) {
        case TT_NUMBER:
        case TT_STRING: {
            string_print(t->str);
        } break;
        case TT_LITERAL: {
            putchar('"');
            string_print(t->str);
            putchar('"');
        } break;
        case TT_BINARY_LITERAL: {
            putchar('\'');
            string_print(t->str);
            putchar('\'');
        } break;
        default: {
            fputs(token_strings[t->type], stdout);
        } break;
    }
    src_pos p = src_map_get_pos(&f->src_map, t->start);
    printf(
        "[%llu; %llu | l: %llu c: %llu]\n", t->start, t->end, p.line, p.column);
}
