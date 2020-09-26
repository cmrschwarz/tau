#include "token.h"
#include "file_map.h"
#include "stdio.h"
#include "utils/debug_utils.h"

const char* COND_KW_CONSTRUCT = "construct";
const char* COND_KW_DESTRUCT = "destruct";
const char* COND_KW_FOR = "for";
const char* COND_KW_ASM = "asm";
const char* COND_KW_MAIN = "main";
const char* COND_KW_START = "_start";

// clang-format off
const char* token_strings[255] = {
    [TK_KW_MODULE] = "module",
    [TK_KW_MACRO] = "macro",
    [TK_KW_PASTE] = "paste",
    [TK_KW_DYNAMIC] = "dynamic",
    [TK_KW_RUNTIME] = "runtime",
    [TK_KW_EXTEND] = "extend",
    [TK_KW_IMPL] = "impl",
    [TK_KW_EXTERN] = "extern",
    [TK_KW_REQUIRE] = "require",
    [TK_KW_IMPORT] = "import",
    [TK_KW_INCLUDE] = "include",
    [TK_KW_CONST] = "const",
    [TK_KW_MUT] = "mut",
    [TK_KW_STATIC] = "static",
    [TK_KW_AS] = "as",
    [TK_KW_FUNC] = "func",
    [TK_KW_OP] = "op",
    [TK_KW_IMPLICIT] = "implicit",
    [TK_KW_EXPLICIT] = "explicit",
    [TK_KW_STRUCT] = "struct",
    [TK_KW_SELF] = "self",
    [TK_KW_SELF_UPPERCASE] = "Self",
     [TK_KW_SUPER] = "super",
    [TK_KW_TRAIT] = "trait",
    [TK_KW_LOOP] = "loop",
    [TK_KW_CONTINUE] = "continue",
    [TK_KW_BREAK] = "break",
    [TK_KW_RETURN] = "return",
    [TK_KW_USE] = "use",
    [TK_KW_IF] = "if",
    [TK_KW_ELSE] = "else",
    [TK_KW_MATCH] = "match",
    [TK_KW_LOCAL] = "local",
    [TK_KW_INTERNAL] = "internal",
    [TK_KW_PRIVATE] = "private",
    [TK_KW_PROTECTED] = "protected",
    [TK_KW_PUBLIC] = "public",

    [TK_MINUS_EQUALS] = "-=",
    [TK_PLUS_EQUALS] = "+=",
    [TK_STAR_EQUALS] = "*=",
    [TK_EQUALS] = "=",
    [TK_DOUBLE_EQUALS] = "==",
    [TK_EXCLAMATION_MARK] = "!=",
    [TK_EXCLAMATION_MARK_EQUALS] = "!=",
    [TK_PERCENT] = "%",
    [TK_PERCENT_EQUALS] = "%=",
    [TK_SLASH] = "/",
    [TK_SLASH_EQUALS] = "/=",
    [TK_LESS_THAN] = "<",
    [TK_LESS_THAN_EQUALS] = "<=",
    [TK_DOUBLE_LESS_THAN] = "<<",
    [TK_DOUBLE_LESS_THAN_EQUALS] = "<<=",
    [TK_GREATER_THAN] = ">",
    [TK_GREATER_THAN_EQUALS] = ">=",
    [TK_DOUBLE_GREATER_THAN] = ">>",
    [TK_DOUBLE_GREATER_THAN_EQUALS] = ">>=",
    [TK_AND] = "&",
    [TK_DOUBLE_AND] = "&&",
    [TK_AND_EQUALS] = "&=",
    [TK_PIPE] = "|",
    [TK_DOUBLE_PIPE] = "||",
    [TK_PIPE_EQUALS] = "|=",
    [TK_CARET] = "^",
    [TK_DOUBLE_CARET] = "^^",
    [TK_CARET_EQUALS] = "^=",
    [TK_TILDE] = "~",
    [TK_TILDE_EQUALS] = "~=",

    [TK_PLUS] = "+",
    [TK_DOUBLE_PLUS] = "++",
    [TK_MINUS] = "-",
    [TK_DOUBLE_MINUS] = "--",
    [TK_STAR] = "*",
    [TK_PAREN_OPEN] = "(",
    [TK_PAREN_CLOSE] = ")",
    [TK_BRACKET_OPEN] = "[",
    [TK_BRACKET_CLOSE] = "]",
    [TK_BRACE_OPEN] = "{",
    [TK_BRACE_CLOSE] = "}",

    [TK_DOLLAR] = "$",
    [TK_COMMA] = ",",
    [TK_SEMICOLON] = ";",
    [TK_HASH] = "#",
    [TK_DOT] = ".",
    [TK_COLON] = ":",
    [TK_DOUBLE_COLON] = ":",
    [TK_ARROW] = "->",
    [TK_FAT_ARROW] = "=>",

    [TK_NUMBER] = "expression",
    [TK_STRING] = "expression",
    [TK_BINARY_STRING] = "expression",
    [TK_IDENTIFIER] = "expression",
    [TK_EOF] = "EOF",
    [TK_AT] = "@",
    [TK_BACKSLASH] = "\\",
};
// clang-format on

token_kind match_kw(string str)
{
    for (ureg kwid = TK_MIN_KW_INDEX; kwid < TK_MAX_KW_INDEX; kwid++) {
        if (string_eq_cstr(str, token_strings[kwid])) return kwid;
    }
    return TK_NONE;
}

void token_debug_print(src_file* f, token* t)
{
    if (t == NULL) {
        puts("ERROR");
        return;
    }
    switch (t->kind) {
        case TK_NUMBER:
        case TK_IDENTIFIER: {
            string_print(t->str);
        } break;
        case TK_STRING: {
            putchar('"');
            string_print(t->str);
            putchar('"');
        } break;
        case TK_BINARY_STRING: {
            putchar('\'');
            string_print(t->str);
            putchar('\'');
        } break;
        default: {
            fputs(token_strings[t->kind], stdout);
        } break;
    }
    src_pos p = src_map_get_pos(&f->smap, t->start);
    tprintf("[%zu; %zu | l: %zu c: %zu]\n", t->start, t->end, p.line, p.column);
}
