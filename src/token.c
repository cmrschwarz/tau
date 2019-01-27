#include "token.h"
#include "stdio.h"
const char* token_strings[255] = {
   [TT_IF]                          = "if",
   [TT_ELSE]                        = "else",
   [TT_MATCH]                       = "match",
   [TT_FOR]                         = "for",
   [TT_WHILE]                       = "while",
   [TT_LOOP]                        = "loop",
   [TT_RETURN]                      = "return",
   [TT_FUNC]                        = "func",
   [TT_PUBLIC]                      = "public",
   [TT_STRUCT]                      = "struct",
   [TT_TRAIT]                       = "trait",
   [TT_INTERFACE]                   = "interface",
   [TT_MODULE]                      = "module",
   [TT_EXTEND]                      = "extend",
   [TT_INHERITS]                    = "inherits",
   [TT_IMPLEMENTS]                  = "implements",
   [TT_ALIAS]                       = "alias",

    [TT_MINUS_EQUALS]               = "-=",
    [TT_PLUS_EQUALS]                = "+=",
    [TT_STAR_EQUALS]                = "*=",
    [TT_EQUALS]                     = "=",
    [TT_DOUBLE_EQUALS]              = "==",
    [TT_EXCLAMATION_MARK]           = "!=",
    [TT_EXCLAMATION_MARK_EQUALS]    = "!=",
    [TT_PERCENT]                    = "%",
    [TT_PERCENT_EQUALS]             = "%=",
    [TT_SLASH]                      = "/",
    [TT_SLASH_EQUALS]               = "/=",
    [TT_LESS_THAN]                  = "<",
    [TT_LESS_THAN_EQUALS]           = "<=",
    [TT_DOUBLE_LESS_THAN]           = "<<",
    [TT_DOUBLE_LESS_THAN_EQUALS]    = "<<=",
    [TT_GREATER_THAN]               = ">",
    [TT_GREATER_THAN_EQUALS]        = ">=",
    [TT_DOUBLE_GREATER_THAN]        = ">>",
    [TT_DOUBLE_GREATER_THAN_EQUALS] = ">>=",
    [TT_AND]                        = "&",
    [TT_DOUBLE_AND]                 = "&&",
    [TT_AND_EQUALS]                 = "&=",
    [TT_PIPE]                       = "|",
    [TT_DOUBLE_PIPE]                = "||",
    [TT_PIPE_EQUALS]                = "|=",
    [TT_CARET]                      = "^",
    [TT_DOUBLE_CARET]               = "^^",
    [TT_CARET_EQUALS]               = "^=",
    [TT_TILDE]                      = "~",
    [TT_TILDE_EQUALS]               = "~=",

    [TT_PLUS]                       = "+",
    [TT_DOUBLE_PLUS]                = "++",
    [TT_MINUS]                      = "-",
    [TT_DOUBLE_MINUS]               = "--",
    [TT_STAR]                       = "*",
    [TT_PAREN_OPEN]                 = "(",
    [TT_PAREN_CLOSE]                = ")",
    [TT_BRACKET_OPEN]               = "[",
    [TT_BRACKET_CLOSE]              = "]",
    [TT_BRACE_OPEN]                 = "{",
    [TT_BRACE_CLOSE]                = "}",

    [TT_DOLLAR]                     = "$",
    [TT_COMMA]                      = ",",
    [TT_SEMICOLON]                  = ";",
    [TT_HASH]                       = "#",
    [TT_DOUBLE_HASH]                = "##",
    [TT_DOT]                        = ".",
    [TT_COLON]                      = ":",
    [TT_DOUBLE_COLON]               = "::",
    [TT_ARROW]                      = "->",
    [TT_LEFT_ARROW]                 = "<==",

    [TT_NUMBER]                     = 0,
    [TT_LITERAL]                    = 0,
    [TT_BINARY_LITERAL]             = 0,
    [TT_STRING]                     = 0,
    [TT_EOF]                        = "EOF",
};
void token_debug_print(file* f, token* t){
    if(t == NULL){
        puts("ERROR");
        return;
    }
    switch(t->type){
        case TT_NUMBER:
        case TT_STRING:{
            string_print(t->str);
        }break;
        case TT_LITERAL:{
            putchar('"');
            string_print(t->str);
            putchar('"');
        }break;
        case TT_BINARY_LITERAL:{
            putchar('\'');
            string_print(t->str);
            putchar('\'');
        }break;
        default:{
            fputs(token_strings[t->type], stdout);
        }break;
    }
    src_pos p = src_map_get_pos(&f->src_map, t->start);
    printf("[%llu; %llu | l: %llu c: %llu]\n", t->start, t->end, p.line, p.column);
}