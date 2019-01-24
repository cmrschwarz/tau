typedef enum keyword_id{
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
}keyword_id;

char* keyword_strings[] = {
    [KW_MODULE] = "module",
    [KW_FUNC] = "func",
    [KW_STRUCT] = "struct",
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
    [KW_EXTEND] = "extend",
    [KW_PUBLIC] = "public",
    [KW_PROTECTED] = "protected",
    [KW_PRIVATE] = "private",
    [KW_STATIC] = "static",
    [KW_VIRTUAL] = "virtual",
}