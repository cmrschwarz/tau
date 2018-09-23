#include "tokenizer.h"
static inline void tk_inc_iter(tokenizer* tk, token** t){
    (*t)++;
    if(*t == tk->token_buffer_end){
        *t = tk->token_buffer;
    }
}
static inline void tk_inc_iter_n(tokenizer* tk, token** t, int n){
    ureg rem = tk->token_buffer_end - *t;
    if (rem>=n){
        *t+=n;
    }
    else{
        *t = tk->token_buffer + (n - rem);/*% TK_TOKEN_BUFFER_SIZE*/
    }
}
static inline void tk_dec_iter(tokenizer* tk, token** t){
    (*t)--;
    if(*t == tk->token_buffer){
        *t = tk->token_buffer_end - 1;
    }
}
static inline void tk_dec_iter_n(tokenizer* tk, token** t, int n){
    ureg rem = *t - tk->token_buffer;
    if (rem>=n){
        *t-=n;
    }
    else{
        *t = tk->token_buffer_end - (n - rem); /*% TK_TOKEN_BUFFER_SIZE*/
    }
}
token* tk_peek_nth(tokenizer* tk, int n){
    token* t = tk->loaded_tokens_start;
    if(tk->loaded_tokens_head < tk->loaded_tokens_start){
        int rem = tk->token_buffer_end - t;
        if(rem >= n) return t + n;
        n -= rem;
        t = tk->token_buffer;
    }
    int rem = t - tk->loaded_tokens_head;
    if(rem>=n){
        return t + n;
    }
    else{
        n -= rem;
        while(n > 1){
            if(!tk_load(tk)) return NULL;
        }
        return tk_load(tk);
    }
}
token* tk_peek(tokenizer* tk){
     if(tk->loaded_tokens_start != tk->loaded_tokens_head) {
        return tk->loaded_tokens_start;
     }
     else{
         return tk_load(tk);
     }
}
token* tk_peek_2nd(tokenizer* p){return tk_peek_nth(p, 2);}
token* tk_peek_3rd(tokenizer* p){return tk_peek_nth(p, 3);}
void tk_void(tokenizer* tk){
    tk_inc_iter(tk, &tk->loaded_tokens_start);
}
void tk_void_n(tokenizer* tk, int n){
    tk_inc_iter_n(tk, &tk->loaded_tokens_start, n);
}

int tk_init(tokenizer* t){
    int r = dbuffer_init_with_capacity(&t->file_buffer, 8192);
    if(r) return r;
    t->file = NULL;
    t->token_buffer_end = t->token_buffer + TK_TOKEN_BUFFER_SIZE;
    return 0;
}
void tk_fin(tokenizer* t){
    dbuffer_fin(&t->file_buffer);
    if(t->file != NULL)tk_close_file(t);
}
char peek_char_holding(tokenizer* tk, char** hold){

}
char tk_peek_char(tokenizer* tk){
    if(tk->file_buffer_pos != tk->file_buffer.head){
        return tk->file_buffer_pos;
    }
    else{
    }
}
void tk_void_char_peak(tokenizer* tk){
    tk->file_buffer_pos++;
}
void _tk_load(tokenizer* tk, bool store_file_pos)
{
    token* tok = tk->loaded_tokens_head;
    tk_inc_iter(tk, &tk->loaded_tokens_head);   
    token* next = tk->loaded_tokens_head; 
    next->line = tok->line;
    char curr = peek_char(tk);
    while (true) {   
        void_char_peek(tk);
        switch(curr){
            case '\0': tok->type = TT_EOF; return;
            case '$': tok->type = TT_DOLLAR; break;
            case '(': tok->type = TT_PAREN_OPEN; break;
            case ')': tok->type = TT_PAREN_CLOSE; break;
            case '{': tok->type = TT_BRACE_OPEN; break;
            case '}': tok->type = TT_BRACE_CLOSE; break;
            case '[': tok->type = TT_BRACKET_OPEN; break;
            case ']': tok->type = TT_BRACKET_CLOSE; break;
            case ',': tok->type = TT_COMMA; break;
            case ';': tok->type = TT_SEMICOLON; break;
            case '.': tok->type = TT_DOT; break;
            case '\t': {
                curr = peek_char(tk);
                while(curr == '\t'){
                    void_char_peek(tk);
                    curr = peek_char(tk);
                    tok->column++; //should this be 2/4/8? 
                }
                continue;
            }
            case ' ': {
                curr = peek_char(tk);
                tok->column++;
                continue;
            }
            case '\n':{
                curr = peek_char(tk);
                tok->line++;
                next->line = tok->line;
                tok->column=0;
                continue;
            }
            case ':':{
                char peek = peek_char(tk);
                if(peek == ':'){
                    void_char_peek(tk);
                    tok->type = TT_DOUBLE_COLON;
                    next->column = tok->column+2;
                    return;
                }
                else{
                    tok->type = TT_COLON;
                    break;
                }
            }
            case '*': {
                char peek = peek_char(tk);
                if(peek == '=') {
                    void_char_peek(tk);
                    tok->type = TT_STAR_EQUALS;
                    next->column = tok->column+2;
                    return;
                }
                else{
                    tok->type = TT_STAR;
                    break;
                }
            }
            case '+': {
                char peek = peek_char(tk);
                if(peek == '+') {
                    void_char_peek(tk);
                    tok->type = TT_DOUBLE_PLUS;
                    next->column = tok->column+2;
                    return;
                }
                else if(peek == '='){
                    void_char_peek(tk);
                    tok->type = TT_PLUS_EQUALS;
                    next->column = tok->column+2;
                    return;
                }
                else{
                    tok->type = TT_PLUS;
                    break;
                }
            }
            case '-': {
                char peek = peek_char(tk);
                if(peek == '-') {
                    void_char_peek(tk);
                    tok->type = TT_DOUBLE_MINUS;
                    next->column = tok->column+2;
                    return;
                }
                else if(peek == '='){
                    void_char_peek(tk);
                    tok->type = TT_MINUS_EQUALS;
                    next->column = tok->column+2;
                    return;
                }
                else if(peek == '>'){
                    void_char_peek(tk);
                    tok->type = TT_ARROW;
                    next->column = tok->column+2;
                    return;
                }
                else{
                    tok->type = TT_MINUS;
                    break;
                }
            }
            case '!': {
                char peek = peek_char(tk);
                if(peek == '=') {
                    void_char_peek(tk);
                    tok->type = TT_EXCLAMATION_MARK_EQUALS;
                    next->column = tok->column+2;
                    return;
                }
                else{
                    tok->type = TT_EXCLAMATION_MARK;
                    break;
                }
            }
            case '|': {
                char peek = peek_char(tk);
                if(peek == '|') {
                    void_char_peek(tk);
                    tok->type = TT_DOUBLE_PIPE;
                    next->column = tok->column+2;
                    return;
                }
                else if(peek == '='){
                    void_char_peek(tk);
                    tok->type = TT_PIPE_EQUALS;
                    next->column = tok->column+2;
                    return;
                }
                else{
                    tok->type = TT_PIPE;
                    break;
                }
            }
            case '&': {
                char peek = peek_char(tk);
                if(peek == '&') {
                    void_char_peek(tk);
                    tok->type = TT_DOUBLE_AND;
                    next->column = tok->column+2;
                    return;
                }
                else if(peek == '='){
                    void_char_peek(tk);
                    tok->type = TT_AND_EQUALS;
                    next->column = tok->column+2;
                    return;
                }
                else{
                    tok->type = TT_AND;
                    break;
                }
            }
            case '^': {
                char peek = peek_char(tk);
                if(peek == '^') {
                    void_char_peek(tk);
                    tok->type = TT_DOUBLE_CARET;
                    next->column = tok->column+2;
                    return;
                }
                else if(peek == '='){
                    void_char_peek(tk);
                    tok->type = TT_CARET_EQUALS;
                    next->column = tok->column+2;
                    return;
                }
                else{
                    tok->type = TT_CARET;
                    break;
                }
            } return;
            case '~': {
                char peek = peek_char(tk);
                if(peek == '=') {
                    void_char_peek(tk);
                    tok->type = TT_TILDE_EQUALS;
                    next->column = tok->column+2;
                    return;
                }
                else{
                    tok->type = TT_TILDE;
                    break;
                }
            }
            case '=': {
                char peek = peek_char(tk);
                if(peek == '=') {
                    void_char_peek(tk);
                    tok->type = TT_DOUBLE_EQUALS;
                    next->column = tok->column+2;
                    return;
                }
                else{
                    tok->type = TT_EQUALS;
                    break;
                }
            }
            case '/': {
                char peek = peek_char(tk);
                if(peek == '/'){
                    do{
                        void_char_peek(tk);
                        curr = peek_char(tk);
                        while(curr == '\\'){
                            void_char_peek(tk);
                            curr = peek_char(tk);
                            comment_assert_neof(tk, tok, curr);
                            void_char_peek(tk);
                            curr = peek_char(tk);
                            comment_assert_neof(tk, tok, curr);
                        }
                    }while(curr != '\n' && curr != '\0');
                    void_char_peek(tk);
                    if(curr == '\n'){
                        tok->line++;
                        next->line = tok->line;
                        tok->column = 0;
                        continue;
                    }
                    tok->type = TT_EOF;
                    return;
                }
                if(peek == '*'){
                    tok->column+=2;
                    void_char_peek(tk);
                    curr = peek_char(tk);
                    do{
                        do{
                            tok->column++;
                            if(curr == '\\'){
                                void_char_peek(tk);
                                curr = peek_char(tk);
                                comment_assert_neof(tk, tok, curr);
                                tok->column++;
                            }
                            if(curr == '\n'){
                                tok->line++;
                                next->line = tok->line;
                                tok->column = 0;
                            }
                            comment_assert_neof(tk, tok, curr);
                            void_char_peek(tk);
                            curr = peek_char(tk);
                        }while(curr != '*');
                        tok->column++;
                        void_char_peek(tk);
                        peek = peek_char(tk);
                    }while(peek != '/');
                    void_char_peek(tk);
                    tok->column++;
                    continue;
                }
                if(peek == '='){
                    void_char_peek(tk);
                    tok->type = TT_SLASH_EQUALS;
                    next->column= tok->column + 2;
                    return;
                }
                else{
                    tok->type = TT_SLASH;
                    break;
                }
            }
            case '%': {
                char peek = peek_char(tk);
                if(peek == '='){
                    void_char_peek(tk);
                    tok->type = TT_PERCENT_EQUALS;
                    next->column = tok->column + 2;
                    return;
                }
                else{
                    tok->type = TT_PERCENT;
                    break;
                }
            }
            case '#': {
                char peek = peek_char(tk);
                if(peek == '#'){
                    void_char_peek(tk);
                    tok->type = TT_DOUBLE_HASH;
                    next->column = tok->column+2;
                    return;
                }
                else{
                    tok->type = TT_HASH;
                    break;
                }
            }
            case '<': {
                char peek = peek_char(tk);
                if(peek == '<'){
                    void_char_peek(tk);
                    peek = peek_char(tk);
                    if(peek == '='){
                        void_char_peek(tk);
                        tok->type = TT_DOUBLE_LESS_THAN_EQUALS;
                        next->column = tok->column+3;
                        return;
                    }
                    else{
                        tok->type = TT_DOUBLE_LESS_THAN;
                        next->column = tok->column+2;
                        return;
                    }
                }
                else if(peek == '='){
                    void_char_peek(tk);
                    peek = peek_char(tk);
                    if(peek == '='){
                        void_char_peek(tk);
                        tok->type = TT_LEFT_ARROW;
                        next->column = tok->column+3;
                        return;
                    }
                    else{
                        tok->type = TT_LESS_THAN_EQUALS;
                        next->column = tok->column+2;
                        return;
                    }

                }
                else{
                    tok->type = TT_LESS_THAN;
                    break;
                }
            } return;
            case '>': {
                char peek = peek_char(tk);
                if(peek == '>'){
                    void_char_peek(tk);
                    peek = peek_char(tk);
                    if(peek == '='){
                        void_char_peek(tk);
                        tok->type = TT_DOUBLE_GREATER_THAN_EQUALS;
                        next->column = tok->column+3;
                        return;
                    }
                    else{
                        tok->type = TT_DOUBLE_GREATER_THAN;
                        next->column = tok->column+2;
                        return;
                    }
                }
                else if(peek == '='){
                    tok->type = TT_GREATER_THAN_EQUALS;
                    next->column = tok->column+2;
                    return;
                }
                else{
                    tok->type = TT_GREATER_THAN;
                    break;
                }
            } return;
            case '\'':{
                char* str_start = tk->file_buffer_pos;
                curr = peek_char_holding(tk, &str_start);
                do{
                    next->column++;
                    assert_neof(tk, tok, curr);
                    if(curr == '\\'){
                        //TODO: think about converting escaped chars
                        void_char_peek(tk);
                        curr = peek_char_holding(tk, &str_start);
                        assert_neof(tk, tok, curr);
                        next->column++;
                    }
                    if(curr == '\n'){
                        curr = peek_char_holding(tk, &str_start);
                        assert_neof(tk, tok, curr);
                    }
                    void_char_peek(tk);
                    curr = peek_char_holding(tk, &str_start);
                }while(curr != '\'');
                tok->str.start = str_start;
                tok->str.end = tk->file_buffer_pos;
                void_char_peek(tk);
                tok->type = TT_BINARY_LITERAL;
            } return;
            case '\"':{
                char* str_start = tk->file_buffer_pos;
                do{
                    next->column++;
                    curr = peek_char_holding(tk, &str_start);
                    assert_neof(tk, tok, curr);
                    if(curr == '\\'){
                        //TODO: think about converting escaped chars
                        void_char_peek(tk);
                        curr = peek_char_holding(tk, &str_start);
                        assert_neof(tk, tok, curr);
                        void_char_peek(tk);
                        curr = peek_char_holding_(tk, &str_start);
                        assert_neof(tk, tok, curr);
                        next->column+=2;
                    }
                    if(curr == '\n'){
                        curr = peek_char_holding(tk, &str_start);
                        assert_neof(tk, tok, curr);
                    }
                    void_char_peek(tk);
                }while(curr != '\"');
                tok->str.start = str_start;
                tok->str.end = tk->file_buffer_pos-1;
                tok->type = TT_LITERAL;
            } return;
            case 'a':case 'b':case 'c':case 'd':case 'e':case 'f':case 'g':case 'h':
            case 'i':case 'j':case 'k':case 'l':case 'm':case 'n':case 'o':case 'p':
            case 'q':case 'r':case 's':case 't':case 'u':case 'v':case 'w':case 'x':
            case 'y':case 'z':
            case 'A':case 'B':case 'C':case 'D':case 'E':case 'F':case 'G':case 'H':
            case 'I':case 'J':case 'K':case 'L':case 'M':case 'N':case 'O':case 'P':
            case 'Q':case 'R':case 'S':case 'T':case 'U':case 'V':case 'W':case 'X':
            case 'Y':case 'Z':
            case '_':
            {
                char* str_start = tk->file_buffer_pos -1;
                curr = peek_char_holding(tk, &str_start);
                while((curr >= 'a' && curr <= 'z') ||
                    (curr >= 'A' && curr <= 'Z') ||
                    (curr >= '0' && curr <= '9') ||
                    (curr == '_'))
                {
                    void_char_peek(tk);
                    curr = peek_char_holding(tk, &str_start);
                }
                tok->str.start = str_start;
                tok->str.end = tk->file_buffer_pos;
                next->column = tok->column + tk->file_buffer_pos - str_start;
                tok->type = TT_STRING;
                return;
            }
            case '0':
            case '1':
            case '2':
            case '3':
            case '4':
            case '5':
            case '6':
            case '7':
            case '8':
            case '9':
            {
                char* str_start = tk->file_buffer_pos-1;
                curr = peek_char_holding(tk, &str_start);
                while(curr >= '0' && curr <= '9'){
                    void_char_peek(tk);
                    curr = peek_char_holding(tk, &str_start);
                assert_neof(tk, tok, curr);
                }
                tok->str.start = str_start;
                tok->str.end = tk->file_buffer_pos;
                next->column = tok->column + tk->file_buffer_pos - str_start;
                tok->type = TT_NUMBER;
            } return;
            default:{
                tk_tokenizing_error(tk, tok,0, "tokenizing error: unknown token");
            }
        }
        next->column = tok->column+1;
    }
}
