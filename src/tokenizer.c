#include "tokenizer.h"

static const int STATUS_OK = 0;
static const int STATUS_IO_ERROR = 0;
static const int STATUS_EOF = 1;

static token* tk_load(tokenizer* tk);

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
            n--;
        }
        return tk_load(tk);
    }
}
token* tk_peek(tokenizer* tk){
    if(tk->loaded_tokens_start != tk->loaded_tokens_head) {
        return tk->loaded_tokens_start;
    }
    return tk_load(tk);
}

token* tk_peek_2nd(tokenizer* p){return tk_peek_nth(p, 2);}
token* tk_peek_3rd(tokenizer* p){return tk_peek_nth(p, 3);}
void tk_void(tokenizer* tk){
    tk_inc_iter(tk, &tk->loaded_tokens_start);
}
void tk_void_n(tokenizer* tk, int n){
    tk_inc_iter_n(tk, &tk->loaded_tokens_start, n);
}
token* tk_consume(tokenizer* tk){
    token* t = tk_peek(tk);
    if(t)tk_void(t); //TODO: is this if required?
    return t;
}
int tk_init(tokenizer* tk, thread_allocator* tal){
    int r = dbuffer_init_with_capacity(
        &tk->file_buffer,
        tal,
        allocator_get_segment_size() * 2
    );
    if(r) return r;
    tk->token_buffer_end = tk->token_buffer + TK_TOKEN_BUFFER_SIZE;
    tk->loaded_tokens_start = tk->token_buffer; //head is set on open_file
    tk->file_buffer_pos = tk->file_buffer.start;
    return 0;
}
void tk_fin(tokenizer* t){
    dbuffer_fin(&t->file_buffer);
    if(t->file != NULL)tk_close_file(t);
}


int tk_open_file(tokenizer* tk, file* f){
    char* path_str = str_to_cstr(f->path);
    if(!path_str)return -1;
    FILE* fs = fopen(path_str, "r");
    str_free_cstr(path_str);
    if(!fs) return -1;
    tk->file = f;
    tk->file_stream = fs;
    dbuffer_clear(&tk->file_buffer);
    tk->loaded_tokens_head = tk->loaded_tokens_start;
    tk->status = STATUS_OK;
    return 0;
}
int tk_close_file(tokenizer* tk){
    return fclose(tk->file_stream);
}
static int tk_load_file_buffer(tokenizer* tk){
      
}

char tk_peek_char(tokenizer* tk){
    if(tk->file_buffer_pos == tk->file_buffer.head){
        if(tk_load_file_buffer(tk)){
            tk->status = STATUS_IO_ERROR;
            return '\0';
        }
    }
    char r = *tk->file_buffer_pos;
    tk->file_buffer_pos++;
    return r;
}
void tk_void_char_peek(tokenizer* tk){
    tk->file_buffer_pos++;
}
static inline token* tk_load_error_eof(tokenizer* tk){
    //TODO: make this print error msg etc.
    //TODO: only call this if the 0 is not part of a binary literal
    tk_dec_iter(tk, &tk->loaded_tokens_head);
    return NULL;  
}
static token* tk_load(tokenizer* tk)
{
    token* tok = tk->loaded_tokens_head; 
    tk_inc_iter(tk, &tk->loaded_tokens_head);   
    token* next = tk->loaded_tokens_head; 
    char curr = tk_peek_char(tk);
    while (true) {   
        tk_void_char_peek(tk);
        switch(curr){
            case '\0': tok->type = TT_EOF; goto exit_1_char;
            case '$': tok->type = TT_DOLLAR; goto exit_1_char;
            case '(': tok->type = TT_PAREN_OPEN; goto exit_1_char;
            case ')': tok->type = TT_PAREN_CLOSE; goto exit_1_char;
            case '{': tok->type = TT_BRACE_OPEN; goto exit_1_char;
            case '}': tok->type = TT_BRACE_CLOSE; goto exit_1_char;
            case '[': tok->type = TT_BRACKET_OPEN; goto exit_1_char;
            case ']': tok->type = TT_BRACKET_CLOSE; goto exit_1_char;
            case ',': tok->type = TT_COMMA; goto exit_1_char;
            case ';': tok->type = TT_SEMICOLON; goto exit_1_char;
            case '.': tok->type = TT_DOT; goto exit_1_char;
            case '\t': {
                curr = tk_peek_char(tk);
                while(curr == '\t'){
                    tk_void_char_peek(tk);
                    curr = tk_peek_char(tk);
                    tok->column++; //should this be 2/4/8? 
                }
                continue;
            }
            case ' ': {
                curr = tk_peek_char(tk);
                tok->column++;
                continue;
            }
            case '\n':{
                curr = tk_peek_char(tk);
                tok->line++;
                tok->column=0;
                continue;
            }
            case ':':{
                char peek = tk_peek_char(tk);
                if(peek == ':'){
                    tk_void_char_peek(tk);
                    tok->type = TT_DOUBLE_COLON;
                    goto exit_2_char;
                }
                else{
                    tok->type = TT_COLON;
                    goto exit_1_char;
                }
            }
            case '*': {
                char peek = tk_peek_char(tk);
                if(peek == '=') {
                    tk_void_char_peek(tk);
                    tok->type = TT_STAR_EQUALS;
                    goto exit_2_char;
                }
                else{
                    tok->type = TT_STAR;
                    goto exit_1_char;
                }
            }
            case '+': {
                char peek = tk_peek_char(tk);
                if(peek == '+') {
                    tk_void_char_peek(tk);
                    tok->type = TT_DOUBLE_PLUS;
                    goto exit_2_char;
                }
                else if(peek == '='){
                    tk_void_char_peek(tk);
                    tok->type = TT_PLUS_EQUALS;
                    goto exit_2_char;
                }
                else{
                    tok->type = TT_PLUS;
                    goto exit_1_char;
                }
            }
            case '-': {
                char peek = tk_peek_char(tk);
                if(peek == '-') {
                    tk_void_char_peek(tk);
                    tok->type = TT_DOUBLE_MINUS;
                    goto exit_2_char;
                }
                else if(peek == '='){
                    tk_void_char_peek(tk);
                    tok->type = TT_MINUS_EQUALS;
                    goto exit_2_char;
                }
                else if(peek == '>'){
                    tk_void_char_peek(tk);
                    tok->type = TT_ARROW;
                    goto exit_2_char;
                }
                else{
                    tok->type = TT_MINUS;
                    goto exit_1_char;
                }
            }
            case '!': {
                char peek = tk_peek_char(tk);
                if(peek == '=') {
                    tk_void_char_peek(tk);
                    tok->type = TT_EXCLAMATION_MARK_EQUALS;
                    goto exit_2_char;
                }
                else{
                    tok->type = TT_EXCLAMATION_MARK;
                    goto exit_1_char;
                }
            }
            case '|': {
                char peek = tk_peek_char(tk);
                if(peek == '|') {
                    tk_void_char_peek(tk);
                    tok->type = TT_DOUBLE_PIPE;
                    goto exit_2_char;
                }
                else if(peek == '='){
                    tk_void_char_peek(tk);
                    tok->type = TT_PIPE_EQUALS;
                    goto exit_2_char;
                }
                else{
                    tok->type = TT_PIPE;
                    goto exit_1_char;
                }
            }
            case '&': {
                char peek = tk_peek_char(tk);
                if(peek == '&') {
                    tk_void_char_peek(tk);
                    tok->type = TT_DOUBLE_AND;
                    goto exit_2_char;
                }
                else if(peek == '='){
                    tk_void_char_peek(tk);
                    tok->type = TT_AND_EQUALS;
                    goto exit_2_char;
                }
                else{
                    tok->type = TT_AND;
                    goto exit_1_char;
                }
            }
            case '^': {
                char peek = tk_peek_char(tk);
                if(peek == '^') {
                    tk_void_char_peek(tk);
                    tok->type = TT_DOUBLE_CARET;
                    goto exit_2_char;
                }
                else if(peek == '='){
                    tk_void_char_peek(tk);
                    tok->type = TT_CARET_EQUALS;
                    goto exit_2_char;
                }
                else{
                    tok->type = TT_CARET;
                    goto exit_1_char;
                }
            } 
            case '~': {
                char peek = tk_peek_char(tk);
                if(peek == '=') {
                    tk_void_char_peek(tk);
                    tok->type = TT_TILDE_EQUALS;
                    goto exit_2_char;
                }
                else{
                    tok->type = TT_TILDE;
                    goto exit_1_char;
                }
            }
            case '=': {
                char peek = tk_peek_char(tk);
                if(peek == '=') {
                    tk_void_char_peek(tk);
                    tok->type = TT_DOUBLE_EQUALS;
                    goto exit_2_char;
                }
                else{
                    tok->type = TT_EQUALS;
                    goto exit_1_char;
                }
            }
            case '/': {
                char peek = tk_peek_char(tk);
                if(peek == '/'){
                    do{
                        tk_void_char_peek(tk);
                        curr = tk_peek_char(tk);
                        while(curr == '\\'){
                            tk_void_char_peek(tk);
                            curr = tk_peek_char(tk);
                            if(curr == '\0')return tk_load_error_eof(tk);
                            tk_void_char_peek(tk);
                            curr = tk_peek_char(tk);
                            if(curr == '\0')return tk_load_error_eof(tk);
                        }
                    }while(curr != '\n' && curr != '\0');
                    tk_void_char_peek(tk);
                    if(curr == '\n'){
                        tok->line++;
                        tok->column = 0;
                        continue;
                    }
                    else{
                        tok->type = TT_EOF;
                        goto exit_1_char;
                    }
                }
                if(peek == '*'){
                    tok->column+=2;
                    tk_void_char_peek(tk);
                    curr = tk_peek_char(tk);
                    do{
                        do{
                            tok->column++;
                            if(curr == '\\'){
                                tk_void_char_peek(tk);
                                curr = tk_peek_char(tk);
                                if(curr == '\0')return tk_load_error_eof(tk);
                                tok->column++;
                            }
                            if(curr == '\n'){
                                tok->line++;
                                tok->column = 0;
                            }
                            if(curr == '\0')return tk_load_error_eof(tk);
                            tk_void_char_peek(tk);
                            curr = tk_peek_char(tk);
                        }while(curr != '*');
                        tok->column++;
                        tk_void_char_peek(tk);
                        peek = tk_peek_char(tk);
                    }while(peek != '/');
                    tk_void_char_peek(tk);
                    tok->column++;
                    continue;
                }
                if(peek == '='){
                    tk_void_char_peek(tk);
                    tok->type = TT_SLASH_EQUALS;
                    goto exit_2_char;
                }
                else{
                    tok->type = TT_SLASH;
                    goto exit_1_char;
                }
            }
            case '%': {
                char peek = tk_peek_char(tk);
                if(peek == '='){
                    tk_void_char_peek(tk);
                    tok->type = TT_PERCENT_EQUALS;
                    goto exit_2_char;
                }
                else{
                    tok->type = TT_PERCENT;
                    goto exit_1_char;
                }
            }
            case '#': {
                char peek = tk_peek_char(tk);
                if(peek == '#'){
                    tk_void_char_peek(tk);
                    tok->type = TT_DOUBLE_HASH;
                    goto exit_2_char;
                }
                else{
                    tok->type = TT_HASH;
                    goto exit_1_char;
                }
            }
            case '<': {
                char peek = tk_peek_char(tk);
                if(peek == '<'){
                    tk_void_char_peek(tk);
                    peek = tk_peek_char(tk);
                    if(peek == '='){
                        tk_void_char_peek(tk);
                        tok->type = TT_DOUBLE_LESS_THAN_EQUALS;
                        goto exit_3_char;
                    }
                    else{
                        tok->type = TT_DOUBLE_LESS_THAN;
                        goto exit_2_char;
                    }
                }
                else if(peek == '='){
                    tk_void_char_peek(tk);
                    peek = tk_peek_char(tk);
                    if(peek == '='){
                        tk_void_char_peek(tk);
                        tok->type = TT_LEFT_ARROW;
                        goto exit_3_char;
                    }
                    else{
                        tok->type = TT_LESS_THAN_EQUALS;
                        goto exit_2_char;
                    }
                }
                else{
                    tok->type = TT_LESS_THAN;
                    goto exit_1_char;
                }
            } 
            case '>': {
                char peek = tk_peek_char(tk);
                if(peek == '>'){
                    tk_void_char_peek(tk);
                    peek = tk_peek_char(tk);
                    if(peek == '='){
                        tk_void_char_peek(tk);
                        tok->type = TT_DOUBLE_GREATER_THAN_EQUALS;
                        goto exit_3_char;
                    }
                    else{
                        tok->type = TT_DOUBLE_GREATER_THAN;
                        goto exit_2_char;
                    }
                }
                else if(peek == '='){
                    tok->type = TT_GREATER_THAN_EQUALS;
                    goto exit_2_char;
                }
                else{
                    tok->type = TT_GREATER_THAN;
                    goto exit_1_char;
                }
            } 
            case '\'':{
                char* str_start = tk->file_buffer_pos;
                curr = tk_peek_char_holding(tk, &str_start);
                next->line = tok->line;
                do{
                    if(curr == '\0')return tk_load_error_eof(tk);
                    if(curr == '\\'){
                        //TODO: think about converting escaped chars
                        tk_void_char_peek(tk);
                        curr = tk_peek_char_holding(tk, &str_start);
                        if(curr == '\0')return tk_load_error_eof(tk);
                    }
                    if(curr == '\n'){
                        next->line++; 
                        curr = tk_peek_char_holding(tk, &str_start);
                        if(curr == '\0')return tk_load_error_eof(tk);
                    }
                    tk_void_char_peek(tk);
                    curr = tk_peek_char_holding(tk, &str_start);
                }while(curr != '\'');
                tk_void_char_peek(tk);
                tok->type = TT_BINARY_LITERAL;
                tok->str.start = str_start;
                tok->str.end = tk->file_buffer_pos;
                next->column = tok->column + tk->file_buffer_pos - str_start;
                return tok;
            }
            case '\"':{
                char* str_start = tk->file_buffer_pos;
                next->line = tok->line;
                do{
                    curr = tk_peek_char_holding(tk, &str_start);
                    if(curr == '\0')return tk_load_error_eof(tk);
                    if(curr == '\\'){
                        //TODO: think about converting escaped chars
                        tk_void_char_peek(tk);
                        curr = tk_peek_char_holding(tk, &str_start);
                        if(curr == '\0')return tk_load_error_eof(tk);
                        tk_void_char_peek(tk);
                        curr = tk_peek_char_holding(tk, &str_start);
                        if(curr == '\0')return tk_load_error_eof(tk);
                    }
                    if(curr == '\n'){
                        next->line++; 
                        curr = tk_peek_char_holding(tk, &str_start);
                        if(curr == '\0')return tk_load_error_eof(tk);
                    }
                    tk_void_char_peek(tk);
                }while(curr != '\"');
                tok->str.start = str_start;
                tok->str.end = tk->file_buffer_pos-1;
                tok->type = TT_LITERAL;
                next->column = tok->column + tk->file_buffer_pos - str_start;
                return tok;
            }
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
                curr = tk_peek_char_holding(tk, &str_start);
                while((curr >= 'a' && curr <= 'z') ||
                    (curr >= 'A' && curr <= 'Z') ||
                    (curr >= '0' && curr <= '9') ||
                    (curr == '_'))
                {
                    tk_void_char_peek(tk);
                    curr = tk_peek_char_holding(tk, &str_start);
                }
                tok->type = TT_STRING;
                tok->str.start = str_start;
                tok->str.end = tk->file_buffer_pos;
                next->line = tok->line; 
                next->column = tok->column + tk->file_buffer_pos - str_start;
                return tok;
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
                curr = tk_peek_char_holding(tk, &str_start);
                while(curr >= '0' && curr <= '9'){
                    tk_void_char_peek(tk);
                    curr = tk_peek_char_holding(tk, &str_start);
                    if(curr == '\0')return tk_load_error_eof(tk);
                }
                tok->type = TT_NUMBER;
                tok->str.start = str_start;
                tok->str.end = tk->file_buffer_pos;
                next->line = tok->line; 
                next->column = tok->column + tk->file_buffer_pos - str_start;
                next->str.start = tok->str.end;
                return tok;
            }
            default:{
                //TODO: print error here
                tk_dec_iter(tk, &tk->loaded_tokens_head);
                return NULL;
            }
        }
    }
exit_1_char:
    next->line = tok->line;
    next->column = tok->column+1;
    return tok;
exit_2_char:
    next->line = tok->line;
    next->column = tok->column+2;
    return tok;
exit_3_char:
    next->line = tok->line;
    next->column = tok->column+3;
    return tok;
}
