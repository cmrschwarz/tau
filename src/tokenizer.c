#include "tokenizer.h"
#include "utils/math_utils.h"
#include "tauc.h"
#include "error_log.h"

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
    int rem = tk->loaded_tokens_head - t;
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
    if(t) tk_void(tk); //TODO: is this if required?
    return t;
}

static inline int tk_load_file_buffer(tokenizer* tk, char** holding){
    token* t = tk->loaded_tokens_start;  
    ureg size_to_keep = 0;
    while (t != tk->loaded_tokens_head){
        if(token_has_string(t))size_to_keep += string_len(t->str);
        tk_inc_iter(tk, &t);
    }
    if(holding){
        size_to_keep += ptrdiff(tk->file_buffer_pos, *holding);
    }
    ureg buff_size = ptrdiff(tk->file_buffer_end, tk->file_buffer_start);
    memblock b;
    bool realloc = buff_size - size_to_keep < TK_MIN_FILE_READ_SIZE; 
    if(realloc){
        buff_size *= 2;   
        if(tal_alloc(&tk->tc->tal, buff_size, &b)){
            error_log_report_allocation_failiure(&tk->tc->error_log);
            return -1;
        }
        ptrswap(&b.start, (void**)&tk->file_buffer_start);
        ptrswap(&b.end, (void**)&tk->file_buffer_end);
    }
    tk->file_buffer_head = tk->file_buffer_start;
    t = tk->loaded_tokens_start;  
    while (t != tk->loaded_tokens_head){
        if(token_has_string(t)){
            ureg slen = string_len(t->str); 
            memcpy(tk->file_buffer_head, t->str.start, slen);
            tk->file_buffer_head = ptradd(tk->file_buffer_head, slen);
        }
        tk_inc_iter(tk, &t);
    }
    if(holding){
        ureg slen = ptrdiff(tk->file_buffer_pos, *holding);
        memcpy(tk->file_buffer_head, *holding, slen);
        *holding = tk->file_buffer_head;
        tk->file_buffer_head = ptradd(tk->file_buffer_head, slen);
    }
    if(realloc){
        tal_free(&tk->tc->tal, &b);
    }
    tk->file_buffer_pos = tk->file_buffer_head;
    ureg siz = fread(tk->file_buffer_head, 1, buff_size - size_to_keep, tk->file_stream);
    if (siz == 0){
        if (ferror(tk->file_stream)) return -1;
        *tk->file_buffer_pos = '\0';
        tk->file_buffer_head++; 
        tk->status = STATUS_EOF;
    }
    else{
        tk->file_buffer_head+= siz;
    }
    return 0;
}
static inline char tk_peek_char_holding(tokenizer* tk, char** hold){
    if(tk->file_buffer_pos == tk->file_buffer_head){
        if(tk_load_file_buffer(tk, hold)){
            tk->status = STATUS_IO_ERROR;
            return '\0';
        }
    }
    char r = *tk->file_buffer_pos;
    return r;
}
static inline char tk_peek_char(tokenizer* tk){
    return tk_peek_char_holding(tk, NULL);
}
static inline void tk_void_char_peek(tokenizer* tk){
    tk->file_buffer_pos++;
}

int tk_init(tokenizer* tk, thread_context* tc){
    memblock b;
    if(tal_alloc(&tk->tc->tal, allocator_get_segment_size() * 8, &b)) return-1;
    tk->tc = tc;
    tk->file_buffer_start = b.start;
    tk->file_buffer_end = b.end;
    tk->token_buffer_end = tk->token_buffer + TK_TOKEN_BUFFER_SIZE;
    tk->loaded_tokens_start = tk->token_buffer; //head is set on open_file
    tk->file_buffer_pos = tk->file_buffer_start;
    return 0;
}
void tk_fin(tokenizer* tk){
    memblock b;
    b.start = tk->file_buffer_start;
    b.end = tk->file_buffer_end;
    tal_free(&tk->tc->tal, &b);
    if(tk->file != NULL)tk_close_file(tk);
}

static inline int tk_open_stream(tokenizer* tk, file* f, FILE* stream){
    tk->file = f;
    tk->file_stream = stream;
    tk->file_buffer_pos = tk->file_buffer_start;
    tk->loaded_tokens_head = tk->loaded_tokens_start;
    tk->loaded_tokens_start->start = 0;
    if(tk_load_file_buffer(tk, NULL)){
        fclose(tk->file_stream);
        return -1;
    }
    tk->status = STATUS_OK;
    return 0;
}
int tk_open_stdin(tokenizer* tk, file* f){
    return tk_open_stream(tk, f, stdin);
}
int tk_open_file(tokenizer* tk, file* f){
    char* path_str = string_to_cstr(f->path);
    if(!path_str)return -1;
    FILE* fs = fopen(path_str, "r");
    string_free_cstr(path_str);
    if(!fs) return -1;
    return tk_open_stream(tk, f, fs);
}
int tk_close_file(tokenizer* tk){
    return fclose(tk->file_stream);
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
                tok->start++; //should this be 2/4/8? 
                continue;
            }
            case ' ': {
                curr = tk_peek_char(tk);
                tok->start++;
                continue;
            }
            case '\n':{
                curr = tk_peek_char(tk);
                tok->start++;
                src_map_add_line(&tk->file->src_map, tk->tc, tok->start);
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
                    tok->start+=2;
                    tk_void_char_peek(tk);
                    do{
                        curr = tk_peek_char(tk);
                        tk_void_char_peek(tk);
                        tok->start++;
                        //THINK: should backslash newline extend the comment?
                    }while(curr != '\n' && curr != '\0');
                    if(curr == '\n'){
                        src_map_add_line(&tk->file->src_map, tk->tc, tok->start);
                        continue;
                    }
                    else{
                        if(tk->status == STATUS_EOF){
                            tok->start--;
                            tok->type = TT_EOF;
                            goto exit_1_char;
                        }
                        else{
                            //TODO: print error
                            tk_dec_iter(tk, &tk->loaded_tokens_start);
                            return NULL;
                        }
                    }
                }
                if(peek == '*'){
                    tk_void_char_peek(tk);
                    tok->start+=2;
                    do{
                        do{
                            curr = tk_peek_char(tk);
                            tk_void_char_peek(tk);
                            tok->start++;
                            if(curr == '\n'){
                                src_map_add_line(&tk->file->src_map, tk->tc, tok->start);
                            }
                            if(curr == '\0')return tk_load_error_eof(tk);
                            curr = tk_peek_char(tk);
                        }while(curr != '*');
                        curr = tk_peek_char(tk);
                        tk_void_char_peek(tk);
                        tok->start++;
                        if(curr == '\0')return tk_load_error_eof(tk);
                    }while(curr != '/');
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
                char* str_start = tk->file_buffer_pos - 1;
                do{
                    curr = tk_peek_char_holding(tk, &str_start);
                    tk_void_char_peek(tk);
                    if(curr == '\0')return tk_load_error_eof(tk);
                    if(curr == '\\'){
                        //TODO: think about converting escaped chars
                        curr = tk_peek_char_holding(tk, &str_start);
                        tk_void_char_peek(tk);
                        if(curr == '\0')return tk_load_error_eof(tk);
                    }
                    if(curr == '\n'){
                        src_map_add_line(
                            &tk->file->src_map,
                            tk->tc,
                            tok->start + ptrdiff(tk->file_buffer_pos, str_start)
                        );
                    }
                }while(curr != '\'');
                tok->type = TT_BINARY_LITERAL;
                tok->str.start = str_start + 1;
                tok->str.end = tk->file_buffer_pos - 1;
                tok->end = tok->start + ptrdiff(tk->file_buffer_pos, str_start);
                next->start = tok->end;
                return tok;
            }
            case '"':{
                char* str_start = tk->file_buffer_pos - 1;
                do{
                    curr = tk_peek_char_holding(tk, &str_start);
                    tk_void_char_peek(tk);
                    if(curr == '\0')return tk_load_error_eof(tk);
                    if(curr == '\\'){
                        //TODO: think about converting escaped chars
                        curr = tk_peek_char_holding(tk, &str_start);
                        tk_void_char_peek(tk);
                        if(curr == '\0')return tk_load_error_eof(tk);
                    }
                    if(curr == '\n'){
                        src_map_add_line(
                            &tk->file->src_map,
                            tk->tc,
                            tok->start + ptrdiff(tk->file_buffer_pos, str_start)
                        );
                    }
                }while(curr != '"');
                tok->type = TT_BINARY_LITERAL;
                tok->str.start = str_start + 1;
                tok->str.end = tk->file_buffer_pos - 1;
                tok->end = tok->start + ptrdiff(tk->file_buffer_pos, str_start);
                next->start = tok->end;
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
                char* str_start = tk->file_buffer_pos - 1;
                curr = tk_peek_char_holding(tk, &str_start);
                while(
                    (curr >= 'a' && curr <= 'z') ||
                    (curr >= 'A' && curr <= 'Z') ||
                    (curr >= '0' && curr <= '9') ||
                    (curr == '_')
                ){
                    tk_void_char_peek(tk);
                    curr = tk_peek_char_holding(tk, &str_start);
                }
                tok->type = TT_STRING;
                tok->str.start = str_start;
                tok->str.end = tk->file_buffer_pos;
                tok->end = tok->start + ptrdiff(tk->file_buffer_pos, str_start);
                next->start = tok->end;
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
                char* str_start = tk->file_buffer_pos - 1;
                curr = tk_peek_char_holding(tk, &str_start);
                while(curr >= '0' && curr <= '9'){
                    tk_void_char_peek(tk);
                    curr = tk_peek_char_holding(tk, &str_start);
                }
                tok->type = TT_NUMBER;
                tok->str.start = str_start;
                tok->str.end = tk->file_buffer_pos;
                tok->end = tok->start + ptrdiff(tk->file_buffer_pos, str_start);
                next->start = tok->end;
                return tok;
            }
            default:{
                //TODO: print error here
                tk_dec_iter(tk, &tk->loaded_tokens_start);
                return NULL;
            }
        }
    }
exit_1_char:
    tok->end = tok->start + 1;
    next->start = tok->end;
    return tok;
exit_2_char:
    tok->end = tok->start + 2;
    next->start = tok->end;
    return tok;
exit_3_char:
    tok->end = tok->start + 3;
    next->start = tok->end;
    return tok;
}
