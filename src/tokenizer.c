#include "tokenizer.h"
#include "error_log.h"
#include "file_map.h"
#include "tauc.h"
#include "utils/math_utils.h"
#include "utils/panic.h"

static token* tk_load(tokenizer* tk);

static inline void tk_inc_iter(tokenizer* tk, token** t)
{
    (*t)++;
    if (*t == tk->token_buffer_end) {
        *t = tk->token_buffer;
    }
}
static inline void tk_inc_iter_n(tokenizer* tk, token** t, int n)
{
    ureg rem = tk->token_buffer_end - *t;
    if (rem > n) {
        *t += n;
    }
    else {
        *t = tk->token_buffer + (n - rem); /*% TK_TOKEN_BUFFER_SIZE*/
    }
}
static inline void tk_dec_iter(tokenizer* tk, token** t)
{
    (*t)--;
    if (*t == tk->token_buffer) {
        *t = tk->token_buffer_end - 1;
    }
}
static inline void tk_dec_iter_n(tokenizer* tk, token** t, int n)
{
    ureg rem = *t - tk->token_buffer;
    if (rem >= n) {
        *t -= n;
    }
    else {
        *t = tk->token_buffer_end - (n - rem); /*% TK_TOKEN_BUFFER_SIZE*/
    }
}
token* tk_peek_nth(tokenizer* tk, int n)
{
    token* t = tk->loaded_tokens_start;
    if (tk->loaded_tokens_head < tk->loaded_tokens_start) {
        int rem = tk->token_buffer_end - t;
        if (rem >= n) return t + n - 1;
        n -= rem;
        t = tk->token_buffer;
    }
    int rem = tk->loaded_tokens_head - t;
    if (rem >= n) {
        return t + n - 1;
    }
    else {
        n -= rem;
        while (n > 1) {
            if (!tk_load(tk)) return NULL;
            n--;
        }
        return tk_load(tk);
    }
}
token* tk_peek(tokenizer* tk)
{
    if (tk->loaded_tokens_start != tk->loaded_tokens_head) {
        return tk->loaded_tokens_start;
    }
    return tk_load(tk);
}

token* tk_peek_2nd(tokenizer* tk)
{
    return tk_peek_nth(tk, 2);
}
token* tk_peek_3rd(tokenizer* tk)
{
    return tk_peek_nth(tk, 3);
}
void tk_void(tokenizer* tk)
{
    tk_inc_iter(tk, &tk->loaded_tokens_start);
}
void tk_void_n(tokenizer* tk, int n)
{
    tk_inc_iter_n(tk, &tk->loaded_tokens_start, n);
}
token* tk_consume(tokenizer* tk)
{
    token* t = tk_peek(tk);
    if (t) tk_void(tk); // PERF: is the if required?
    return t;
}

static inline int tk_load_file_buffer(tokenizer* tk, char** holding)
{
    token* t = tk->loaded_tokens_start;
    ureg size_to_keep = 0;
    while (t != tk->loaded_tokens_head) {
        if (token_has_string(t)) size_to_keep += string_len(t->str);
        tk_inc_iter(tk, &t);
    }
    if (holding) {
        size_to_keep += ptrdiff(tk->file_buffer_pos, *holding);
    }
    ureg buff_size = ptrdiff(tk->file_buffer_end, tk->file_buffer_start);
    void* old_buff = NULL;
    if (buff_size - size_to_keep < TK_MIN_FILE_READ_SIZE) {
        buff_size *= 2;
        old_buff = tk->file_buffer_start;
        tk->file_buffer_start = tmalloc(buff_size);
        if (!tk->file_buffer_start) {
            error_log_report_allocation_failiure(&tk->tc->error_log);
            return -1;
        }
        tk->file_buffer_end = ptradd(tk->file_buffer_start, buff_size);
    }
    tk->file_buffer_head = tk->file_buffer_start;
    t = tk->loaded_tokens_start;
    while (t != tk->loaded_tokens_head) {
        if (token_has_string(t)) {
            ureg slen = string_len(t->str);
            memcpy(tk->file_buffer_head, t->str.start, slen);
            tk->file_buffer_head = (char*)ptradd(tk->file_buffer_head, slen);
        }
        tk_inc_iter(tk, &t);
    }
    if (holding) {
        ureg slen = ptrdiff(tk->file_buffer_pos, *holding);
        memcpy(tk->file_buffer_head, *holding, slen);
        *holding = tk->file_buffer_head;
        tk->file_buffer_head = (char*)ptradd(tk->file_buffer_head, slen);
    }
    if (old_buff) {
        tfree(old_buff);
    }
    tk->file_buffer_pos = tk->file_buffer_head;
    ureg siz = fread(
        tk->file_buffer_head, 1, buff_size - size_to_keep,
        tk->file->file_stream);
    if (siz == 0) {
        if (ferror(tk->file->file_stream)) {
            tk->status = TK_STATUS_IO_ERROR;
            error* e =
                (error*)error_log_alloc(&tk->tc->error_log, sizeof(error));
            if (!e) return ERR;
            e->file = tk->file;
            e->stage = ES_TOKENIZER;
            e->type = ET_ERROR;
            e->position = tk->loaded_tokens_head->start;
            e->message = "file io error";
            error_log_report(&tk->tc->error_log, e);
            return ERR;
        }
        if (tk->status == TK_STATUS_EOF) return 0;
        *tk->file_buffer_pos = '\0';
        tk->file_buffer_head++;
        tk->status = TK_STATUS_EOF;
    }
    else {
        if (tk->status == TK_STATUS_EOF) tk->status = TK_STATUS_OK;
        tk->file_buffer_head += siz;
    }
    return 0;
}
static inline char tk_peek_char_holding(tokenizer* tk, char** hold)
{
    if (tk->file_buffer_pos == tk->file_buffer_head) {
        if (tk_load_file_buffer(tk, hold)) {
            tk->status = TK_STATUS_IO_ERROR;
            return '\0';
        }
    }
    char r = *tk->file_buffer_pos;
    return r;
}
static inline char tk_peek_char(tokenizer* tk)
{
    return tk_peek_char_holding(tk, NULL);
}
static inline void tk_void_char_peek(tokenizer* tk)
{
    tk->file_buffer_pos++;
}
static inline char tk_consume_char(tokenizer* tk)
{
    char c = tk_peek_char_holding(tk, NULL);
    tk_void_char_peek(tk);
    return c;
}

int tk_init(tokenizer* tk, thread_context* tc)
{
    tk->tc = tc;
    ureg size = plattform_get_page_size() * 8;
    tk->file_buffer_start = tmalloc(size);
    if (!tk->file_buffer_start) return -1;
    tk->file_buffer_end = ptradd(tk->file_buffer_start, size);
    tk->token_buffer_end = tk->token_buffer + TK_TOKEN_BUFFER_SIZE;
    tk->loaded_tokens_start = tk->token_buffer; // head is set on open_file
    return 0;
}
void tk_fin(tokenizer* tk)
{
    tfree(tk->file_buffer_start);
    if (tk->file) {
        if (tk->file->file_stream) tk_close_file(tk);
    }
}

int tk_open_stream(tokenizer* tk, src_file* f, FILE* stream)
{
    tk->file = f;
    tk->file->file_stream = stream;
    tk->file_buffer_pos = tk->file_buffer_start;
    tk->file_buffer_head = tk->file_buffer_start;
    tk->loaded_tokens_start->start = 0;
    tk->loaded_tokens_head = tk->loaded_tokens_start;

    if (tk_load_file_buffer(tk, NULL)) {
        fclose(tk->file->file_stream);
        return ERR;
    }
    tk->status = TK_STATUS_OK;
    return OK;
}
int tk_open_file(tokenizer* tk, src_file* f)
{
    if (src_file_start_parse(f, tk->tc)) {
        return ERR;
    }
    char pathbuff[256];
    ureg pathlen = src_file_get_path_len(f);
    char* path;
    if (pathlen < 256) {
        src_file_write_path(f, pathbuff);
        path = pathbuff;
    }
    else {
        path = tmalloc(pathlen + 1);
        src_file_write_path(f, pathbuff);
    }
    FILE* fs = fopen(path, "r");
    if (path != pathbuff) tfree(path);
    if (fs == NULL) {
        return ERR;
    }
    return tk_open_stream(tk, f, fs);
}
int tk_close_file(tokenizer* tk)
{
    int r = fclose(tk->file->file_stream);
    tk->file->file_stream = NULL;
    return r;
}

static inline token* tk_return_head(tokenizer* tk, ureg tok_length)
{
    token* tok = tk->loaded_tokens_head;
    tk_inc_iter(tk, &tk->loaded_tokens_head);
    token* next = tk->loaded_tokens_head;
    tok->end = tok->start + tok_length;
    next->start = tok->end;
    return tok;
}
static inline token*
tk_unterminated_string_error(tokenizer* tk, char* string_start, ureg tok_pos)
{
    ureg start1 = tok_pos + ptrdiff(tk->file_buffer_pos, string_start) - 1;
    ureg start2 = tok_pos;
    error_log_report_annotated_twice(
        &tk->tc->error_log, ES_TOKENIZER, false, "unterminated string",
        tk->file, start1, start1 + 1,
        "reached eof before the string was closed", start2, start2 + 1,
        "string starts here");
    tk->status = TK_STATUS_TOKENIZATION_ERROR;
    return NULL;
}
static token* tk_load(tokenizer* tk)
{
    char curr = tk_peek_char(tk);
    token* tok = tk->loaded_tokens_head;
    while (true) {
        tk_void_char_peek(tk);
        switch (curr) {
            case '\0': {
                if (tk->status == TK_STATUS_IO_ERROR) return NULL;
                tok->type = TT_EOF;
                return tk_return_head(tk, 0);
            }
            case '$': tok->type = TT_DOLLAR; return tk_return_head(tk, 1);
            case '(': tok->type = TT_PAREN_OPEN; return tk_return_head(tk, 1);
            case ')': tok->type = TT_PAREN_CLOSE; return tk_return_head(tk, 1);
            case '{': tok->type = TT_BRACE_OPEN; return tk_return_head(tk, 1);
            case '}': tok->type = TT_BRACE_CLOSE; return tk_return_head(tk, 1);
            case '[': tok->type = TT_BRACKET_OPEN; return tk_return_head(tk, 1);
            case ':': tok->type = TT_COLON; return tk_return_head(tk, 1);
            case '@': tok->type = TT_AT; return tk_return_head(tk, 1);
            case ']':
                tok->type = TT_BRACKET_CLOSE;
                return tk_return_head(tk, 1);
            case ',': tok->type = TT_COMMA; return tk_return_head(tk, 1);
            case ';': tok->type = TT_SEMICOLON; return tk_return_head(tk, 1);
            case '.': tok->type = TT_DOT; return tk_return_head(tk, 1);
            case '\t': {
                curr = tk_peek_char(tk);
                tok->start++; // TODO: make an option to increase by 2/4/8/n
                continue;
            }
            case ' ': {
                curr = tk_peek_char(tk);
                tok->start++;
                continue;
            }
            case '\n': {
                curr = tk_peek_char(tk);
                tok->start++;
                src_map_add_line(&tk->file->src_map, tk->tc, tok->start);
                continue;
            }
            case '*': {
                char peek = tk_peek_char(tk);
                if (peek == '=') {
                    tk_void_char_peek(tk);
                    tok->type = TT_STAR_EQUALS;
                    return tk_return_head(tk, 2);
                }
                else {
                    tok->type = TT_STAR;
                    return tk_return_head(tk, 1);
                }
            }
            case '+': {
                char peek = tk_peek_char(tk);
                if (peek == '+') {
                    tk_void_char_peek(tk);
                    tok->type = TT_DOUBLE_PLUS;
                    return tk_return_head(tk, 2);
                }
                else if (peek == '=') {
                    tk_void_char_peek(tk);
                    tok->type = TT_PLUS_EQUALS;
                    return tk_return_head(tk, 2);
                }
                else {
                    tok->type = TT_PLUS;
                    return tk_return_head(tk, 1);
                }
            }
            case '-': {
                char peek = tk_peek_char(tk);
                if (peek == '-') {
                    tk_void_char_peek(tk);
                    tok->type = TT_DOUBLE_MINUS;
                    return tk_return_head(tk, 2);
                }
                else if (peek == '=') {
                    tk_void_char_peek(tk);
                    tok->type = TT_MINUS_EQUALS;
                    return tk_return_head(tk, 2);
                }
                else if (peek == '>') {
                    tk_void_char_peek(tk);
                    tok->type = TT_ARROW;
                    return tk_return_head(tk, 2);
                }
                else {
                    tok->type = TT_MINUS;
                    return tk_return_head(tk, 1);
                }
            }
            case '!': {
                char peek = tk_peek_char(tk);
                if (peek == '=') {
                    tk_void_char_peek(tk);
                    tok->type = TT_EXCLAMATION_MARK_EQUALS;
                    return tk_return_head(tk, 2);
                }
                else {
                    tok->type = TT_EXCLAMATION_MARK;
                    return tk_return_head(tk, 1);
                }
            }
            case '|': {
                char peek = tk_peek_char(tk);
                if (peek == '|') {
                    tk_void_char_peek(tk);
                    tok->type = TT_DOUBLE_PIPE;
                    return tk_return_head(tk, 2);
                }
                else if (peek == '=') {
                    tk_void_char_peek(tk);
                    tok->type = TT_PIPE_EQUALS;
                    return tk_return_head(tk, 2);
                }
                else {
                    tok->type = TT_PIPE;
                    return tk_return_head(tk, 1);
                }
            }
            case '&': {
                char peek = tk_peek_char(tk);
                if (peek == '&') {
                    tk_void_char_peek(tk);
                    tok->type = TT_DOUBLE_AND;
                    return tk_return_head(tk, 2);
                }
                else if (peek == '=') {
                    tk_void_char_peek(tk);
                    tok->type = TT_AND_EQUALS;
                    return tk_return_head(tk, 2);
                }
                else {
                    tok->type = TT_AND;
                    return tk_return_head(tk, 1);
                }
            }
            case '^': {
                char peek = tk_peek_char(tk);
                if (peek == '^') {
                    tk_void_char_peek(tk);
                    tok->type = TT_DOUBLE_CARET;
                    return tk_return_head(tk, 2);
                }
                else if (peek == '=') {
                    tk_void_char_peek(tk);
                    tok->type = TT_CARET_EQUALS;
                    return tk_return_head(tk, 2);
                }
                else {
                    tok->type = TT_CARET;
                    return tk_return_head(tk, 1);
                }
            }
            case '~': {
                char peek = tk_peek_char(tk);
                if (peek == '=') {
                    tk_void_char_peek(tk);
                    tok->type = TT_TILDE_EQUALS;
                    return tk_return_head(tk, 2);
                }
                else {
                    tok->type = TT_TILDE;
                    return tk_return_head(tk, 1);
                }
            }
            case '=': {
                char peek = tk_peek_char(tk);
                if (peek == '=') {
                    tk_void_char_peek(tk);
                    tok->type = TT_DOUBLE_EQUALS;
                    return tk_return_head(tk, 2);
                }
                else if (peek == '>') {
                    tk_void_char_peek(tk);
                    tok->type = TT_FAT_ARROW;
                    return tk_return_head(tk, 2);
                }
                else {
                    tok->type = TT_EQUALS;
                    return tk_return_head(tk, 1);
                }
            }
            case '/': {
                char peek = tk_peek_char(tk);
                if (peek == '/') {
                    tok->start += 2;
                    tk_void_char_peek(tk);
                    do {
                        curr = tk_peek_char(tk);
                        tk_void_char_peek(tk);
                        tok->start++;
                    } while (curr != '\n' && curr != '\0');
                    if (curr == '\n') {
                        src_map_add_line(
                            &tk->file->src_map, tk->tc, tok->start);
                        curr = tk_peek_char(tk);
                        continue;
                    }
                    else {
                        if (tk->status == TK_STATUS_EOF) {
                            tok->start--;
                            tok->type = TT_EOF;
                            return tk_return_head(tk, 1);
                        }
                        else {
                            // TODO: print error
                            tk_dec_iter(tk, &tk->loaded_tokens_start);
                            return NULL;
                        }
                    }
                }
                if (peek == '*') {
                    ureg comment_start = tok->start;
                    tk_void_char_peek(tk);
                    tok->start += 2;
                    ureg nest_count = 1;
                    do {
                        curr = tk_consume_char(tk);
                        tok->start++;
                        switch (curr) {
                            case '\n': {
                                src_map_add_line(
                                    &tk->file->src_map, tk->tc, tok->start);
                            } break;
                            case '\t': break;
                            case '*': {
                                curr = tk_peek_char(tk);
                                tok->start++;
                                if (curr == '/') {
                                    tk_void_char_peek(tk);
                                    nest_count--;
                                }
                            } break;
                            case '/': {
                                curr = tk_peek_char(tk);
                                tok->start++;
                                if (curr == '*') {
                                    tk_void_char_peek(tk);
                                    nest_count++;
                                }
                            } break;
                            case '\0': {
                                tok->start--;
                                error_log_report_annotated_twice(
                                    &tk->tc->error_log, ES_TOKENIZER, false,
                                    "unterminated block comment", tk->file,
                                    tok->start, tok->start + 1,
                                    "reached eof before the comment was closed",
                                    comment_start, comment_start + 2,
                                    "comment starts here");
                                tk->status = TK_STATUS_TOKENIZATION_ERROR;
                                return NULL;
                            }
                        }
                    } while (nest_count > 0);
                    curr = tk_peek_char(tk);
                    continue;
                }
                if (peek == '=') {
                    tk_void_char_peek(tk);
                    tok->type = TT_SLASH_EQUALS;
                    return tk_return_head(tk, 2);
                }
                else {
                    tok->type = TT_SLASH;
                    return tk_return_head(tk, 1);
                }
            }
            case '%': {
                char peek = tk_peek_char(tk);
                if (peek == '=') {
                    tk_void_char_peek(tk);
                    tok->type = TT_PERCENT_EQUALS;
                    return tk_return_head(tk, 2);
                }
                else {
                    tok->type = TT_PERCENT;
                    return tk_return_head(tk, 1);
                }
            }
            case '#': {
                char peek = tk_peek_char(tk);
                if (peek == '#') {
                    tk_void_char_peek(tk);
                    tok->type = TT_DOUBLE_HASH;
                    return tk_return_head(tk, 2);
                }
                else {
                    tok->type = TT_HASH;
                    return tk_return_head(tk, 1);
                }
            }
            case '<': {
                char peek = tk_peek_char(tk);
                if (peek == '<') {
                    tk_void_char_peek(tk);
                    peek = tk_peek_char(tk);
                    if (peek == '=') {
                        tk_void_char_peek(tk);
                        tok->type = TT_DOUBLE_LESS_THAN_EQUALS;
                        return tk_return_head(tk, 3);
                    }
                    else {
                        tok->type = TT_DOUBLE_LESS_THAN;
                        return tk_return_head(tk, 2);
                    }
                }
                else if (peek == '=') {
                    tk_void_char_peek(tk);
                    tok->type = TT_LESS_THAN_EQUALS;
                    return tk_return_head(tk, 2);
                }
                else {
                    tok->type = TT_LESS_THAN;
                    return tk_return_head(tk, 1);
                }
            }
            case '>': {
                char peek = tk_peek_char(tk);
                if (peek == '>') {
                    tk_void_char_peek(tk);
                    peek = tk_peek_char(tk);
                    if (peek == '=') {
                        tk_void_char_peek(tk);
                        tok->type = TT_DOUBLE_GREATER_THAN_EQUALS;
                        return tk_return_head(tk, 3);
                    }
                    else {
                        tok->type = TT_DOUBLE_GREATER_THAN;
                        return tk_return_head(tk, 2);
                    }
                }
                else if (peek == '=') {
                    tok->type = TT_GREATER_THAN_EQUALS;
                    return tk_return_head(tk, 2);
                }
                else {
                    tok->type = TT_GREATER_THAN;
                    return tk_return_head(tk, 1);
                }
            }
            case '\'': {
                char* str_start = tk->file_buffer_pos - 1;
                do {
                    curr = tk_peek_char_holding(tk, &str_start);
                    tk_void_char_peek(tk);
                    if (curr == '\0') {
                        return tk_unterminated_string_error(
                            tk, str_start, tok->start);
                    }
                    if (curr == '\\') {
                        // TODO: think about converting escaped chars
                        curr = tk_peek_char_holding(tk, &str_start);
                        tk_void_char_peek(tk);
                        if (curr == '\0') {
                            return tk_unterminated_string_error(
                                tk, str_start, tok->start);
                        }
                    }
                    if (curr == '\n') {
                        src_map_add_line(
                            &tk->file->src_map, tk->tc,
                            tok->start +
                                ptrdiff(tk->file_buffer_pos, str_start));
                    }
                } while (curr != '\'');
                tok->type = TT_BINARY_LITERAL;
                tok->str.start = str_start + 1;
                tok->str.end = tk->file_buffer_pos - 1;
                return tk_return_head(
                    tk, ptrdiff(tk->file_buffer_pos, str_start));
            }
            case '"': {
                char* str_start = tk->file_buffer_pos - 1;
                do {
                    curr = tk_peek_char_holding(tk, &str_start);
                    tk_void_char_peek(tk);
                    if (curr == '\0') {
                        return tk_unterminated_string_error(
                            tk, str_start, tok->start);
                    }
                    if (curr == '\\') {
                        // TODO: think about converting escaped chars
                        curr = tk_peek_char_holding(tk, &str_start);
                        tk_void_char_peek(tk);
                        if (curr == '\0') {
                            return tk_unterminated_string_error(
                                tk, str_start, tok->start);
                        }
                    }
                    if (curr == '\n') {
                        src_map_add_line(
                            &tk->file->src_map, tk->tc,
                            tok->start +
                                ptrdiff(tk->file_buffer_pos, str_start));
                    }
                } while (curr != '"');
                tok->type = TT_LITERAL;
                tok->str.start = str_start + 1;
                tok->str.end = tk->file_buffer_pos - 1;
                return tk_return_head(
                    tk, ptrdiff(tk->file_buffer_pos, str_start));
            }
            case 'a':
            case 'b':
            case 'c':
            case 'd':
            case 'e':
            case 'f':
            case 'g':
            case 'h':
            case 'i':
            case 'j':
            case 'k':
            case 'l':
            case 'm':
            case 'n':
            case 'o':
            case 'p':
            case 'q':
            case 'r':
            case 's':
            case 't':
            case 'u':
            case 'v':
            case 'w':
            case 'x':
            case 'y':
            case 'z':
            case 'A':
            case 'B':
            case 'C':
            case 'D':
            case 'E':
            case 'F':
            case 'G':
            case 'H':
            case 'I':
            case 'J':
            case 'K':
            case 'L':
            case 'M':
            case 'N':
            case 'O':
            case 'P':
            case 'Q':
            case 'R':
            case 'S':
            case 'T':
            case 'U':
            case 'V':
            case 'W':
            case 'X':
            case 'Y':
            case 'Z':
            case '_': {
                char* str_start = tk->file_buffer_pos - 1;
                curr = tk_peek_char_holding(tk, &str_start);
                while ((curr >= 'a' && curr <= 'z') ||
                       (curr >= 'A' && curr <= 'Z') ||
                       (curr >= '0' && curr <= '9') || (curr == '_')) {
                    tk_void_char_peek(tk);
                    curr = tk_peek_char_holding(tk, &str_start);
                }
                tok->str.start = str_start;
                tok->str.end = tk->file_buffer_pos;
                tok->type = match_kw(tok->str);
                if (tok->type == TT_NONE) tok->type = TT_STRING;
                return tk_return_head(
                    tk, ptrdiff(tk->file_buffer_pos, str_start));
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
            case '9': {
                char* str_start = tk->file_buffer_pos - 1;
                curr = tk_peek_char_holding(tk, &str_start);
                while (curr >= '0' && curr <= '9') {
                    tk_void_char_peek(tk);
                    curr = tk_peek_char_holding(tk, &str_start);
                }
                tok->type = TT_NUMBER;
                tok->str.start = str_start;
                tok->str.end = tk->file_buffer_pos;
                return tk_return_head(
                    tk, ptrdiff(tk->file_buffer_pos, str_start));
            }
            default: {
                /*
                //TODO: proper unicode handling
                ureg len = get_utf8_seq_len_from_head(curr);
                for(ureg i = 1; i<len;i++){
                    char c = tk_peek_char(tk);
                    if(!is_utf8_continuation(c)){
                        len = i;
                        break;
                    }
                    tk_void_char_peek(tk);
                }
                */
                error_log_report_annotated(
                    &tk->tc->error_log, ES_TOKENIZER, false, "unknown token",
                    tk->file, tok->start, tok->start + 1,
                    "not the start for any valid token");
                return NULL;
            }
        }
    }
    panic("unexpected error in tokenizer");
    return NULL;
}
