#include "lexer.h"
#include "error_log.h"
#include "file_map.h"
#include "tauc.h"
#include "utils/math_utils.h"
#include "utils/panic.h"
#include "thread_context.h"

static token* lx_load(lexer* tk);

static inline void lx_inc_iter(lexer* tk, token** t)
{
    (*t)++;
    if (*t == tk->token_buffer_end) {
        *t = tk->token_buffer;
    }
}
static inline void lx_inc_iter_n(lexer* tk, token** t, int n)
{
    ureg rem = tk->token_buffer_end - *t;
    if (rem > n) {
        *t += n;
    }
    else {
        *t = tk->token_buffer + (n - rem); /*% LX_TOKEN_BUFFER_SIZE*/
    }
}
static inline void lx_dec_iter(lexer* tk, token** t)
{
    (*t)--;
    if (*t == tk->token_buffer) {
        *t = tk->token_buffer_end - 1;
    }
}
static inline void lx_dec_iter_n(lexer* tk, token** t, int n)
{
    ureg rem = *t - tk->token_buffer;
    if (rem >= n) {
        *t -= n;
    }
    else {
        *t = tk->token_buffer_end - (n - rem); /*% LX_TOKEN_BUFFER_SIZE*/
    }
}
token* lx_peek_nth(lexer* tk, int n)
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
    n -= rem;
    while (n > 1) {
        if (!lx_load(tk)) return NULL;
        n--;
    }
    return lx_load(tk);
}
token* lx_peek(lexer* tk)
{
    if (tk->loaded_tokens_start != tk->loaded_tokens_head) {
        return tk->loaded_tokens_start;
    }
    return lx_load(tk);
}

token* lx_peek_2nd(lexer* tk)
{
    return lx_peek_nth(tk, 2);
}
token* lx_peek_3rd(lexer* tk)
{
    return lx_peek_nth(tk, 3);
}
void lx_void(lexer* tk)
{
    lx_inc_iter(tk, &tk->loaded_tokens_start);
}
void lx_void_n(lexer* tk, int n)
{
    lx_inc_iter_n(tk, &tk->loaded_tokens_start, n);
}
token* lx_consume(lexer* tk)
{
    token* t = lx_peek(tk);
    if (t) lx_void(tk); // PERF: is the if required?
    return t;
}

static inline int lx_load_file_buffer(lexer* tk, char** holding)
{
    token* t = tk->loaded_tokens_start;
    ureg size_to_keep = 0;
    while (t != tk->loaded_tokens_head) {
        if (token_has_string(t)) size_to_keep += string_len(t->str);
        lx_inc_iter(tk, &t);
    }
    if (holding) {
        size_to_keep += ptrdiff(tk->file_buffer_pos, *holding);
    }
    ureg buff_size = ptrdiff(tk->file_buffer_end, tk->file_buffer_start);
    void* old_buff = NULL;
    if (buff_size - size_to_keep < LX_MIN_FILE_READ_SIZE) {
        buff_size *= 2;
        old_buff = tk->file_buffer_start;
        tk->file_buffer_start = tmalloc(buff_size);
        if (!tk->file_buffer_start) {
            error_log_report_allocation_failiure(tk->tc->err_log);
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
        lx_inc_iter(tk, &t);
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
            tk->status = LX_STATUS_IO_ERROR;
            error_log_report_simple(
                tk->tc->err_log, ES_TOKENIZER, false, "file io error", tk->file,
                tk->loaded_tokens_head->start);
            return ERR;
        }
        if (tk->status == LX_STATUS_EOF) return 0;
        *tk->file_buffer_pos = '\0';
        tk->file_buffer_head++;
        tk->status = LX_STATUS_EOF;
    }
    else {
        if (tk->status == LX_STATUS_EOF) tk->status = LX_STATUS_OK;
        tk->file_buffer_head += siz;
    }
    return 0;
}
static inline char lx_peek_char_holding(lexer* tk, char** hold)
{
    if (tk->file_buffer_pos == tk->file_buffer_head) {
        if (lx_load_file_buffer(tk, hold)) {
            tk->status = LX_STATUS_IO_ERROR;
            return '\0';
        }
    }
    char r = *tk->file_buffer_pos;
    return r;
}
static inline char lx_peek_char(lexer* tk)
{
    return lx_peek_char_holding(tk, NULL);
}
static inline void lx_void_char_peek(lexer* tk)
{
    tk->file_buffer_pos++;
}
static inline char lx_consume_char(lexer* tk)
{
    char c = lx_peek_char_holding(tk, NULL);
    lx_void_char_peek(tk);
    return c;
}

int lx_init(lexer* tk, thread_context* tc)
{
    tk->tc = tc;
    ureg size = plattform_get_page_size() * 8;
    tk->file_buffer_start = tmalloc(size);
    if (!tk->file_buffer_start) return -1;
    tk->file_buffer_end = ptradd(tk->file_buffer_start, size);
    tk->token_buffer_end = tk->token_buffer + LX_TOKEN_BUFFER_SIZE;
    tk->loaded_tokens_start = tk->token_buffer; // head is set on open_file
    tk->status = LX_STATUS_OK;
    return 0;
}
void lx_fin(lexer* tk)
{
    tfree(tk->file_buffer_start);
}

int lx_open_stream(lexer* tk, src_file* f, FILE* stream)
{
    tk->file = f;
    tk->file->file_stream = stream;
    tk->file_buffer_pos = tk->file_buffer_start;
    tk->file_buffer_head = tk->file_buffer_start;
    tk->loaded_tokens_start->start = 0;
    tk->loaded_tokens_head = tk->loaded_tokens_start;

    if (lx_load_file_buffer(tk, NULL)) {
        lx_close_file(tk);
        return ERR;
    }
    tk->status = LX_STATUS_OK;
    return OK;
}
int lx_open_file(lexer* tk, src_file* f)
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
    return lx_open_stream(tk, f, fs);
}
int lx_close_file(lexer* tk)
{
    int r = fclose(tk->file->file_stream);
    tk->file->file_stream = NULL;
    return r;
}

static inline token* lx_return_head(lexer* tk, ureg tok_length)
{
    token* tok = tk->loaded_tokens_head;
    lx_inc_iter(tk, &tk->loaded_tokens_head);
    token* next = tk->loaded_tokens_head;
    tok->end = tok->start + tok_length;
    next->start = tok->end;
    return tok;
}
static inline token*
lx_unterminated_string_error(lexer* tk, char* string_start, ureg tok_pos)
{
    ureg start1 = tok_pos + ptrdiff(tk->file_buffer_pos, string_start) - 1;
    ureg start2 = tok_pos;
    error_log_report_annotated_twice(
        tk->tc->err_log, ES_TOKENIZER, false, "unterminated string", tk->file,
        start1, start1 + 1, "reached eof before the string was closed",
        tk->file, start2, start2 + 1, "string starts here");
    tk->status = LX_STATUS_TOKENIZATION_ERROR;
    return NULL;
}
static token* lx_load(lexer* tk)
{
    char curr = lx_peek_char(tk);
    token* tok = tk->loaded_tokens_head;
    while (true) {
        lx_void_char_peek(tk);
        switch (curr) {
            case '\0': {
                if (tk->status == LX_STATUS_IO_ERROR) return NULL;
                tok->kind = TK_EOF;
                return lx_return_head(tk, 0);
            }
            case '$': tok->kind = TK_DOLLAR; return lx_return_head(tk, 1);
            case '(': tok->kind = TK_PAREN_OPEN; return lx_return_head(tk, 1);
            case ')': tok->kind = TK_PAREN_CLOSE; return lx_return_head(tk, 1);
            case '{': tok->kind = TK_BRACE_OPEN; return lx_return_head(tk, 1);
            case '}': tok->kind = TK_BRACE_CLOSE; return lx_return_head(tk, 1);
            case '[': tok->kind = TK_BRACKET_OPEN; return lx_return_head(tk, 1);
            case '#': tok->kind = TK_HASH; return lx_return_head(tk, 1);
            case '@': tok->kind = TK_AT; return lx_return_head(tk, 1);
            case ']':
                tok->kind = TK_BRACKET_CLOSE;
                return lx_return_head(tk, 1);
            case ',': tok->kind = TK_COMMA; return lx_return_head(tk, 1);
            case ';': tok->kind = TK_SEMICOLON; return lx_return_head(tk, 1);
            case '.': tok->kind = TK_DOT; return lx_return_head(tk, 1);
            case '\t': {
                curr = lx_peek_char(tk);
                tok->start++; // TODO: make an option to increase by 2/4/8/n
                continue;
            }
            case ' ': {
                curr = lx_peek_char(tk);
                tok->start++;
                continue;
            }
            case '\n': {
                curr = lx_peek_char(tk);
                tok->start++;
                src_map_add_line(&tk->file->src_map, tok->start);
                continue;
            }
            case ':': {
                char peek = lx_peek_char(tk);
                if (peek == ':') {
                    lx_void_char_peek(tk);
                    tok->kind = TK_DOUBLE_COLON;
                    return lx_return_head(tk, 2);
                }
                tok->kind = TK_COLON;
                return lx_return_head(tk, 1);
            }
            case '*': {
                char peek = lx_peek_char(tk);
                if (peek == '=') {
                    lx_void_char_peek(tk);
                    tok->kind = TK_STAR_EQUALS;
                    return lx_return_head(tk, 2);
                }
                tok->kind = TK_STAR;
                return lx_return_head(tk, 1);
            }
            case '+': {
                char peek = lx_peek_char(tk);
                if (peek == '+') {
                    lx_void_char_peek(tk);
                    tok->kind = TK_DOUBLE_PLUS;
                    return lx_return_head(tk, 2);
                }
                if (peek == '=') {
                    lx_void_char_peek(tk);
                    tok->kind = TK_PLUS_EQUALS;
                    return lx_return_head(tk, 2);
                }
                tok->kind = TK_PLUS;
                return lx_return_head(tk, 1);
            }
            case '-': {
                char peek = lx_peek_char(tk);
                if (peek == '-') {
                    lx_void_char_peek(tk);
                    tok->kind = TK_DOUBLE_MINUS;
                    return lx_return_head(tk, 2);
                }
                if (peek == '=') {
                    lx_void_char_peek(tk);
                    tok->kind = TK_MINUS_EQUALS;
                    return lx_return_head(tk, 2);
                }
                if (peek == '>') {
                    lx_void_char_peek(tk);
                    tok->kind = TK_ARROW;
                    return lx_return_head(tk, 2);
                }
                tok->kind = TK_MINUS;
                return lx_return_head(tk, 1);
            }
            case '!': {
                char peek = lx_peek_char(tk);
                if (peek == '=') {
                    lx_void_char_peek(tk);
                    tok->kind = TK_EXCLAMATION_MARK_EQUALS;
                    return lx_return_head(tk, 2);
                }
                tok->kind = TK_EXCLAMATION_MARK;
                return lx_return_head(tk, 1);
            }
            case '|': {
                char peek = lx_peek_char(tk);
                if (peek == '|') {
                    lx_void_char_peek(tk);
                    tok->kind = TK_DOUBLE_PIPE;
                    return lx_return_head(tk, 2);
                }
                if (peek == '=') {
                    lx_void_char_peek(tk);
                    tok->kind = TK_PIPE_EQUALS;
                    return lx_return_head(tk, 2);
                }
                tok->kind = TK_PIPE;
                return lx_return_head(tk, 1);
            }
            case '&': {
                char peek = lx_peek_char(tk);
                if (peek == '&') {
                    lx_void_char_peek(tk);
                    tok->kind = TK_DOUBLE_AND;
                    return lx_return_head(tk, 2);
                }
                if (peek == '=') {
                    lx_void_char_peek(tk);
                    tok->kind = TK_AND_EQUALS;
                    return lx_return_head(tk, 2);
                }
                tok->kind = TK_AND;
                return lx_return_head(tk, 1);
            }
            case '^': {
                char peek = lx_peek_char(tk);
                if (peek == '^') {
                    lx_void_char_peek(tk);
                    tok->kind = TK_DOUBLE_CARET;
                    return lx_return_head(tk, 2);
                }
                if (peek == '=') {
                    lx_void_char_peek(tk);
                    tok->kind = TK_CARET_EQUALS;
                    return lx_return_head(tk, 2);
                }
                tok->kind = TK_CARET;
                return lx_return_head(tk, 1);
            }
            case '~': {
                char peek = lx_peek_char(tk);
                if (peek == '=') {
                    lx_void_char_peek(tk);
                    tok->kind = TK_TILDE_EQUALS;
                    return lx_return_head(tk, 2);
                }
                tok->kind = TK_TILDE;
                return lx_return_head(tk, 1);
            }
            case '=': {
                char peek = lx_peek_char(tk);
                if (peek == '=') {
                    lx_void_char_peek(tk);
                    tok->kind = TK_DOUBLE_EQUALS;
                    return lx_return_head(tk, 2);
                }
                if (peek == '>') {
                    lx_void_char_peek(tk);
                    tok->kind = TK_FAT_ARROW;
                    return lx_return_head(tk, 2);
                }
                tok->kind = TK_EQUALS;
                return lx_return_head(tk, 1);
            }
            case '/': {
                char peek = lx_peek_char(tk);
                if (peek == '/') {
                    tok->start += 2;
                    lx_void_char_peek(tk);
                    do {
                        curr = lx_peek_char(tk);
                        lx_void_char_peek(tk);
                        tok->start++;
                    } while (curr != '\n' && curr != '\0');
                    if (curr == '\n') {
                        src_map_add_line(&tk->file->src_map, tok->start);
                        curr = lx_peek_char(tk);
                        continue;
                    }
                    else {
                        if (tk->status == LX_STATUS_EOF) {
                            tok->start--;
                            tok->kind = TK_EOF;
                            return lx_return_head(tk, 1);
                        }
                        else {
                            // TODO: print error
                            lx_dec_iter(tk, &tk->loaded_tokens_start);
                            return NULL;
                        }
                    }
                }
                if (peek == '*') {
                    ureg comment_start = tok->start;
                    lx_void_char_peek(tk);
                    tok->start += 2;
                    ureg nest_count = 1;
                    do {
                        curr = lx_consume_char(tk);
                        tok->start++;
                        switch (curr) {
                            case '\n': {
                                src_map_add_line(
                                    &tk->file->src_map, tok->start);
                            } break;
                            case '\t': break;
                            case '*': {
                                curr = lx_peek_char(tk);
                                tok->start++;
                                if (curr == '/') {
                                    lx_void_char_peek(tk);
                                    nest_count--;
                                }
                            } break;
                            case '/': {
                                curr = lx_peek_char(tk);
                                tok->start++;
                                if (curr == '*') {
                                    lx_void_char_peek(tk);
                                    nest_count++;
                                }
                            } break;
                            case '\0': {
                                tok->start--;
                                error_log_report_annotated_twice(
                                    tk->tc->err_log, ES_TOKENIZER, false,
                                    "unterminated block comment", tk->file,
                                    tok->start, tok->start + 1,
                                    "reached eof before the comment was closed",
                                    tk->file, comment_start, comment_start + 2,
                                    "comment starts here");
                                tk->status = LX_STATUS_TOKENIZATION_ERROR;
                                return NULL;
                            }
                        }
                    } while (nest_count > 0);
                    curr = lx_peek_char(tk);
                    continue;
                }
                if (peek == '=') {
                    lx_void_char_peek(tk);
                    tok->kind = TK_SLASH_EQUALS;
                    return lx_return_head(tk, 2);
                }
                else {
                    tok->kind = TK_SLASH;
                    return lx_return_head(tk, 1);
                }
            }
            case '%': {
                char peek = lx_peek_char(tk);
                if (peek == '=') {
                    lx_void_char_peek(tk);
                    tok->kind = TK_PERCENT_EQUALS;
                    return lx_return_head(tk, 2);
                }
                tok->kind = TK_PERCENT;
                return lx_return_head(tk, 1);
            }
            case '<': {
                char peek = lx_peek_char(tk);
                if (peek == '<') {
                    lx_void_char_peek(tk);
                    peek = lx_peek_char(tk);
                    if (peek == '=') {
                        lx_void_char_peek(tk);
                        tok->kind = TK_DOUBLE_LESS_THAN_EQUALS;
                        return lx_return_head(tk, 3);
                    }
                    tok->kind = TK_DOUBLE_LESS_THAN;
                    return lx_return_head(tk, 2);
                }
                if (peek == '=') {
                    lx_void_char_peek(tk);
                    tok->kind = TK_LESS_THAN_EQUALS;
                    return lx_return_head(tk, 2);
                }
                tok->kind = TK_LESS_THAN;
                return lx_return_head(tk, 1);
            }
            case '>': {
                char peek = lx_peek_char(tk);
                if (peek == '>') {
                    lx_void_char_peek(tk);
                    peek = lx_peek_char(tk);
                    if (peek == '=') {
                        lx_void_char_peek(tk);
                        tok->kind = TK_DOUBLE_GREATER_THAN_EQUALS;
                        return lx_return_head(tk, 3);
                    }
                    tok->kind = TK_DOUBLE_GREATER_THAN;
                    return lx_return_head(tk, 2);
                }
                if (peek == '=') {
                    tok->kind = TK_GREATER_THAN_EQUALS;
                    return lx_return_head(tk, 2);
                }
                tok->kind = TK_GREATER_THAN;
                return lx_return_head(tk, 1);
            }
            case '\'': {
                char* str_start = tk->file_buffer_pos - 1;
                do {
                    curr = lx_peek_char_holding(tk, &str_start);
                    lx_void_char_peek(tk);
                    if (curr == '\0') {
                        return lx_unterminated_string_error(
                            tk, str_start, tok->start);
                    }
                    if (curr == '\\') {
                        // TODO: think about converting escaped chars
                        curr = lx_peek_char_holding(tk, &str_start);
                        lx_void_char_peek(tk);
                        if (curr == '\0') {
                            return lx_unterminated_string_error(
                                tk, str_start, tok->start);
                        }
                    }
                    if (curr == '\n') {
                        src_map_add_line(
                            &tk->file->src_map,
                            tok->start +
                                ptrdiff(tk->file_buffer_pos, str_start));
                    }
                } while (curr != '\'');
                tok->kind = TK_BINARY_STRING;
                tok->str.start = str_start + 1;
                tok->str.end = tk->file_buffer_pos - 1;
                return lx_return_head(
                    tk, ptrdiff(tk->file_buffer_pos, str_start));
            }
            case '"': {
                char* str_start = tk->file_buffer_pos - 1;
                do {
                    curr = lx_peek_char_holding(tk, &str_start);
                    lx_void_char_peek(tk);
                    if (curr == '\0') {
                        return lx_unterminated_string_error(
                            tk, str_start, tok->start);
                    }
                    if (curr == '\\') {
                        // TODO: think about converting escaped chars
                        curr = lx_peek_char_holding(tk, &str_start);
                        lx_void_char_peek(tk);
                        if (curr == '\0') {
                            return lx_unterminated_string_error(
                                tk, str_start, tok->start);
                        }
                    }
                    if (curr == '\n') {
                        src_map_add_line(
                            &tk->file->src_map,
                            tok->start +
                                ptrdiff(tk->file_buffer_pos, str_start));
                    }
                } while (curr != '"');
                tok->kind = TK_STRING;
                tok->str.start = str_start + 1;
                tok->str.end = tk->file_buffer_pos - 1;
                return lx_return_head(
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
                curr = lx_peek_char_holding(tk, &str_start);
                while ((curr >= 'a' && curr <= 'z') ||
                       (curr >= 'A' && curr <= 'Z') ||
                       (curr >= '0' && curr <= '9') || (curr == '_')) {
                    lx_void_char_peek(tk);
                    curr = lx_peek_char_holding(tk, &str_start);
                }
                tok->str.start = str_start;
                tok->str.end = tk->file_buffer_pos;
                tok->kind = match_kw(tok->str);
                if (tok->kind == TK_NONE) tok->kind = TK_IDENTIFIER;
                return lx_return_head(
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
                curr = lx_peek_char_holding(tk, &str_start);
                while (curr >= '0' && curr <= '9') {
                    lx_void_char_peek(tk);
                    curr = lx_peek_char_holding(tk, &str_start);
                }
                tok->kind = TK_NUMBER;
                tok->str.start = str_start;
                tok->str.end = tk->file_buffer_pos;
                return lx_return_head(
                    tk, ptrdiff(tk->file_buffer_pos, str_start));
            }
            default: {
                /*
                //TODO: proper unicode handling
                ureg len = get_utf8_seq_len_from_head(curr);
                for(ureg i = 1; i<len;i++){
                    char c = lx_peek_char(tk);
                    if(!is_utf8_continuation(c)){
                        len = i;
                        break;
                    }
                    lx_void_char_peek(tk);
                }
                */
                error_log_report_annotated(
                    tk->tc->err_log, ES_TOKENIZER, false, "unknown token",
                    tk->file, tok->start, tok->start + 1,
                    "not the start for any valid token");
                return NULL;
            }
        }
    }
    panic("unexpected error in lexer");
    return NULL;
}
