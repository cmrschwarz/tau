#include "lexer.h"
#include "error_log.h"
#include "file_map.h"
#include "tauc.h"
#include "utils/math_utils.h"
#include "utils/panic.h"
#include "thread_context.h"
#include <assert.h>

static token* lx_load(lexer* lx);

static inline void lx_inc_iter(lexer* lx, token** t)
{
    (*t)++;
    if (*t == lx->token_buffer_end) {
        *t = lx->token_buffer;
    }
}
static inline void lx_inc_iter_n(lexer* lx, token** t, ureg n)
{
    ureg rem = lx->token_buffer_end - *t;
    if (rem > n) {
        *t += n;
    }
    else {
        *t = lx->token_buffer + (n - rem); /*% LX_TOKEN_BUFFER_SIZE*/
    }
}
static inline void lx_dec_iter(lexer* lx, token** t)
{
    (*t)--;
    if (*t == lx->token_buffer) {
        *t = lx->token_buffer_end - 1;
    }
}
static inline void lx_dec_iter_n(lexer* lx, token** t, ureg n)
{
    ureg rem = *t - lx->token_buffer;
    if (rem >= n) {
        *t -= n;
    }
    else {
        *t = lx->token_buffer_end - (n - rem); /*% LX_TOKEN_BUFFER_SIZE*/
    }
}
token* lx_peek_nth(lexer* lx, ureg n)
{
    token* t = lx->loaded_tokens_start;
    if (lx->loaded_tokens_head < lx->loaded_tokens_start) {
        ureg rem = lx->token_buffer_end - t;
        if (rem >= n) return t + n - 1;
        n -= rem;
        t = lx->token_buffer;
    }
    ureg rem = lx->loaded_tokens_head - t;
    if (rem >= n) {
        return t + n - 1;
    }
    n -= rem;
    while (n > 1) {
        if (!lx_load(lx)) return NULL;
        n--;
    }
    return lx_load(lx);
}
token* lx_peek(lexer* lx)
{
    if (lx->loaded_tokens_start != lx->loaded_tokens_head) {
        return lx->loaded_tokens_start;
    }
    return lx_load(lx);
}
token* lx_peek_2nd(lexer* lx)
{
    return lx_peek_nth(lx, 2);
}
token* lx_peek_3rd(lexer* lx)
{
    return lx_peek_nth(lx, 3);
}
void lx_void(lexer* lx)
{
    lx_inc_iter(lx, &lx->loaded_tokens_start);
}
void lx_void_n(lexer* lx, ureg n)
{
    lx_inc_iter_n(lx, &lx->loaded_tokens_start, n);
}
token* lx_consume(lexer* lx)
{
    token* t = lx_peek(lx);
    if (t) lx_void(lx); // PERF: is the if required?
    return t;
}
static inline size_t
lx_stream_read(lexer* lx, char* tgt, size_t size, int* error)
{
    ureg read_size;
    *error = src_map_read(lx->smap, size, &read_size, tgt);
    return read_size;
}
static inline int lx_load_file_buffer(lexer* lx, char** holding)
{
    token* t = lx->loaded_tokens_start;
    ureg size_to_keep = 0;
    while (t != lx->loaded_tokens_head) {
        if (token_has_string(t)) size_to_keep += string_len(t->str);
        lx_inc_iter(lx, &t);
    }
    if (holding) {
        size_to_keep += ptrdiff(lx->file_buffer_pos, *holding);
    }
    ureg buff_size = ptrdiff(lx->file_buffer_end, lx->file_buffer_start);
    void* old_buff = NULL;
    if (buff_size - size_to_keep < LX_MIN_FILE_READ_SIZE) {
        buff_size *= 2;
        old_buff = lx->file_buffer_start;
        lx->file_buffer_start = tmalloc(buff_size);
        if (!lx->file_buffer_start) {
            error_log_report_allocation_failiure(lx->tc->err_log);
            return -1;
        }
        lx->file_buffer_end = ptradd(lx->file_buffer_start, buff_size);
    }
    lx->file_buffer_head = lx->file_buffer_start;
    t = lx->loaded_tokens_start;
    while (t != lx->loaded_tokens_head) {
        if (token_has_string(t)) {
            ureg slen = string_len(t->str);
            memcpy(lx->file_buffer_head, t->str.start, slen);
            lx->file_buffer_head = (char*)ptradd(lx->file_buffer_head, slen);
        }
        lx_inc_iter(lx, &t);
    }
    if (holding) {
        ureg slen = ptrdiff(lx->file_buffer_pos, *holding);
        memcpy(lx->file_buffer_head, *holding, slen);
        *holding = lx->file_buffer_head;
        lx->file_buffer_head = (char*)ptradd(lx->file_buffer_head, slen);
    }
    if (old_buff) {
        tfree(old_buff);
    }
    lx->file_buffer_pos = lx->file_buffer_head;
    int error = 0;
    ureg siz = lx_stream_read(
        lx, lx->file_buffer_head, buff_size - size_to_keep, &error);
    if (siz == 0) {
        if (error) {
            lx->status = LX_STATUS_IO_ERROR;
            error_log_report_simple(
                lx->tc->err_log, ES_TOKENIZER, false, "file io error", lx->smap,
                lx->loaded_tokens_head->start);
            return ERR;
        }
        if (lx->status == LX_STATUS_EOF) return 0;
        *lx->file_buffer_pos = '\0';
        lx->file_buffer_head++;
        lx->status = LX_STATUS_EOF;
    }
    else {
        if (lx->status == LX_STATUS_EOF) lx->status = LX_STATUS_OK;
        lx->file_buffer_head += siz;
    }
    return 0;
}
static inline char lx_peek_char_holding(lexer* lx, char** hold)
{
    if (lx->file_buffer_pos == lx->file_buffer_head) {
        if (lx_load_file_buffer(lx, hold)) {
            lx->status = LX_STATUS_IO_ERROR;
            return '\0';
        }
    }
    char r = *lx->file_buffer_pos;
    return r;
}
static inline char lx_peek_char(lexer* lx)
{
    return lx_peek_char_holding(lx, NULL);
}
static inline void lx_void_char_peek(lexer* lx)
{
    lx->file_buffer_pos++;
}
static inline char lx_consume_char(lexer* lx)
{
    char c = lx_peek_char_holding(lx, NULL);
    lx_void_char_peek(lx);
    return c;
}
token* lx_consume_macro_string(lexer* lx)
{
    assert(lx->loaded_tokens_start == lx->loaded_tokens_head);
    token* tok = lx->loaded_tokens_head;
    char start = lx_consume_char(lx);
    char end;
    if (start == '(') {
        end = ')';
    }
    else if (start == '{') {
        end = '}';
    }
    else {
        error_log_report_simple(
            lx->tc->err_log, ES_TOKENIZER, false,
            "expected '(' or '{' to begin macro string", lx->smap,
            lx->loaded_tokens_head->start);
        return NULL;
    }
    char curr = lx_consume_char(lx);
    char* str_start = lx->file_buffer_pos - 1;
    ureg paren_level = 1;
    while (true) {
        lx_void_char_peek(lx);
        if (curr == end) {
            paren_level--;
            if (paren_level == 0) break;
        }
        else if (curr == start) {
            paren_level++;
        }
        curr = lx_peek_char_holding(lx, &str_start);
    }
    tok->kind = TK_STRING;
    tok->str.start = str_start;
    tok->str.end = lx->file_buffer_pos - 1; //-1 for the trailing ')'
    // plus one for the initial '('
    tok->end = tok->start + ptrdiff(lx->file_buffer_head, str_start) + 1;
    lx_inc_iter(lx, &lx->loaded_tokens_head);
    lx->loaded_tokens_start = lx->loaded_tokens_head;
    lx->loaded_tokens_head->start = tok->end;
    return tok;
}
int lx_init(lexer* lx, thread_context* tc)
{
    lx->tc = tc;
    ureg size = plattform_get_page_size() * 8;
    lx->smap = NULL;
    lx->file_buffer_start = tmalloc(size);
    if (!lx->file_buffer_start) return -1;
    lx->file_buffer_end = ptradd(lx->file_buffer_start, size);
    lx->token_buffer_end = lx->token_buffer + LX_TOKEN_BUFFER_SIZE;
    lx->loaded_tokens_start = lx->token_buffer; // head is set on open_file
    lx->status = LX_STATUS_OK;
    return 0;
}
void lx_fin(lexer* lx)
{
    tfree(lx->file_buffer_start);
}
void lx_reset_buffer(lexer* lx)
{
    lx->file_buffer_pos = lx->file_buffer_start;
    lx->file_buffer_head = lx->file_buffer_start;
    lx->loaded_tokens_start->start = 0;
    lx->loaded_tokens_head = lx->loaded_tokens_start;
}

int lx_open_smap(lexer* lx, src_map* smap)
{
    assert(lx->smap == NULL);
    lx->smap = smap;
    int r = src_map_open(lx->smap);
    if (r) return ERR;
    lx_reset_buffer(lx);
    if (lx_load_file_buffer(lx, NULL)) {
        src_map_close(lx->smap);
        return ERR;
    }
    lx->status = LX_STATUS_OK;
    return OK;
}
void lx_close_smap(lexer* lx)
{
    assert(lx->smap);
    src_map_close(lx->smap);
    lx->smap = NULL;
}
int lx_open_paste(lexer* lx, paste_evaluation* pe, src_map* parent_smap)
{
    src_map* smap = src_map_create_child(parent_smap, (ast_elem*)pe, lx->tc);
    if (!smap) return ERR;
    return lx_open_smap(lx, smap);
}
void lx_close_paste(lexer* lx)
{
    lx_close_smap(lx);
}

int lx_open_file(lexer* lx, src_file* f)
{
    if (src_file_start_parse(f, lx->tc)) {
        return ERR;
    }
    return lx_open_smap(lx, &f->smap);
}
void lx_close_file(lexer* lx)
{
    lx_close_smap(lx);
}

static inline token* lx_return_head(lexer* lx, ureg tok_length)
{
    token* tok = lx->loaded_tokens_head;
    lx_inc_iter(lx, &lx->loaded_tokens_head);
    token* next = lx->loaded_tokens_head;
    tok->end = tok->start + tok_length;
    next->start = tok->end;
    return tok;
}
static inline token*
lx_unterminated_string_error(lexer* lx, char* string_start, ureg tok_pos)
{
    ureg start1 = tok_pos + ptrdiff(lx->file_buffer_pos, string_start) - 1;
    ureg start2 = tok_pos;
    error_log_report_annotated_twice(
        lx->tc->err_log, ES_TOKENIZER, false, "unterminated string", lx->smap,
        start1, start1 + 1, "reached eof before the string was closed",
        lx->smap, start2, start2 + 1, "string starts here");
    lx->status = LX_STATUS_TOKENIZATION_ERROR;
    return NULL;
}
static token* lx_load(lexer* lx)
{
    char curr = lx_peek_char(lx);
    token* tok = lx->loaded_tokens_head;
    while (true) {
        lx_void_char_peek(lx);
        switch (curr) {
            case '\0': {
                if (lx->status == LX_STATUS_IO_ERROR) return NULL;
                tok->kind = TK_EOF;
                return lx_return_head(lx, 0);
            }
            case '$': tok->kind = TK_DOLLAR; return lx_return_head(lx, 1);
            case '(': tok->kind = TK_PAREN_OPEN; return lx_return_head(lx, 1);
            case ')': tok->kind = TK_PAREN_CLOSE; return lx_return_head(lx, 1);
            case '{': tok->kind = TK_BRACE_OPEN; return lx_return_head(lx, 1);
            case '}': tok->kind = TK_BRACE_CLOSE; return lx_return_head(lx, 1);
            case '[': tok->kind = TK_BRACKET_OPEN; return lx_return_head(lx, 1);
            case '#': tok->kind = TK_HASH; return lx_return_head(lx, 1);
            case '@': tok->kind = TK_AT; return lx_return_head(lx, 1);
            case ']':
                tok->kind = TK_BRACKET_CLOSE;
                return lx_return_head(lx, 1);
            case ',': tok->kind = TK_COMMA; return lx_return_head(lx, 1);
            case ';': tok->kind = TK_SEMICOLON; return lx_return_head(lx, 1);
            case '.': tok->kind = TK_DOT; return lx_return_head(lx, 1);
            case '\t': {
                curr = lx_peek_char(lx);
                tok->start++; // TODO: make an option to increase by 2/4/8/n
                continue;
            }
            case ' ': {
                curr = lx_peek_char(lx);
                tok->start++;
                continue;
            }
            case '\n': {
                curr = lx_peek_char(lx);
                tok->start++;
                src_map_add_line(lx->smap, tok->start);
                continue;
            }
            case ':': {
                char peek = lx_peek_char(lx);
                if (peek == ':') {
                    lx_void_char_peek(lx);
                    tok->kind = TK_DOUBLE_COLON;
                    return lx_return_head(lx, 2);
                }
                tok->kind = TK_COLON;
                return lx_return_head(lx, 1);
            }
            case '*': {
                char peek = lx_peek_char(lx);
                if (peek == '=') {
                    lx_void_char_peek(lx);
                    tok->kind = TK_STAR_EQUALS;
                    return lx_return_head(lx, 2);
                }
                tok->kind = TK_STAR;
                return lx_return_head(lx, 1);
            }
            case '+': {
                char peek = lx_peek_char(lx);
                if (peek == '+') {
                    lx_void_char_peek(lx);
                    tok->kind = TK_DOUBLE_PLUS;
                    return lx_return_head(lx, 2);
                }
                if (peek == '=') {
                    lx_void_char_peek(lx);
                    tok->kind = TK_PLUS_EQUALS;
                    return lx_return_head(lx, 2);
                }
                tok->kind = TK_PLUS;
                return lx_return_head(lx, 1);
            }
            case '-': {
                char peek = lx_peek_char(lx);
                if (peek == '-') {
                    lx_void_char_peek(lx);
                    tok->kind = TK_DOUBLE_MINUS;
                    return lx_return_head(lx, 2);
                }
                if (peek == '=') {
                    lx_void_char_peek(lx);
                    tok->kind = TK_MINUS_EQUALS;
                    return lx_return_head(lx, 2);
                }
                if (peek == '>') {
                    lx_void_char_peek(lx);
                    tok->kind = TK_ARROW;
                    return lx_return_head(lx, 2);
                }
                tok->kind = TK_MINUS;
                return lx_return_head(lx, 1);
            }
            case '!': {
                char peek = lx_peek_char(lx);
                if (peek == '=') {
                    lx_void_char_peek(lx);
                    tok->kind = TK_EXCLAMATION_MARK_EQUALS;
                    return lx_return_head(lx, 2);
                }
                tok->kind = TK_EXCLAMATION_MARK;
                return lx_return_head(lx, 1);
            }
            case '|': {
                char peek = lx_peek_char(lx);
                if (peek == '|') {
                    lx_void_char_peek(lx);
                    tok->kind = TK_DOUBLE_PIPE;
                    return lx_return_head(lx, 2);
                }
                if (peek == '=') {
                    lx_void_char_peek(lx);
                    tok->kind = TK_PIPE_EQUALS;
                    return lx_return_head(lx, 2);
                }
                tok->kind = TK_PIPE;
                return lx_return_head(lx, 1);
            }
            case '&': {
                char peek = lx_peek_char(lx);
                if (peek == '&') {
                    lx_void_char_peek(lx);
                    tok->kind = TK_DOUBLE_AND;
                    return lx_return_head(lx, 2);
                }
                if (peek == '=') {
                    lx_void_char_peek(lx);
                    tok->kind = TK_AND_EQUALS;
                    return lx_return_head(lx, 2);
                }
                tok->kind = TK_AND;
                return lx_return_head(lx, 1);
            }
            case '^': {
                char peek = lx_peek_char(lx);
                if (peek == '^') {
                    lx_void_char_peek(lx);
                    tok->kind = TK_DOUBLE_CARET;
                    return lx_return_head(lx, 2);
                }
                if (peek == '=') {
                    lx_void_char_peek(lx);
                    tok->kind = TK_CARET_EQUALS;
                    return lx_return_head(lx, 2);
                }
                tok->kind = TK_CARET;
                return lx_return_head(lx, 1);
            }
            case '~': {
                char peek = lx_peek_char(lx);
                if (peek == '=') {
                    lx_void_char_peek(lx);
                    tok->kind = TK_TILDE_EQUALS;
                    return lx_return_head(lx, 2);
                }
                tok->kind = TK_TILDE;
                return lx_return_head(lx, 1);
            }
            case '=': {
                char peek = lx_peek_char(lx);
                if (peek == '=') {
                    lx_void_char_peek(lx);
                    tok->kind = TK_DOUBLE_EQUALS;
                    return lx_return_head(lx, 2);
                }
                if (peek == '>') {
                    lx_void_char_peek(lx);
                    tok->kind = TK_FAT_ARROW;
                    return lx_return_head(lx, 2);
                }
                tok->kind = TK_EQUALS;
                return lx_return_head(lx, 1);
            }
            case '/': {
                char peek = lx_peek_char(lx);
                if (peek == '/') {
                    tok->start += 2;
                    lx_void_char_peek(lx);
                    do {
                        curr = lx_peek_char(lx);
                        lx_void_char_peek(lx);
                        tok->start++;
                    } while (curr != '\n' && curr != '\0');
                    if (curr == '\n') {
                        src_map_add_line(lx->smap, tok->start);
                        curr = lx_peek_char(lx);
                        continue;
                    }
                    else {
                        if (lx->status == LX_STATUS_EOF) {
                            tok->start--;
                            tok->kind = TK_EOF;
                            return lx_return_head(lx, 1);
                        }
                        else {
                            // TODO: print error
                            lx_dec_iter(lx, &lx->loaded_tokens_start);
                            return NULL;
                        }
                    }
                }
                if (peek == '*') {
                    ureg comment_start = tok->start;
                    lx_void_char_peek(lx);
                    tok->start += 2;
                    ureg nest_count = 1;
                    do {
                        curr = lx_consume_char(lx);
                        tok->start++;
                        switch (curr) {
                            case '\n': {
                                src_map_add_line(lx->smap, tok->start);
                            } break;
                            case '\t': break;
                            case '*': {
                                curr = lx_peek_char(lx);
                                if (curr == '/') {
                                    lx_void_char_peek(lx);
                                    tok->start++;
                                    nest_count--;
                                }
                            } break;
                            case '/': {
                                curr = lx_peek_char(lx);
                                if (curr == '*') {
                                    lx_void_char_peek(lx);
                                    tok->start++;
                                    nest_count++;
                                }
                            } break;
                            case '\0': {
                                tok->start--;
                                error_log_report_annotated_twice(
                                    lx->tc->err_log, ES_TOKENIZER, false,
                                    "unterminated block comment", lx->smap,
                                    tok->start, tok->start + 1,
                                    "reached eof before the comment was closed",
                                    lx->smap, comment_start, comment_start + 2,
                                    "comment starts here");
                                lx->status = LX_STATUS_TOKENIZATION_ERROR;
                                return NULL;
                            }
                        }
                    } while (nest_count > 0);
                    curr = lx_peek_char(lx);
                    continue;
                }
                if (peek == '=') {
                    lx_void_char_peek(lx);
                    tok->kind = TK_SLASH_EQUALS;
                    return lx_return_head(lx, 2);
                }
                else {
                    tok->kind = TK_SLASH;
                    return lx_return_head(lx, 1);
                }
            }
            case '%': {
                char peek = lx_peek_char(lx);
                if (peek == '=') {
                    lx_void_char_peek(lx);
                    tok->kind = TK_PERCENT_EQUALS;
                    return lx_return_head(lx, 2);
                }
                tok->kind = TK_PERCENT;
                return lx_return_head(lx, 1);
            }
            case '<': {
                char peek = lx_peek_char(lx);
                if (peek == '<') {
                    lx_void_char_peek(lx);
                    peek = lx_peek_char(lx);
                    if (peek == '=') {
                        lx_void_char_peek(lx);
                        tok->kind = TK_DOUBLE_LESS_THAN_EQUALS;
                        return lx_return_head(lx, 3);
                    }
                    tok->kind = TK_DOUBLE_LESS_THAN;
                    return lx_return_head(lx, 2);
                }
                if (peek == '=') {
                    lx_void_char_peek(lx);
                    tok->kind = TK_LESS_THAN_EQUALS;
                    return lx_return_head(lx, 2);
                }
                tok->kind = TK_LESS_THAN;
                return lx_return_head(lx, 1);
            }
            case '>': {
                char peek = lx_peek_char(lx);
                if (peek == '>') {
                    lx_void_char_peek(lx);
                    peek = lx_peek_char(lx);
                    if (peek == '=') {
                        lx_void_char_peek(lx);
                        tok->kind = TK_DOUBLE_GREATER_THAN_EQUALS;
                        return lx_return_head(lx, 3);
                    }
                    tok->kind = TK_DOUBLE_GREATER_THAN;
                    return lx_return_head(lx, 2);
                }
                if (peek == '=') {
                    tok->kind = TK_GREATER_THAN_EQUALS;
                    return lx_return_head(lx, 2);
                }
                tok->kind = TK_GREATER_THAN;
                return lx_return_head(lx, 1);
            }
            case '\'': {
                char* str_start = lx->file_buffer_pos - 1;
                do {
                    curr = lx_peek_char_holding(lx, &str_start);
                    lx_void_char_peek(lx);
                    if (curr == '\0') {
                        return lx_unterminated_string_error(
                            lx, str_start, tok->start);
                    }
                    if (curr == '\\') {
                        // TODO: think about converting escaped chars
                        curr = lx_peek_char_holding(lx, &str_start);
                        lx_void_char_peek(lx);
                        if (curr == '\0') {
                            return lx_unterminated_string_error(
                                lx, str_start, tok->start);
                        }
                    }
                    if (curr == '\n') {
                        src_map_add_line(
                            lx->smap,
                            tok->start +
                                ptrdiff(lx->file_buffer_pos, str_start));
                    }
                } while (curr != '\'');
                tok->kind = TK_BINARY_STRING;
                tok->str.start = str_start + 1;
                tok->str.end = lx->file_buffer_pos - 1;
                return lx_return_head(
                    lx, ptrdiff(lx->file_buffer_pos, str_start));
            }
            case '"': {
                char* str_start = lx->file_buffer_pos - 1;
                do {
                    curr = lx_peek_char_holding(lx, &str_start);
                    lx_void_char_peek(lx);
                    if (curr == '\0') {
                        return lx_unterminated_string_error(
                            lx, str_start, tok->start);
                    }
                    if (curr == '\\') {
                        // TODO: think about converting escaped chars
                        curr = lx_peek_char_holding(lx, &str_start);
                        lx_void_char_peek(lx);
                        if (curr == '\0') {
                            return lx_unterminated_string_error(
                                lx, str_start, tok->start);
                        }
                    }
                    if (curr == '\n') {
                        src_map_add_line(
                            lx->smap,
                            tok->start +
                                ptrdiff(lx->file_buffer_pos, str_start));
                    }
                } while (curr != '"');
                tok->kind = TK_STRING;
                tok->str.start = str_start + 1;
                tok->str.end = lx->file_buffer_pos - 1;
                return lx_return_head(
                    lx, ptrdiff(lx->file_buffer_pos, str_start));
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
                char* str_start = lx->file_buffer_pos - 1;
                curr = lx_peek_char_holding(lx, &str_start);
                while ((curr >= 'a' && curr <= 'z') ||
                       (curr >= 'A' && curr <= 'Z') ||
                       (curr >= '0' && curr <= '9') || (curr == '_')) {
                    lx_void_char_peek(lx);
                    curr = lx_peek_char_holding(lx, &str_start);
                }
                tok->str.start = str_start;
                tok->str.end = lx->file_buffer_pos;
                tok->kind = match_kw(tok->str);
                if (tok->kind == TK_NONE) tok->kind = TK_IDENTIFIER;
                return lx_return_head(
                    lx, ptrdiff(lx->file_buffer_pos, str_start));
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
                char* str_start = lx->file_buffer_pos - 1;
                curr = lx_peek_char_holding(lx, &str_start);
                while (curr >= '0' && curr <= '9') {
                    lx_void_char_peek(lx);
                    curr = lx_peek_char_holding(lx, &str_start);
                }
                tok->kind = TK_NUMBER;
                tok->str.start = str_start;
                tok->str.end = lx->file_buffer_pos;
                return lx_return_head(
                    lx, ptrdiff(lx->file_buffer_pos, str_start));
            }
            default: {
                /*
                //TODO: proper unicode handling
                ureg len = get_utf8_seq_len_from_head(curr);
                for(ureg i = 1; i<len;i++){
                    char c = lx_peek_char(lx);
                    if(!is_utf8_continuation(c)){
                        len = i;
                        break;
                    }
                    lx_void_char_peek(lx);
                }
                */
                error_log_report_annotated(
                    lx->tc->err_log, ES_TOKENIZER, false, "unknown token",
                    lx->smap, tok->start, tok->start + 1,
                    "not the start for any valid token");
                return NULL;
            }
        }
    }
    panic("unexpected error in lexer");
    return NULL;
}
