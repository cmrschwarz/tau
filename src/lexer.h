#ifndef TAUC_LEXER_H
#define TAUC_LEXER_H

#include "src_map.h"
#include "token.h"
#include "utils/c_extensions.h"
#include "utils/dbuffer.h"
#include "utils/sbuffer.h"
#include "utils/string.h"
#include <stdio.h>

typedef struct thread_context_s thread_context;
typedef struct src_file_s src_file;

typedef enum PACK_ENUM lx_status {
    LX_STATUS_OK,
    LX_STATUS_EOF,
    LX_STATUS_TOKENIZATION_ERROR,
    LX_STATUS_IO_ERROR,
} lx_status;

#define LX_TOKEN_BUFFER_SIZE 32
#define LX_MIN_FILE_READ_SIZE 4096
typedef struct lexer_s {
    src_file* file;
    token token_buffer[LX_TOKEN_BUFFER_SIZE];
    token* token_buffer_end;
    token* loaded_tokens_start;
    token* loaded_tokens_head;
    char* file_buffer_start;
    char* file_buffer_head;
    char* file_buffer_end;
    char* file_buffer_pos;
    thread_context* tc;
    lx_status status;
} lexer;

int lx_init(lexer* tk, thread_context* tc);
void lx_fin(lexer* tk);

int lx_open_stream(lexer* tk, src_file* f, FILE* stream);
int lx_open_file(lexer* tk, src_file* f);
int lx_close_file(lexer* tk);

// any peek or consume invalidates all pointers to voided tokens
// and to strings these contained
// the void operation itself does not invalidate
token* lx_consume(lexer* tk);
token* lx_peek(lexer* tk);
token* lx_peek_2nd(lexer* p);
token* lx_peek_3rd(lexer* p);
token* lx_peek_nth(lexer* tk, ureg n);
void lx_void(lexer* tk);
void lx_void_n(lexer* tk, ureg n);

// get a token that was already peeked at
static inline token* lx_aquire(lexer* tk)
{
    return tk->loaded_tokens_start;
}
#endif
