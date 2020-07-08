#pragma once

#include "src_map.h"
#include "token.h"
#include "utils/c_extensions.h"
#include "utils/dbuffer.h"
#include "utils/sbuffer.h"
#include "utils/string.h"
#include <stdio.h>
#include <assert.h>

typedef struct thread_context_s thread_context;
typedef struct src_file_s src_file;
typedef struct pasted_str_s pasted_str;
typedef struct paste_evaluation_s paste_evaluation;

typedef enum PACK_ENUM lx_status {
    LX_STATUS_OK,
    LX_STATUS_EOF,
    LX_STATUS_TOKENIZATION_ERROR,
    LX_STATUS_IO_ERROR,
    LX_STATUS_FILE_UNAVAILABLE,
    LX_STATUS_FATAL_ERROR,
} lx_status;

#define LX_TOKEN_BUFFER_SIZE 32
#define LX_MIN_FILE_READ_SIZE 4096
typedef struct lexer_s {
    src_map* smap;
    char* pasted_str_pos;
    pasted_str* paste_str;
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

int lx_init(lexer* lx, thread_context* tc);
void lx_fin(lexer* lx);

int lx_open_paste(lexer* lx, paste_evaluation* pe, src_map* parent_smap);
void lx_close_paste(lexer* lx);

lx_status lx_open_file(lexer* lx, src_file* f);
void lx_close_file(lexer* lx);

// any peek or consume invalidates all pointers to voided tokens
// and to strings these contained
// the void operation itself does not invalidate
token* lx_consume(lexer* lx);
token* lx_peek(lexer* lx);
// this completely circumvents the lookahead and normal lexing structure
// no lookahead may be present when calling this
token* lx_consume_macro_string(lexer* lx);
token* lx_peek_2nd(lexer* p);
token* lx_peek_3rd(lexer* p);
token* lx_peek_nth(lexer* lx, ureg n);
void lx_void(lexer* lx);
void lx_void_n(lexer* lx, ureg n);

// get a token that was already peeked at
static inline token* lx_aquire(lexer* lx)
{
    assert(lx->loaded_tokens_start != lx->loaded_tokens_head);
    return lx->loaded_tokens_start;
}
