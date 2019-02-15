#pragma once
#include "src_map.h"
#include "token.h"
#include "utils/c_extensions.h"
#include "utils/dbuffer.h"
#include "utils/sbuffer.h"
#include "utils/string.h"
#include <stdio.h>

typedef enum PACK_ENUM tk_status {
    TK_STATUS_OK,
    TK_STATUS_EOF,
    TK_STATUS_TOKENIZATION_ERROR,
    TK_STATUS_IO_ERROR,
} tk_status;

#define TK_TOKEN_BUFFER_SIZE 32
#define TK_MIN_FILE_READ_SIZE 4096
typedef struct {
    FILE* file_stream;
    file* file;
    token token_buffer[TK_TOKEN_BUFFER_SIZE];
    token* token_buffer_end;
    token* loaded_tokens_start;
    token* loaded_tokens_head;
    char* file_buffer_start;
    char* file_buffer_head;
    char* file_buffer_end;
    char* file_buffer_pos;
    thread_context* tc;
    tk_status status;
} tokenizer;

int tk_init(tokenizer* tk, thread_context* tc);
void tk_fin(tokenizer* tk);

int tk_open_stream(tokenizer* tk, file* f, FILE* stream);
int tk_open_file(tokenizer* tk, file* f);
int tk_close_file(tokenizer* tk);

token* tk_consume(tokenizer* tk);
token* tk_peek(tokenizer* tk);
token* tk_peek_2nd(tokenizer* p);
token* tk_peek_3rd(tokenizer* p);
token* tk_peek_nth(tokenizer* tk, int n);
void tk_void(tokenizer* tk);
void tk_void_n(tokenizer* tk, int n);
