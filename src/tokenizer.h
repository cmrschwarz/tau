#include "token.h"
#include "utils/dbuffer.h"
#include "utils/sbuffer.h"
#include <stdio.h>
#include "utils/string.h"
#include "src_map.h"

#define TK_TOKEN_BUFFER_SIZE 32
typedef struct{
    FILE* file_stream;
    file* file;
    token token_buffer[TK_TOKEN_BUFFER_SIZE];
    token* token_buffer_end;
    token* loaded_tokens_start;
    token* loaded_tokens_head;
    dbuffer file_buffer;
    char* string_to_keep;
    char* file_buffer_pos;
    thread_allocator* tal;
    int status;
}tokenizer;

int tk_init(tokenizer* tk, thread_allocator* tal);
void tk_fin(tokenizer* tk);
 
int tk_open_file(tokenizer* tk, file* f);
int tk_close_file(tokenizer* tk);

token* tk_consume(tokenizer* tk);
token* tk_peek(tokenizer* tk);
token* tk_peek_2nd(tokenizer* p);
token* tk_peek_3rd(tokenizer* p);
token* tk_peek_nth(tokenizer* tk, int n);
void tk_void(tokenizer* tk);
void tk_void_n(tokenizer* tk, int n);
