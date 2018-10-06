#include "token_type.h"
#include "utils/dbuffer.h"
#include "utils/sbuffer.h"
#include <stdio.h>
#include "utils/string.h"


typedef struct {
    token_type type;
    string str;
    ureg filepos;
    ureg column;
    ureg line;
}token;

#define TK_TOKEN_BUFFER_SIZE 32
typedef struct{
    char* filename;
    FILE* file;
    token token_buffer[TK_TOKEN_BUFFER_SIZE];
    token* token_buffer_end;
    token* loaded_tokens_start;
    token* loaded_tokens_head;
    dbuffer file_buffer;
    char* file_buffer_pos;
    thread_allocator* tal;
}tokenizer;

int tk_init(tokenizer* tk, thread_allocator* tal);
void tk_fin(tokenizer* tk);
 
int tk_open_file(tokenizer* tk, char* filename);
void tk_close_file(tokenizer* tk);

token* tk_peek(tokenizer* tk);
token* tk_peek_2nd(tokenizer* p);
token* tk_peek_3rd(tokenizer* p);
token* tk_peek_nth(tokenizer* tk, int n);
void tk_void(tokenizer* tk);
void tk_void_n(tokenizer* tk, int n);
