#include "utils/threading.h"

typedef struct thread_error_log;
typedef struct error;

typedef enum error_type{
    TOKENIZER_IO_ERROR,
    TOKENIZER_INVALID_TOKEN,
    TOKENIZER_UNEXPECTED_EOF,
    TOKENIZER_ALLOCATION_FAILIURE,
    PARSER_UNEXPECTED_TOKEN,
    PARSER_UNEXPECTED_EOF,
    PARSER_ALLOCATION_FAILIURE,
    TYPESETTER_NAME_CLASH,
    TYPESETTER_ALLOCATION_FAILIURE,
}error_type;

typedef struct error{
    error_type type;
    error* follow_up;
}error;

typedef struct error_log{
    mutex mtx;
    bool error_occured;
    thread_error_log* thread_error_logs;
}error_log;

typedef struct thread_error_log{
    thread_error_log* next;
    error_log* error_log;
    error* error;
    //store this type of error here so we don't have to allocate on allocation failiure
    error alloc_failiure; 
}thread_error_log;

void error_emit(thread_context* tc, error* e);
void error_allocation_failiure(thread_context* tc);