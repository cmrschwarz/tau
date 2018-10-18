#pragma once
#include "utils/threading.h"

struct thread_context;

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
    struct error* previous;
}error;

typedef struct thread_error_log{
    struct thread_error_log* next;
    error* errors;
    error* allocation_failure_point;
}thread_error_log;

typedef struct error_log{
    atomic_bool error_occured;
    mutex mtx;
    thread_error_log* thread_error_logs;
}error_log;

int error_log_init(error_log* el);
void error_log_fin(error_log* el);
int thread_error_log_init(error_log* el, thread_error_log* tel);
void thread_error_log_fin(thread_error_log* tel);


void error_log_report(struct thread_context* tc, error* e);
void error_log_report_allocation_failiure(struct thread_context* tc);