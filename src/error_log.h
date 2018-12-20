#pragma once
#include "utils/threading.h"
#include "utils/allocator.h"
#include "utils/pool.h"

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

typedef struct master_error_log master_error_log;

typedef struct error_log{
    struct error_log* next;
    error* errors;
    error* allocation_failure_point;
}error_log;

typedef struct master_error_log{
    atomic_bool error_occured;
    mutex mtx;
    error_log* error_logs;
    int error_log_error;
}master_error_log;

int master_error_log_init();
int master_error_log_unwind(int r);

void master_error_log_fin();
error_log* error_log_aquire();
void error_log_release(error_log* tel);


void error_log_report(error_log* tel, error* e);
void error_log_report_allocation_failiure(error_log* tel);
