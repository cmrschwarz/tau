#pragma once
#include "src_map.h"
#include "utils/allocator.h"
#include "utils/c_extensions.h"
#include "utils/error.h"
#include "utils/pool.h"
#include "utils/threading.h"

#define TAUC_MAX_GLOBAL_ERRORS 16
typedef enum PACK_ENUM error_stage {
    ES_TOKENIZER,
    ES_PARSER,
    ES_RESOLVER,
    ES_SIZER,
    //...
} error_stage;

typedef enum error_kind {
    ET_ERROR,
    ET_BUG,
    ET_1_ANNOT,
    ET_MULTI_ANNOT,
} error_kind;

typedef struct error {
    struct error* previous;
    bool warn;
    error_stage stage;
    error_kind kind;
    src_file* file;
    ureg position;
    const char* message;
} error;

typedef struct error_annotated {
    error error;
    ureg end;
    const char* annotation;
} error_annotated;

typedef struct error_multi_annotated {
    error_annotated err_annot;
    ureg annot_count;
} error_multi_annotated;

typedef struct error_annotation {
    ureg start;
    ureg end;
    const char* annotation;
    src_file* file;
} error_annotation;

typedef struct master_error_log master_error_log;

typedef struct error_log {
    struct error_log* next;
    error* errors;
    error* critical_failiure_point;
    const char* critical_failiure_msg;
    pool* error_mem_pool;
} error_log;

typedef struct master_error_log {
    error_log* error_logs;
    char* global_errors[TAUC_MAX_GLOBAL_ERRORS];
    ureg global_error_count;
    ureg tab_size;
    const char* tab_spaces;
    bool err_tty;
    sreg max_err_line_length;
    sreg sane_err_line_length;
} master_error_log;

// MAIN THREAD ONLY
extern master_error_log MASTER_ERROR_LOG;
int master_error_log_init();
void master_error_log_report(char* critical_error);
void master_error_log_unwind();
void master_error_log_fin();

// THREAD SAFE
void error_log_init(error_log* el, pool* error_mem_pool);
void error_log_fin(error_log* el);
bool error_log_sane_state(error_log* el);
void error_log_report_simple(
    error_log* el, error_stage stage, bool warn, const char* message,
    src_file* file, ureg position);
void error_log_report_annotated(
    error_log* el, error_stage stage, bool warn, const char* message,
    src_file* file, ureg start, ureg end, const char* annotation);
void error_log_report_annotated_twice(
    error_log* el, error_stage stage, bool warn, const char* message,
    src_file* file, ureg start, ureg end, const char* annotation,
    src_file* file2, ureg start2, ureg end2, const char* annotation2);
void error_log_report_annotated_thrice(
    error_log* el, error_stage stage, bool warn, const char* message,
    src_file* file, ureg start, ureg end, const char* annotation,
    src_file* file2, ureg start2, ureg end2, const char* annotation2,
    src_file* file3, ureg start3, ureg end3, const char* annotation3);

char* error_log_cat_strings_2(error_log* e, const char* s1, const char* s2);
char* error_log_cat_strings_3(
    error_log* e, const char* s1, const char* s2, const char* s3);
char* error_log_cat_strings(error_log* e, ureg count, const char** strs);

void* error_log_alloc(error_log* e, ureg size);
void error_log_report(error_log* el, error* e);
void error_log_report_allocation_failiure(error_log* el);
void error_log_report_synchronization_failiure(error_log* el);
void error_log_report_critical_failiure(error_log* el, const char* msg);
