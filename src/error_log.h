#pragma once

#include "utils/aseglist.h"
#include "src_map.h"
#include "utils/allocator.h"
#include "utils/c_extensions.h"
#include "utils/error.h"
#include "utils/pool.h"
#include "utils/threading.h"
#include "file_map.h"

typedef struct src_file_s src_file;

#define TAUC_MAX_GLOBAL_ERRORS 16
typedef enum PACK_ENUM error_stage_e {
    ES_TOKENIZER,
    ES_PARSER,
    ES_RESOLVER,
    ES_LIFETIMES,
    ES_LINKER,
    //...
} error_stage;

typedef enum error_kind_e {
    ET_ERROR,
    ET_BUG,
    ET_1_ANNOT,
    ET_MULTI_ANNOT,
} error_kind;

// a plain error at a position, no context message
typedef struct error_s {
    struct error_s* previous;
    bool warn;
    error_stage stage;
    error_kind kind;
    src_map* smap;
    ureg position;
    const char* message;
} error;

// an error where the position of the error is highlighted,
// but there aren't any other annotated positions
typedef struct error_annotated_s {
    error err;
    ureg end;
    const char* annotation;
} error_annotated;

// the error position is highlighed, and there are additional
// locations highlighted
typedef struct error_multi_annotated_s {
    error_annotated err_annot;
    ureg annot_count;
} error_multi_annotated;

typedef struct error_annotation_s {
    ureg start;
    ureg end;
    // NULL means just make sure that this range is shown,
    // don't underline it
    const char* annotation;
    src_map* smap;
} error_annotation;

typedef struct master_error_log_s master_error_log;

typedef struct error_log_s {
    master_error_log* mel;
    ureg tid;
    error* errors;
    error* critical_failiure_point;
    const char* critical_failiure_msg;
} error_log;

typedef struct master_error_log_s {
    aseglist error_logs;
    char* global_errors[TAUC_MAX_GLOBAL_ERRORS];
    file_map* filemap;
    ureg global_error_count;
    ureg tab_size;
    const char* tab_spaces;
    bool err_tty;
    ureg max_err_line_length;
    ureg sane_err_line_length;
    atomic_pool error_pool;
    bool alloc_failiure;
} master_error_log;

// MAIN THREAD ONLY
int master_error_log_init(master_error_log* mel, file_map* filemap);
void* master_error_log_alloc(master_error_log* mel, ureg size);
void master_error_log_report(master_error_log* mel, char* critical_error);
void master_error_log_unwind(master_error_log* mel);
void master_error_log_fin(master_error_log* mel);

// THREAD SAFE
error_log* error_log_create(
    master_error_log* mel); // these get free'd on master_error_log_fin
bool error_log_sane_state(error_log* el);

void error_log_report_general(
    error_log* el, error_stage stage, bool warn, const char* message);

void error_log_report_simple(
    error_log* el, error_stage stage, bool warn, const char* message,
    src_map* smap, ureg position);

error* error_log_create_error(
    error_log* el, error_stage stage, bool warn, const char* message,
    src_map* smap, ureg start, ureg end, const char* annot,
    ureg extra_annot_count);
void error_add_annotation(
    error* e, src_map* smap, ureg start, ureg end, const char* message);

void error_log_report(error_log* el, error* e);

void error_log_report_annotated(
    error_log* el, error_stage stage, bool warn, const char* message,
    src_map* smap, ureg start, ureg end, const char* annotation);
void error_log_report_annotated_twice(
    error_log* el, error_stage stage, bool warn, const char* message,
    src_map* smap, ureg start, ureg end, const char* annotation, src_map* smap2,
    ureg start2, ureg end2, const char* annotation2);
void error_log_report_annotated_thrice(
    error_log* el, error_stage stage, bool warn, const char* message,
    src_map* smap, ureg start, ureg end, const char* annotation, src_map* smap2,
    ureg start2, ureg end2, const char* annotation2, src_map* smap3,
    ureg start3, ureg end3, const char* annotation3);

void print_critical_thread_error(master_error_log* mel, const char* msg);
void print_critical_error(master_error_log* mel, const char* msg);
void print_critical_error_begin(master_error_log* mel);
void print_critical_error_end(master_error_log* mel);
// create a concatenated string stored inside the error memory pool
char* error_log_cat_strings_2(error_log* e, const char* s1, const char* s2);
char* error_log_cat_strings_3(
    error_log* e, const char* s1, const char* s2, const char* s3);
char* error_log_cat_strings_4(
    error_log* e, const char* s1, const char* s2, const char* s3,
    const char* s4);
char* error_log_cat_strings_5(
    error_log* e, const char* s1, const char* s2, const char* s3,
    const char* s4, const char* s5);
char* error_log_cat_strings(error_log* e, ureg count, const char** strs);

void error_log_report_allocation_failiure(error_log* el);
void error_log_report_synchronization_failiure(error_log* el);
void error_log_report_critical_failiure(error_log* el, const char* msg);
