#pragma once
#include "utils/threading.h"
#include "utils/allocator.h"
#include "utils/pool.h"
#include "src_map.h"
#include "utils/c_extensions.h"

#define OK 0 //zero to allow error handling like if(res){errorhandling} 
#define ERR -1
#define TAUC_MAX_GLOBAL_ERRORS 64
typedef enum PACK_ENUM error_stage{
    ES_TOKENIZER,
    ES_PARSER,
    ES_TYPESETTER,
    ES_SIZER,
    //...
}error_stage;

typedef enum error_type{
    ET_ERROR,
    ET_BUG,
    ET_1_ANNOT,
    ET_MULTI_ANNOT,
}error_type;

typedef struct error{
    struct error* previous;
    bool warn;
    error_stage stage;
    error_type type;
    file* file;
    ureg position;
    char* message;
}error;

typedef struct error_annotated{
    error error;
    ureg end;
    char* annotation;
}error_annotated;

typedef struct error_multi_annotated{
    error_annotated err_annot;
    ureg annot_count;
}error_multi_annotated;

typedef struct error_annotation{
    ureg start;
    ureg end;
    char* annotation;
}error_annotation;

typedef struct master_error_log master_error_log;

typedef struct error_log{
    struct error_log* next;
    error* errors;
    error* allocation_failure_point;
    error* synchronization_failure_point;
    pool* error_mem_pool;
}error_log;

typedef struct master_error_log{
    error_log* error_logs;
    char* global_errors[TAUC_MAX_GLOBAL_ERRORS];
    ureg global_error_count;
    ureg tab_size;
    char* tab_spaces;
    bool err_tty;
    sreg max_err_line_length;
    sreg sane_err_line_length;
}master_error_log;

//MAIN THREAD ONLY
extern master_error_log MASTER_ERROR_LOG;
int master_error_log_init();
void master_error_log_report(char* critical_error);
void master_error_log_unwind(pool* memory);
void master_error_log_fin();

//THREAD SAFE
void error_log_init(error_log* el, pool* error_mem_pool);
void error_log_fin(error_log* el);
bool error_log_sane_state(error_log* el);
void error_log_report_simple(
    error_log* el,
    error_stage stage,
    bool warn,
    char* message,
    file* file,
    ureg position
);
void error_log_report_annotated(
    error_log* el,
    error_stage stage,
    bool warn,
    char* message,
    file* file,
    ureg start,
    ureg end,
    char* annotation
);
void error_log_report_annotated_twice(
    error_log* el,
    error_stage stage,
    bool warn,
    char* message,
    file* file,
    ureg start1,
    ureg end1,
    char* annotation1,
    ureg start2,
    ureg end2,
    char* annotation2
);
void error_log_report_annotated_thrice(
    error_log* el,
    error_stage stage,
    bool warn,
    char* message,
    file* file,
    ureg start1,
    ureg end1,
    char* annotation1,
    ureg start2,
    ureg end2,
    char* annotation2,
    ureg start3,
    ureg end3,
    char* annotation3
);

char* error_log_cat_strings_2(error_log* e, char* s1, char* s2);
char* error_log_cat_strings_3(error_log* e, char* s1, char* s2, char* s3);
char* error_log_cat_strings(error_log* e, ureg count, char** strs);

void* error_log_alloc(error_log* e, ureg size);
void error_log_report(error_log* el, error* e);
void error_log_report_allocation_failiure(error_log* el);
void error_log_report_synchronization_failiure(error_log* el);


