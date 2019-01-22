#pragma once
#include "utils/threading.h"
#include "utils/allocator.h"
#include "utils/pool.h"
#include "src_map.h"

#define OK 0 //zero to allow error handling like if(res){errorhandling} 
#define ERR -1
#define TAUC_MAX_GLOBAL_ERRORS 64
typedef enum error_stage{
    ES_TOKENIZER,
    ES_PARSER,
    ES_TYPESETTER,
    ES_SIZER,
    //...
}error_stage;

typedef enum error_type{
    ET_ERROR,
    ET_1_ANNOT,
    ET_2_ANNOT,
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

typedef struct error_1_annotation{
    error error;
    ureg length;
    char* annotation;
}error_1_annotation;

typedef struct error_2_annotations{
    error error;
    ureg length1;
    char* annotation1;
    ureg position2;
    ureg length2;
    char* annotation2;
}error_2_annotations;

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
}master_error_log;

//MAIN THREAD ONLY
int master_error_log_init();
void master_error_log_report(char* critical_error);
void master_error_log_unwind(pool* memory);
void master_error_log_fin();
void error_log_init(error_log* el, pool* error_mem_pool);
void error_log_fin(error_log* el);
void error_log_report_error(
    error_log* el,
    error_stage stage,
    bool warn,
    char* message,
    file* file,
    ureg position
);
void error_log_report_error_1_annotation(
    error_log* el,
    error_stage stage,
    bool warn,
    char* message,
    file* file,
    ureg position,
    ureg length,
    char* annotation
);
void error_log_report_error_2_annotations(
    error_log* el,
    error_stage stage,
    bool warn,
    char* message,
    file* file,
    ureg position,
    ureg length1,
    char* annotation1,
    ureg position2,
    ureg length2,
    char* annotation2
);
//THREAD SAFE
error* error_log_alloc(error_log* e, ureg size);
void error_log_report(error_log* el, error* e);
void error_log_report_allocation_failiure(error_log* el);
void error_log_report_synchronization_failiure(error_log* el);


