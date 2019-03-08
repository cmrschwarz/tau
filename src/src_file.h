#pragma once
#include "src_map.h"
#include "utils/rwslock.h"
typedef struct src_file {
    char* path;
    struct src_file* next;
    src_map src_map;
    atomic_bool parsed;
    rwslock lock;
} source_file;

int src_file_init(src_file* f, thread_context* tc, char* path);
void src_file_fin(src_file* f);