#pragma once
#include "pool.h"

typedef struct list_build_segment{
    struct decl_list_build_segment* next;
    struct decl_list_build_segment* prev;
    void** end;
}list_build_segment;

typedef struct list_builder{
    pool* memsrc;
    list_build_segment* head_segment;
    void** head;
}list_builder;



int list_builder_init(list_builder* b, pool* memsrc, ureg initial_capacity);
void** list_builder_start(list_builder* b);
int list_builder_add(list_builder* b, void* el);
int list_builder_pop_list(
    list_builder* b, void** list_start, void*** tgt, pool* tgtmem
);