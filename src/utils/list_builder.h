#pragma once
#include "pool.h"

typedef struct list_build_segment {
    struct list_build_segment* next;
    struct list_build_segment* prev;
    void** end;
} list_build_segment;

typedef struct list_builder {
    pool* memsrc;
    list_build_segment* head_segment;
    void** head;
} list_builder;

int list_builder_init(list_builder* b, pool* memsrc, ureg initial_capacity);
void** list_builder_start(list_builder* b);
int list_builder_add(list_builder* b, void* el);
// returns begin of allocated list or NULL on failiure
void** list_builder_pop_list(
    list_builder* b, void** list_start, pool* memtgt, ureg* count, ureg premem,
    ureg postmem);

void** list_builder_pop_list_zt(
    list_builder* b, void** list_start, pool* memtgt, ureg* count);