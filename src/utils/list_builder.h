#pragma once
#include "pool.h"

typedef struct list_build_segment_s {
    struct list_build_segment_s* next;
    struct list_build_segment_s* prev;
    void* end;
} list_build_segment;

typedef struct list_builder_s {
    pool* memsrc;
    list_build_segment* head_segment;
    void* head;
} list_builder;

int list_builder_init(list_builder* b, pool* memsrc, ureg initial_capacity);
void list_builder_fin();
void** list_builder_start(list_builder* b);
void* list_builder_start_blocklist(list_builder* b);
int list_builder_add(list_builder* b, void* el);
int list_builder_add_block(list_builder* b, void* block, ureg size);
// returns begin of allocated list or NULL on failiure
void** list_builder_pop_list(
    list_builder* b, void** list_start, pool* memtgt, ureg* count, ureg premem,
    ureg postmem);

void**
list_builder_pop_list_zt(list_builder* b, void** list_start, pool* memtgt);

void* list_builder_pop_block_list(
    list_builder* b, void* list_start, pool* tgtmem, ureg* list_size,
    ureg premem, ureg postmem);

void**
list_builder_create_single_entry_zt(list_builder* b, void* entry, pool* tgtmem);

void* list_builder_pop_block_list_zt(
    list_builder* b, void* list_start, pool* tgtmem);

void list_builder_drop_list(list_builder* b, void* list_start);

