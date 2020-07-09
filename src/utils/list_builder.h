#pragma once
#include "pool.h"

typedef struct list_builder_segment_s {
    struct list_builder_segment_s* next;
    struct list_builder_segment_s* prev;
    void* end;
} list_builder_segment;

typedef struct list_builder_s {
    pool* memsrc;
    list_builder_segment* head_segment;
    void* head;
} list_builder;

typedef struct list_builder_rev_iter {
    list_builder_segment* curr_seg;
    void** list_start;
    void** pos;
} list_builder_rev_iter;

void list_builder_rev_iter_init(
    list_builder_rev_iter* it, list_builder* lb, void** list);
void** list_builder_rev_iter_prev(list_builder_rev_iter* it, ureg elem_size);

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
