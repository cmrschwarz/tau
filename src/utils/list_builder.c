#include "list_builder.h"
#include "error_log.h"
#include "math_utils.h"
#include "memory.h"
#include "panic.h"

int list_builder_init(list_builder* b, pool* memsrc, ureg initial_capacity)
{
    b->memsrc = memsrc;
    ureg size = sizeof(list_build_segment) + initial_capacity * sizeof(void*);
    list_build_segment* first = pool_alloc(memsrc, size);
    if (!first) return ERR;
    first->prev = NULL;
    first->next = NULL;
    first->end = ptradd(first, size);
    b->head_segment = first;
    b->head = ptradd(first, sizeof(list_build_segment));
    return OK;
}
void list_builder_fin()
{
}
void** list_builder_start(list_builder* b)
{
    return b->head;
}
void* list_builder_start_blocklist(list_builder* b)
{
    return (void*)b->head;
}
int list_builder_add_block(list_builder* b, void* block, ureg block_size)
{
    assert(block_size % sizeof(void*) == 0);
    void** head_new = ptradd(b->head, block_size);
    if (ptradd(b->head, block_size) < b->head_segment->end) {
        memcpy(b->head, block, block_size);
        b->head = head_new;
    }
    else {
        ureg size_left = ptrdiff(b->head_segment->end, b->head);
        if (size_left) {
            memcpy(b->head, block, size_left);
            block = ptradd(block, size_left);
            block_size -= size_left;
        }
        list_build_segment* next = b->head_segment->next;
        if (next == NULL) {
            ureg size = ptrdiff(b->head_segment->end, b->head_segment) * 2;
            next = pool_alloc(b->memsrc, size);
            if (!next) return ERR;
            next->end = ptradd(next, size);
            next->next = NULL;
            next->prev = b->head_segment;
            b->head_segment->next = next;
        }
        b->head_segment = next;
        b->head = ptradd(next, sizeof(list_build_segment));
        return list_builder_add_block(b, block, block_size);
    }
    return OK;
}
int list_builder_add(list_builder* b, void* el)
{
    return list_builder_add_block(b, &el, sizeof(void*));
}
void* list_builder_pop_block_list(
    list_builder* b, void* list_start, pool* tgtmem, ureg* list_size,
    ureg premem, ureg postmem)
{
    void** tgt;
    ureg size = 0;
    list_build_segment* s = b->head_segment;
    if ((void*)s <= list_start && s->end > (void*)list_start) {
        size = ptrdiff(b->head, list_start);
        *list_size = size;
        tgt = pool_alloc(tgtmem, size + premem + postmem);
        if (!tgt) return NULL;
        memcpy(ptradd(tgt, premem), list_start, size);
        b->head = list_start;
        return tgt;
    }
    size = ptrdiff(b->head, b->head_segment) - sizeof(list_build_segment);
    do {
        s = s->prev;
        size += ptrdiff(s->end, s) - sizeof(list_build_segment);
    } while (list_start <= (void*)s || list_start >= s->end);
    *list_size = size;
    tgt = (void**)pool_alloc(tgtmem, size + premem + postmem);
    if (!tgt) return NULL;
    void** h = ptradd(tgt, premem);
    size = ptrdiff(s->end, list_start);
    memcpy(h, list_start, size);
    list_build_segment* head_old = b->head_segment;
    b->head_segment = s;
    while (s != head_old->next) {
        h += size;
        void** start = ptradd(s, sizeof(list_build_segment));
        size = ptrdiff(s->end, start);
        memcpy(h, start, size);
        s = s->next;
    }
    b->head = list_start;
    return tgt;
}

void* list_builder_pop_block_list_zt(
    list_builder* b, void* list_start, pool* tgtmem)
{
    ureg size;
    void* res = list_builder_pop_block_list(
        b, list_start, tgtmem, &size, 0, sizeof(void*));
    if (!res) return NULL;
    *(void**)ptradd(res, size) = NULL;
    return res;
}

void** list_builder_pop_list(
    list_builder* b, void** list_start, pool* tgtmem, ureg* count, ureg premem,
    ureg postmem)
{
    void** res = (void**)list_builder_pop_block_list(
        b, (void*)list_start, tgtmem, count, premem, postmem);
    if (!res) return NULL;
    *count /= sizeof(void*);
    return res;
}
void**
list_builder_pop_list_zt(list_builder* b, void** list_start, pool* memtgt)
{
    ureg count;
    void** tgt =
        list_builder_pop_list(b, list_start, memtgt, &count, 0, sizeof(void*));
    if (!tgt) return NULL;
    tgt[count] = NULL;
    return tgt;
}

void list_builder_drop_list(list_builder* b, void* list_start)
{
    b->head = list_start;
}