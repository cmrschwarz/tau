#include "list_builder.h"
#include "error_log.h"
#include "math_utils.h"
#include "memory.h"
#include "panic.h"
#include <assert.h>

int list_builder_init(list_builder* b, pool* memsrc, ureg initial_capacity)
{
    b->memsrc = memsrc;
    ureg size = sizeof(list_builder_segment) + initial_capacity * sizeof(void*);
    list_builder_segment* first = pool_alloc(memsrc, size);
    if (!first) return ERR;
    first->prev = NULL;
    first->next = NULL;
    first->end = ptradd(first, size);
    b->head_segment = first;
    b->head = ptradd(first, sizeof(list_builder_segment));
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
    if (ptradd(b->head, block_size) < b->head_segment->end) { //<=!
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
        list_builder_segment* next = b->head_segment->next;
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
        b->head = ptradd(next, sizeof(list_builder_segment));
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
    list_builder_segment* s = b->head_segment;
    if ((void*)s <= list_start && s->end > (void*)list_start) {
        size = ptrdiff(b->head, list_start);
        *list_size = size;
        tgt = pool_alloc(tgtmem, size + premem + postmem);
        if (!tgt) return NULL;
        memcpy(ptradd(tgt, premem), list_start, size);
        b->head = list_start;
        return tgt;
    }
    ureg last_seg_size =
        ptrdiff(b->head, b->head_segment) - sizeof(list_builder_segment);
    size = last_seg_size;
    while (true) {
        s = s->prev;
        assert(s); // the list_start has to be somewhere
        if ((void*)s < list_start && s->end >= list_start) {
            size += ptrdiff(s->end, list_start);
            break;
        }
        size += ptrdiff(s->end, s) - sizeof(list_builder_segment);
    }
    *list_size = size;
    tgt = (void**)pool_alloc(tgtmem, size + premem + postmem);
    if (!tgt) return NULL;
    list_builder_segment* head_old = b->head_segment;
    b->head_segment = s;
    void** h = ptradd(tgt, premem);
    void** start = list_start;
    while (s != head_old) {
        ureg seg_size = ptrdiff(s->end, start);
        memcpy(h, start, seg_size);
        s = s->next;
        h = ptradd(h, seg_size);
        start = ptradd(s, sizeof(list_builder_segment));
    }
    memcpy(h, ptradd(s, sizeof(list_builder_segment)), last_seg_size);
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
    list_builder_segment* s = b->head_segment;
    while (true) {
        if ((void*)s <= list_start && s->end > (void*)list_start) {
            b->head_segment = s;
            break;
        }
        s = s->prev;
        assert(s);
    }
}

void**
list_builder_create_single_entry_zt(list_builder* b, void* entry, pool* tgtmem)
{
    void** res = pool_alloc(tgtmem, sizeof(void*) * 2);
    if (!res) return NULL;
    res[0] = entry;
    res[1] = NULL;
    return res;
}

void list_builder_rev_iter_init(
    list_builder_rev_iter* it, list_builder* lb, void** list)
{
    it->curr_seg = lb->head_segment;
    it->pos = lb->head;
    it->list_start = list;
}
void** list_builder_rev_iter_prev(list_builder_rev_iter* it, ureg elem_size)
{
    if (it->pos == it->list_start) return NULL;
    if (it->pos == ptradd(it->curr_seg, sizeof(list_builder_segment))) {
        it->curr_seg = it->curr_seg->prev;
        it->pos = it->curr_seg->end;
    }
    // this could happen if segments are too small
    assert(
        ptrdiff(it->pos, it->curr_seg) >=
        sizeof(list_builder_segment) + elem_size);
    it->pos = ptrsub(it->pos, elem_size);
    return it->pos;
}
