#pragma once

#include "allocator.h"

typedef struct stack_segment_s {
    struct stack_segment_s* prev;
    struct stack_segment_s* next;
    void** end;
} stack_segment;

typedef struct stack_s {
    stack_segment* curr_seg;
    void** curr_seg_start;
    void** head;
    pool* mempool;
} stack;

typedef struct stack_state_s {
    stack_segment* curr_seg;
    void** head;
} stack_state;

static inline void stack_state_save(stack_state* ss, stack* s)
{
    ss->curr_seg = s->curr_seg;
    ss->head = s->head;
}
static inline void stack_state_apply(stack_state* ss, stack* s)
{
    s->curr_seg = ss->curr_seg;
    s->head = ss->head;
}
static inline ureg stack_is_empty(stack* s)
{
    return (s->curr_seg->prev == NULL && s->head == s->curr_seg_start);
}

static inline stack_segment*
stack_alloc_segment(stack* s, ureg size, stack_segment* prev)
{
    size = sizeof(void*) * size;
    stack_segment* seg = (stack_segment*)pool_alloc(s->mempool, size);
    if (!s) return NULL;
    seg->end = (void**)ptradd(seg, size);
    seg->prev = prev;
    seg->next = NULL;
    return seg;
}
static inline void stack_set_curr_seg(stack* s, stack_segment* seg)
{
    s->curr_seg = seg;
    s->curr_seg_start = (void**)ptradd(seg, sizeof(stack_segment));
}
static inline int stack_init(stack* s, pool* mempool)
{
    s->mempool = mempool;
    s->curr_seg = stack_alloc_segment(s, 16, NULL);
    if (!s->curr_seg) return ERR;
    stack_set_curr_seg(s, s->curr_seg);
    s->head = s->curr_seg_start;
    return OK;
}
static inline void stack_fin(stack* s)
{
}
static inline int stack_push(stack* s, void* data)
{
    if (s->head == s->curr_seg->end) {
        if (!s->curr_seg->next) {
            s->curr_seg->next = stack_alloc_segment(
                s, ptrdiff(s->curr_seg->end, s->curr_seg) * 2, s->curr_seg);
            if (!s->curr_seg->next) return ERR;
            stack_set_curr_seg(s, s->curr_seg->next);
            s->head = s->curr_seg_start;
        }
    }
    *s->head = data;
    s->head++;
    return OK;
}
static inline void* stack_pop(stack* s)
{
    if (s->head == s->curr_seg_start) {
        if (s->curr_seg->prev == NULL) return NULL;
        stack_set_curr_seg(s, s->curr_seg->prev);
        s->head = s->curr_seg->end;
    }
    s->head--;
    return *s->head;
}
static inline void stack_clear(stack* s)
{
    while (s->curr_seg->prev) {
        s->curr_seg = s->curr_seg->prev;
    }
    stack_set_curr_seg(s, s->curr_seg);
    s->head = s->curr_seg_start;
}
static inline void* stack_peek(stack* s)
{
    if (s->head != s->curr_seg_start) return *(s->head - 1);
    if (s->curr_seg->prev == NULL) return NULL;
    return *(s->curr_seg->prev->end - 1);
}
static inline int stack_set(stack* s, void* value)
{
    if (s->head != s->curr_seg_start) {
        *(s->head - 1) = value;
        return OK;
    }
    if (s->curr_seg->prev == NULL) return ERR;
    *(s->curr_seg->prev->end - 1) = value;
    return OK;
}
static inline void* stack_peek_prev(stack* s)
{
    if (s->head > s->curr_seg_start + 1) return *(s->head - 2);
    if (s->curr_seg->prev == NULL) return NULL;
    return *(s->curr_seg->end - 1);
}
static inline void* stack_peek_nth(stack* s, int i)
{
    if (s->head > s->curr_seg_start + i) return *(s->head - i - 1);
    stack_state ss;
    void* res;
    stack_state_save(&ss, s);
    while (s->head <= s->curr_seg_start + i) {
        if (s->curr_seg->prev == NULL) return NULL;
        stack_set_curr_seg(s, s->curr_seg->prev);
        s->head = s->curr_seg->end;
        i -= s->head - s->curr_seg_start;
    }
    res = *(s->head - i - 1);
    stack_state_apply(&ss, s);
    return res;
}

static inline ureg stack_size(stack* s)
{
    ureg size = ptrdiff(s->head, s->curr_seg_start);
    stack_segment* seg = s->curr_seg->prev;
    while (seg) {
        size += ptrdiff(seg->end, seg) - sizeof(stack_segment);
        seg = seg->prev;
    }
    return size / sizeof(void*);
}

static inline void
stack_pop_to_list(stack* s, stack_state* start, stack_state* end, void** tgt)
{
    stack_state_apply(end, s);
    while (s->head != start->head) {
        *tgt = stack_pop(s);
        tgt++;
    }
}

