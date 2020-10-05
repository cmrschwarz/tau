#pragma once

#include "allocator.h"
// this gives us 5 elements in the first segment since the metadata is 3 void*
#define STACK_SEG_MIN_SIZE (sizeof(void*) * 8)
typedef struct stack_segment_s {
    struct stack_segment_s* prev;
    struct stack_segment_s* next;
    void** end;
} stack_segment;

typedef struct stack_s {
    stack_segment* curr_seg;
    void** head;
    pool* mempool;
} stack;

typedef struct stack_iter_s {
    stack_segment* curr_seg;
    void** head;
} stack_iter;

static inline bool stack_seg_empty(stack_segment* seg, void** head)
{
    return ptrdiff(head, seg) == sizeof(stack_segment);
}

static inline ureg stack_seg_elems(stack_segment* seg, void** head)
{
    return (ptrdiff(head, seg) - sizeof(stack_segment)) / sizeof(void*);
}
static inline void stack_iter_begin(stack_iter* sit, stack* s)
{
    sit->curr_seg = s->curr_seg;
    sit->head = s->head;
}
static inline void* stack_iter_prev(stack_iter* sit)
{
    if (stack_seg_empty(sit->curr_seg, sit->head)) {
        if (sit->curr_seg->prev == NULL) return NULL;
        sit->curr_seg = sit->curr_seg->prev;
        sit->head = sit->curr_seg->end;
    }
    sit->head--;
    return *sit->head;
}
static inline void stack_set_end(stack* s, stack_iter* sit)
{
    s->curr_seg = sit->curr_seg;
    s->head = sit->head;
}
static inline ureg stack_is_empty(stack* s)
{
    return s->curr_seg->prev == NULL && stack_seg_empty(s->curr_seg, s->head);
}

static inline stack_segment*
stack_alloc_segment(stack* s, ureg size, stack_segment* prev)
{
    stack_segment* seg = (stack_segment*)pool_alloc(s->mempool, size);
    if (!s) return NULL;
    seg->end = (void**)ptradd(seg, size);
    seg->prev = prev;
    seg->next = NULL;
    return seg;
}

static inline int stack_init(stack* s, pool* mempool)
{
    s->mempool = mempool;
    s->curr_seg = stack_alloc_segment(s, STACK_SEG_MIN_SIZE, NULL);
    if (!s->curr_seg) return ERR;
    s->head = (void**)ptradd(s->curr_seg, sizeof(stack_segment));
    return OK;
}
static inline void stack_fin(stack* s)
{
}
static inline int stack_push(stack* s, void* data)
{
    if (s->head == s->curr_seg->end) {
        if (s->curr_seg->next) {
            s->curr_seg = s->curr_seg->next;
        }
        else {
            stack_segment* ss = stack_alloc_segment(
                s, ptrdiff(s->curr_seg->end, s->curr_seg) * 2, s->curr_seg);
            if (!ss) return ERR;
            s->curr_seg->next = ss;
            s->curr_seg = ss;
        }
        s->head = (void**)ptradd(s->curr_seg, sizeof(stack_segment));
    }
    *s->head = data;
    s->head++;
    return OK;
}
static inline void* stack_pop(stack* s)
{
    if (stack_seg_empty(s->curr_seg, s->head)) {
        if (s->curr_seg->prev == NULL) return NULL;
        s->curr_seg = s->curr_seg->prev;
        s->head = s->curr_seg->end;
    }
    s->head--;
    return *s->head;
}
static inline void stack_clear(stack* s)
{
    stack_segment* seg = s->curr_seg;
    while (seg->prev) seg = seg->prev;
    s->curr_seg = seg;
    s->head = (void**)ptradd(seg, sizeof(stack_segment));
}
static inline void* stack_peek(stack* s)
{
    if (!stack_seg_empty(s->curr_seg, s->head)) return *(s->head - 1);
    if (s->curr_seg->prev == NULL) return NULL;
    return *(s->curr_seg->prev->end - 1);
}
static inline int stack_set(stack* s, void* value)
{
    if (!stack_seg_empty(s->curr_seg, s->head)) {
        *(s->head - 1) = value;
        return OK;
    }
    if (s->curr_seg->prev == NULL) return ERR;
    *(s->curr_seg->prev->end - 1) = value;
    return OK;
}
static inline void* stack_peek_prev(stack* s)
{
    ureg elems = stack_seg_elems(s->curr_seg, s->head);
    if (elems > 1) return *(s->head - 2);
    if (s->curr_seg->prev == NULL) return NULL;
    return *(s->curr_seg->prev->end - (2 - elems));
}
// returns the element at position <stack top element pos> - i
static inline void* stack_peek_nth(stack* s, ureg i)
{
    stack_segment* seg = s->curr_seg;
    void** head = s->head;
    while (true) {
        ureg elems = stack_seg_elems(seg, head);
        if (elems >= i) return *(head - i - 1);
        seg = seg->prev;
        assert(seg);
        i -= elems;
        head = seg->end;
    }
}

static inline ureg stack_element_count(stack* s)
{
    ureg curr_seg_size = ptrdiff(s->curr_seg->end, s->curr_seg);
    ureg seg_count = ulog2(curr_seg_size) - ulog2(STACK_SEG_MIN_SIZE) + 1;
    // size of the segments combined
    ureg space = ((1 << seg_count) - 1) * STACK_SEG_MIN_SIZE;
    // subtract size of the metadata
    space -= seg_count * sizeof(stack_segment);
    // subtract unused slots in the current segment
    space -= ptrdiff(s->curr_seg->end, s->head);
    return space / sizeof(void*);
}
