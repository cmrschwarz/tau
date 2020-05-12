#pragma once

#include "allocator.h"
#include "math_utils.h"
#include "types.h"
#include <assert.h>

typedef struct sbuffer_segment_s {
    void* tail;
    void* end;
    struct sbuffer_segment_s* prev;
    struct sbuffer_segment_s* next;
} sbuffer_segment;

typedef struct sbuffer_s {
    // if this segment becomes empty it is moved after head_seg
    sbuffer_segment* first_seg;
    sbuffer_segment* tail_seg;
    ureg biggest_seg_size;
} sbuffer;

typedef struct sbuffer_iterator_s {
    sbuffer_segment* seg;
    void* pos;
} sbuffer_iterator;

int sbuffer_init(sbuffer* sb, ureg initial_capacity);
void sbuffer_fin(sbuffer* sb);
void* sbuffer_prepend(sbuffer* sb, ureg size);
void* sbuffer_append(sbuffer* sb, ureg size);
void* sbuffer_front(sbuffer* sb, ureg size);
void* sbuffer_back(sbuffer* sb, ureg size);
ureg sbuffer_get_used_size(sbuffer* sb);
bool sbuffer_is_empty(sbuffer* sb);

// the following methods invalidate all iterators to elements after the
// removed / inserted one
void sbuffer_remove(sbuffer* sb, sbuffer_iterator* sbi, ureg size);
void sbuffer_remove_prev(sbuffer* sb, sbuffer_iterator* sbi, ureg size);
void* sbuffer_insert(sbuffer* sb, sbuffer_iterator* sbi, ureg size);

// invalidates all iterators
void sbuffer_remove_front(sbuffer* sb, ureg size);

// invalidates no iterators
void sbuffer_remove_back(sbuffer* sb, ureg size);

void sbuffer_compact(sbuffer* sb);
void sbuffer_clear(sbuffer* sb);
static inline sbuffer_iterator sbuffer_iterator_begin(sbuffer* sb)
{
    sbuffer_iterator sbi;
    sbi.seg = sb->first_seg;
    sbi.pos = ptradd(sb->first_seg, sizeof(sbuffer_segment));
    return sbi;
}
static inline sbuffer_iterator sbuffer_iterator_begin_at_end(sbuffer* sb)
{
    sbuffer_iterator sbi;
    sbi.seg = sb->tail_seg;
    sbi.pos = sb->tail_seg->tail;
    return sbi;
}
static inline void* sbuffer_iterator_next(sbuffer_iterator* sbi, ureg size)
{
    while (true) {
        if (ptrdiff(sbi->seg->tail, sbi->pos) != 0) {
            assert(ptrdiff(sbi->seg->tail, sbi->pos) >= size);
            void* res = sbi->pos;
            sbi->pos = ptradd(sbi->pos, size);
            return res;
        }
        if (sbi->seg->next == NULL) return NULL;
        sbi->seg = sbi->seg->next;
        sbi->pos = (u8*)(sbi->seg + 1);
        // there are no empty segments in the middle, so we can return without
        // checking trailing segments
        if (sbi->seg->tail == ptradd(sbi->seg, sizeof(sbuffer_segment))) {
            return NULL;
        }
    }
}
static inline void* sbuffer_iterator_previous(sbuffer_iterator* sbi, ureg size)
{
    while (true) {
        if (ptrdiff(sbi->pos, sbi->seg + 1) >= size) {
            sbi->pos = ptrsub(sbi->pos, size);
            return sbi->pos;
        }
        if (sbi->seg->prev == NULL) return NULL;
        sbi->seg = sbi->seg->prev;
        sbi->pos = sbi->seg->tail;
    }
}
static inline void* sbuffer_iterator_get(sbuffer_iterator* sbi, ureg size)
{
    if (ptrdiff(sbi->seg->tail, sbi->pos) < size) return NULL;
    return sbi->pos;
}

