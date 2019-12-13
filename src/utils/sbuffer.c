#include "sbuffer.h"
#include "allocator.h"
#include "math_utils.h"
#include <assert.h>
#include <memory.h>
static inline sbuffer_segment* sbuffer_segment_create(ureg size)
{
    sbuffer_segment* seg = (sbuffer_segment*)tmalloc(size);
    if (!seg) return NULL;
    seg->tail = (void*)(seg + 1);
    seg->end = ptradd(seg, size);
    return seg;
}
static inline sbuffer_segment* sbuffer_segment_append_after_tail(sbuffer* sb)
{
    sb->biggest_seg_size *= 2;
    sbuffer_segment* s = sbuffer_segment_create(sb->biggest_seg_size);
    if (!s) return NULL;
    s->prev = sb->tail_seg;
    s->next = NULL;
    sb->tail_seg->next = s;
    sb->tail_seg = s;
    return s;
}
int sbuffer_init(sbuffer* sb, ureg initial_capacity)
{
    sb->biggest_seg_size =
        ceil_to_pow2(initial_capacity + sizeof(sbuffer_segment));
    sbuffer_segment* s = sbuffer_segment_create(sb->biggest_seg_size);
    if (!s) return -1;
    s->next = NULL;
    s->prev = NULL;
    sb->first_seg = s;
    sb->tail_seg = s;
    return 0;
}
void sbuffer_fin(sbuffer* sb)
{
    sbuffer_segment* s = sb->first_seg;
    while (s) {
        sbuffer_segment* next = s->next;
        tfree(s);
        s = next;
    }
}
ureg sbuffer_get_used_size(sbuffer* sb)
{
    ureg res = 0;
    for (sbuffer_segment* s = sb->first_seg; s != NULL; s = s->next) {
        res += ptrdiff(s->tail, s + 1);
    }
    return res;
}
bool sbuffer_is_empty(sbuffer* sb)
{
    return sbuffer_get_used_size(sb) == 0; // TODO: optimize
}
void* sbuffer_front(sbuffer* sb, ureg size)
{
    assert(ptrdiff(sb->first_seg->tail, sb->first_seg + 1) >= size);
    return (void*)(sb->first_seg + 1);
}
void* sbuffer_back(sbuffer* sb, ureg size)
{
    assert(ptrdiff(sb->tail_seg->tail, sb->tail_seg + 1) >= size);
    return ptrsub(sb->tail_seg->tail, size);
}
void* sbuffer_prepend(sbuffer* sb, ureg size)
{
    // PERF: optimize!
    sbuffer_iterator sbi = sbuffer_iterator_begin(sb);
    return sbuffer_insert(sb, &sbi, size);
}
void* sbuffer_append(sbuffer* sb, ureg size)
{
    while (true) {
        void* tail = sb->tail_seg->tail;
        if (ptrdiff(sb->tail_seg->end, tail) >= size) {
            sb->tail_seg->tail = ptradd(tail, size);
            return tail;
        }
        if (sb->tail_seg->next) {
            sb->tail_seg = sb->tail_seg->next;
            continue;
        }
        if (!sbuffer_segment_append_after_tail(sb)) return NULL;
    }
}
void sbuffer_remove(sbuffer* sb, sbuffer_iterator* sbi, ureg size)
{
    sbi->seg->tail = ptrsub(sbi->seg->tail, size);
    if (sbi->seg->tail != ptradd(sbi->seg, sizeof(sbuffer_segment))) {
        memmove(
            sbi->pos, ptradd(sbi->pos, size),
            ptrdiff(sbi->seg->tail, sbi->pos));
        return;
    }
    if (sbi->seg == sb->tail_seg) {
        if (sb->tail_seg->prev) sb->tail_seg = sb->tail_seg->prev;
        sbi->seg = sb->tail_seg;
        sbi->pos = sb->tail_seg->tail;
    }
    else {
        sbuffer_segment* s = sbi->seg;
        sbi->seg = s->next; // s->next must exist since s is before tail_seg
        sbi->pos = ptradd(s->next, sizeof(sbuffer_segment));
        if (s->prev) {
            s->prev->next = s->next;
        }
        else {
            sb->first_seg = s->next;
        }
        s->next->prev = s->prev; // again, s->next must exist
        s->prev = sb->tail_seg;
        s->next = sb->tail_seg->next;
        sb->tail_seg->next = s;
        if (s->next) s->next->prev = s;
    }
}
// PERF: maybe hand roll these two
void sbuffer_remove_front(sbuffer* sb, ureg size)
{
    sbuffer_iterator sbi = sbuffer_iterator_begin(sb);
    sbuffer_remove(sb, &sbi, size);
}
void sbuffer_remove_back(sbuffer* sb, ureg size)
{
    sbuffer_iterator sbi = sbuffer_iterator_begin_at_end(sb);
    sbuffer_iterator_previous(&sbi, size);
    sbuffer_remove(sb, &sbi, size);
}

void* sbuffer_insert(sbuffer* sb, sbuffer_iterator* sbi, ureg size)
{
    if (ptrdiff(sbi->seg->end, sbi->seg->tail) >= size) {
        memmove(
            ptradd(sbi->pos, size), sbi->pos,
            ptrdiff(sbi->seg->tail, sbi->pos));
        sbi->seg->tail = ptradd(sbi->seg->tail, size);
        return sbi->pos;
    }
    // TODO: this could be done a lot smarter
    // possible optimizations include leaving the insert in the current seg,
    // and checking prev and following blocks for available space
    ureg after_size = ptrdiff(sbi->pos, sbi->seg->tail);
    ureg needed_size = after_size + size;
    sbuffer_segment* s = sb->tail_seg->next;
    while (s) {
        if (ptrdiff(s->end, s + sizeof(sbuffer_segment)) >= needed_size) {
            s->prev->next = s->next; // s->prev exists since it's after sbi->seg
            if (s->next) {
                s->next->prev = s->prev;
            }
            break;
        }
        s = s->next;
    }
    if (!s) {
        sb->biggest_seg_size *= 2;
        if (sb->biggest_seg_size < needed_size) {
            sb->biggest_seg_size = ceil_to_pow2(needed_size);
        }
        s = sbuffer_segment_create(sb->biggest_seg_size);
    }
    s->prev = sbi->seg;
    s->next = sbi->seg->next;
    if (s->next) s->next->prev = s;
    sbi->seg->next = s;
    memcpy(ptradd(s, sizeof(sbuffer_segment) + size), sbi->pos, after_size);
    s->tail = ptradd(s, sizeof(sbuffer_segment) + needed_size);
    sbi->seg = s;
    sbi->pos = ptradd(s, sizeof(sbuffer_segment));
    return sbi->pos;
}

void sbuffer_clear(sbuffer* sb)
{
    sbuffer_segment* s = sb->first_seg;
    do {
        s->tail = ptradd(s, sizeof(sbuffer_segment));
        s = s->next;
    } while (s);
    sb->tail_seg = sb->first_seg;
}
