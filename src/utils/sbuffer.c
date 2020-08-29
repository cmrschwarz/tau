#include "sbuffer.h"
#include "allocator.h"
#include "math_utils.h"
#include "error.h"
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
    if (!s) return ERR;
    s->next = NULL;
    s->prev = NULL;
    sb->first_seg = s;
    sb->tail_seg = s;
    return OK;
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
    while (true) {
        ureg tss = ptrdiff(sb->tail_seg->tail, sb->tail_seg + 1);
        assert(tss == 0 || tss >= size);
        if (tss) break;
        sb->tail_seg = sb->tail_seg->prev;
        assert(sb->tail_seg);
    }
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
void sbuffer_remove_next(sbuffer* sb, sbuffer_iterator* sbi, ureg size)
{
    assert(ptrdiff(sbi->seg->tail, sbi->seg) >= sizeof(sbuffer_segment) + size);
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
void sbuffer_remove_prev(sbuffer* sb, sbuffer_iterator* sbi, ureg size)
{
    sbuffer_iterator_previous(sbi, size);
    sbuffer_remove_next(sb, sbi, size);
}
// PERF: maybe hand roll these two
void sbuffer_remove_front(sbuffer* sb, ureg size)
{
    sbuffer_iterator sbi = sbuffer_iterator_begin(sb);
    sbuffer_remove_next(sb, &sbi, size);
}
void sbuffer_remove_back(sbuffer* sb, ureg size)
{
    sbuffer_iterator sbi = sbuffer_iterator_begin_at_end(sb);
    sbuffer_iterator_previous(&sbi, size);
    sbuffer_remove_next(sb, &sbi, size);
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
void sbuffer_normalize_tail_seg(sbuffer* sb)
{
    while (sb->tail_seg->prev &&
           sb->tail_seg->tail ==
               ptradd(sb->tail_seg, sizeof(sbuffer_segment))) {
        sb->tail_seg = sb->tail_seg->prev;
    }
}
void sbuffer_take_and_invalidate(sbuffer* sb, sbuffer* donor)
{
    sbuffer_segment* donor_rem = donor->tail_seg->next;
    donor->tail_seg->next = sb->first_seg;
    sb->first_seg->prev = donor->tail_seg;
    sb->first_seg = donor->first_seg;
    if (donor_rem) {
        donor_rem->prev = sb->tail_seg;
        if (sb->tail_seg->next) {
            sbuffer_segment* donor_end = donor_rem;
            while (donor_end->next) donor_end = donor_end->next;
            donor_end->next = sb->tail_seg->next;
            donor_end->next->prev = donor_rem;
        }
        sb->tail_seg->next = donor_rem;
    }
    sbuffer_normalize_tail_seg(sb);
}
int sbuffer_steal_used(sbuffer* sb, sbuffer* donor, bool sb_initialized)
{
    sbuffer_segment* donor_rem = donor->tail_seg->next;
    sbuffer_segment* donor_new = NULL;
    if (!donor_rem) {
        // sbuffer must always have at least one segment
        // we alloc this first so we don't have to undo on failiure
        donor->biggest_seg_size *= 2;
        donor_new = sbuffer_segment_create(donor->biggest_seg_size);
        if (!donor_new) return ERR;
        donor_new->prev = NULL;
        donor_new->next = NULL;
    }
    if (sb_initialized) {
        sb->first_seg->prev = donor->tail_seg;
    }
    else {
        sb->biggest_seg_size = ptrdiff(donor->tail_seg->end, donor->tail_seg);
        sb->first_seg = NULL;
        sb->tail_seg = donor->tail_seg;
    }
    donor->tail_seg->next = sb->first_seg;
    sb->first_seg = donor->first_seg;
    sbuffer_normalize_tail_seg(sb);
    if (donor_rem) {
        donor_rem->prev = NULL;
        donor->first_seg = donor_rem;
        donor->tail_seg = donor_rem;
        return OK;
    }
    donor->first_seg = donor_new;
    donor->tail_seg = donor_new;
    return OK;
}

void sbuffer_memcpy(void* target, sbuffer_iterator src, ureg size)
{
    while (true) {
        ureg s = ptrdiff(src.seg->tail, src.pos);
        if (s >= size) {
            memcpy(target, src.pos, size);
            return;
        }
        memcpy(target, src.pos, s);
        target = ptradd(target, s);
        src.seg = src.seg->next;
        assert(src.seg);
        size -= s;
        src.pos = ptradd(src.seg, sizeof(sbuffer_segment));
    }
}
