#include "pool.h"
#include "../error_log.h"
#include "allocator.h"
#include "math_utils.h"
static inline pool_segment* pool_alloc_segment(pool* p, ureg size)
{
    pool_segment* seg = (pool_segment*)tmalloc(size);
    if (!seg) return NULL;
    seg->head = (u8*)(seg + 1);
    seg->end = (u8*)ptradd(seg, size);
    return seg;
}
static inline void pool_free_segment(pool* p, pool_segment* s)
{
    tfree(s);
}
int pool_init(pool* p)
{
    pool_segment* seg = pool_alloc_segment(p, plattform_get_page_size());
    if (!seg) return ERR;
    p->head_segment = seg;
    seg->next = NULL;
    seg->prev = NULL;
    return 0;
}
void pool_fin(pool* p)
{
    pool_segment* next = p->head_segment->next;
    pool_segment* seg = p->head_segment;
    while (seg != NULL) {
        pool_segment* to_free = seg;
        seg = seg->prev;
        pool_free_segment(p, to_free);
    }
    seg = next;
    while (seg != NULL) {
        pool_segment* to_free = seg;
        seg = seg->next;
        pool_free_segment(p, to_free);
    }
}
void* pool_alloc(pool* p, ureg size)
{
    while (true) {
        if (p->head_segment->head + size <= p->head_segment->end) {
            void* res = p->head_segment->head;
            p->head_segment->head += size;
            return res;
        }
        if (!p->head_segment->next) break;
        p->head_segment = p->head_segment->next;
    }
    ureg size_new = 2 * (ptrdiff(p->head_segment->end, p->head_segment) -
                         sizeof(pool_segment));
    if (size_new < size) {
        size_new = ceil_to_pow2(size);
        if (size_new == 0) size_new = size;
    }
    pool_segment* seg = pool_alloc_segment(p, size_new);
    if (!seg) return NULL;
    p->head_segment->next = seg;
    seg->prev = p->head_segment;
    seg->next = NULL;
    p->head_segment = seg;
    void* res = seg->head;
    seg->head += size;
    return res;
}
void pool_clear(pool* p)
{
    pool_segment* seg = p->head_segment;
    while (true) {
        seg->head = ptradd(seg, sizeof(pool_segment));
        if (!seg->prev) {
            p->head_segment = seg;
            break;
        }
        seg = seg->prev;
    }
}
void pool_steal_all(pool* p, pool* donor)
{
    if (!donor->head_segment) return;
    if (!p->head_segment) {
        p->head_segment = donor->head_segment;
        donor->head_segment = NULL;
        return;
    }
    if (p->head_segment->next) {
        pool_segment* donor_last = donor->head_segment;
        while (donor_last->next) donor_last = donor_last->next;
        donor_last->next = p->head_segment->next;
        p->head_segment->next->prev = donor_last;
    }
    pool_segment* donor_first = donor->head_segment;
    while (donor_first->prev) donor_first = donor_first->prev;
    p->head_segment->next = donor_first;
    donor_first->prev = p->head_segment;
    donor->head_segment = NULL;
}
void pool_steal_used(pool* p, pool* donor)
{
    pool_segment* donor_unused = donor->head_segment->next;
    donor->head_segment->next = NULL;
    pool_steal_all(p, donor);
    donor->head_segment = donor_unused;
    if (donor_unused) donor_unused->prev = NULL;
}

void pool_undo_last_alloc(pool* p, ureg size)
{
    // it's either in the current segment or the current segment is empty
    // there could be multiple emptys because of steals
    while (p->head_segment->head ==
           ptradd(p->head_segment, sizeof(pool_segment))) {
        p->head_segment = p->head_segment->prev;
    }
    p->head_segment->head = ptrsub(p->head_segment->head, size);
}
