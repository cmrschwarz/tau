#include "pool.h"
#include "../error_log.h"
#include "allocator.h"
#include "math_utils.h"
static inline pool_segment* pool_alloc_segment(pool* p, ureg size)
{
    pool_segment* seg = (pool_segment*)tmalloc(size);
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
    p->segments = seg;
    seg->next = NULL;
    return 0;
}
void pool_fin(pool* p)
{
    pool_segment* segs = p->segments;
    while (segs != NULL) {
        pool_segment* to_free = segs;
        segs = segs->next;
        pool_free_segment(p, to_free);
    }
}
void* pool_alloc(pool* p, ureg size)
{
    if (p->segments->head + size <= p->segments->end) {
        void* res = p->segments->head;
        p->segments->head += size;
        return res;
    }
    ureg size_new =
        2 * (ptrdiff(p->segments->end, p->segments) - sizeof(pool_segment));
    if (size_new < size) {
        size_new = ceil_to_pow2(size);
        if (size_new == 0) size_new = size;
    }
    pool_segment* seg = pool_alloc_segment(p, size_new);
    if (!seg) return NULL;
    seg->next = p->segments;
    p->segments = seg;
    void* res = seg->head;
    seg->head += size;
    return res;
}
void pool_clear(pool* p)
{
    pool_segment* segs = p->segments;
    while (segs->next != NULL) {
        pool_free_segment(p, segs);
        segs = segs->next;
    }
    p->segments = segs;
    segs->head = (u8*)ptradd(segs, sizeof(pool_segment));
}
