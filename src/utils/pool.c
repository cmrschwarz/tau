#include "pool.h"
#include "math_utils.h"
#include "allocator.h"

static inline pool_segment* arena_alloc_segment(pool* p, ureg size){
    memblock b;
    if(!tal_alloc(p->tal, size, &b)) return NULL;
    pool_segment* seg = b.start;
    seg->head = ptradd(seg, sizeof(pool_segment));
    seg->end = b.end;
    return seg;
}
static inline void pool_free_segment(pool* p, pool_segment* s){
    memblock b;
    b.start = (void*)s;
    b.end = s->end;
    tal_free(p->tal, &b);
}
int arena_init(pool* p, thread_allocator* tal){
    p->tal = tal;
    pool_segment* seg = arena_alloc_segment(p, PAGE_SIZE);
    if(!seg)return -1;
    p->segments = seg;
    seg->next = NULL;
    return 0;
}
void arena_fin(pool* p){
    pool_segment* segs = p->segments;
    while(segs != NULL){
        pool_segment* to_free = segs;
        segs = segs->next;
        pool_free_segment(p, to_free);
    }
}
void* pool_alloc(pool* p, ureg size){
    if(p->segments->head + size <= p->segments->end){
        void* res = p->segments->head;
        p->segments->head += size;
        return res;
    }
    else{
        ureg size_new = 
            2 * (ptrdiff(p->segments->end, p->segments) - sizeof(pool_segment));
        if(size_new < size){
            size_new = ceil_to_pow2(size);
            if(size_new == 0) size_new = size; 
        }
        pool_segment* seg = arena_alloc_segment(p, size_new);
        if(!seg)return NULL;
        seg->next = p->segments;
        p->segments = seg; 
        void* res = seg->head;
        seg->head += size;
        return res;
    }
}
void pool_clear(pool* p){
    pool_segment* segs = p->segments;
    while(segs->next != NULL){
        pool_free_segment(p, segs);
        segs = segs->next;
    }
    p->segments = segs;
    segs->head = ptradd(segs, sizeof(pool_segment));
}