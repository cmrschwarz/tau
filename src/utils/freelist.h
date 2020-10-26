#pragma once

#include "pool.h"
#if DEBUG
#include <assert.h>
#endif

typedef struct freelist_node_s {
    struct freelist_node_s* next;
} freelist_node;

typedef struct freelist_s {
    freelist_node* free_nodes;
    ureg node_size;
    pool* p;
#if DEBUG
    sreg alloc_count;
#endif
} freelist;

static inline int freelist_init(freelist* f, pool* p, ureg node_size)
{
    f->free_nodes = NULL;
    f->node_size = node_size;
    f->p = p;
#if DEBUG
    f->alloc_count = 0;
#endif
    return 0;
}
static inline void* freelist_alloc(freelist* f)
{
    void* res;
    if (f->free_nodes) {
        res = f->free_nodes;
        f->free_nodes = f->free_nodes->next;
    }
    else {
        // if this returns NULL, we propagate that
        // this guy takes care of alignment for us
        res = pool_alloc(f->p, f->node_size);
    }
#if DEBUG
    f->alloc_count++;
#endif
    return res;
}
static inline void freelist_free(freelist* f, void* n)
{
    ((freelist_node*)n)->next = f->free_nodes;
    f->free_nodes = (freelist_node*)n;
#if DEBUG
    f->alloc_count--;
#endif
}
static inline void freelist_fin(freelist* f)
{
#if DEBUG
    assert(f->alloc_count == 0);
#endif
}
// release "ownership" of all nodes, useful after the pool was reset
static inline void freelist_clear(freelist* f)
{
    f->free_nodes = NULL;
#if DEBUG
    f->alloc_count = 0;
#endif
}
