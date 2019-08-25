#ifndef TAUC_UTILS_FREELIST_H
#define TAUC_UTILS_FREELIST_H

#include "pool.h"

typedef struct freelist_node_s {
    struct freelist_node* next;
} freelist_node;

typedef struct freelist_s {
    freelist_node* free_nodes;
    ureg node_size;
    pool* p;
} freelist;

static inline int freelist_init(freelist* f, pool* p, ureg node_size)
{
    f->free_nodes = NULL;
    f->node_size = node_size;
    f->p = p;
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
        res = pool_alloc(f->p, f->node_size);
    }
    return res;
}
static inline void freelist_free(freelist* f, void* n)
{
    ((freelist_node*)n)->next = f->free_nodes;
    f->free_nodes = n;
}
static inline void freelist_fin(freelist* p)
{
}
#endif