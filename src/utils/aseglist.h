#pragma once
#include "atomic_pool.h"
#include "error_log.h"
#include "math_utils.h"
#include "threading.h"

typedef struct aseglist_node {
    struct aseglist_node* next;
} aseglist_node;

typedef struct aseglist {
    atomic_ptr head;
    atomic_ureg size;
} aseglist;

typedef struct aseglist_iterator {
    aseglist_node* node;
    void** pos;
} aseglist_iterator;

static inline void aseglist_iterator_begin(aseglist_iterator* it, aseglist* l)
{
}
static inline void* aseglist_iterator_next(aseglist_iterator* it)
{
    return 0;
}
static inline void aseglist_iterator_fin(aseglist_iterator* it)
{
}

static inline int aseglist_node_init(aseglist_node* n, ureg size)
{
    return 0;
}
static inline void aseglist_node_fin(aseglist_node* n)
{
}

static inline int aseglist_init(aseglist* l)
{
    return 0;
}

static inline void aseglist_fin(aseglist* l)
{
}

static inline int aseglist_add(aseglist* l, atomic_pool* pool, void* val)
{
    return 0;
}