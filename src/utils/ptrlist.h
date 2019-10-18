// a small convenience wrapper around sbuffer just for pointers
#ifndef TAUC_UTILS_PTRLIST_H
#define TAUC_UTILS_PTRLIST_H

#include "sbuffer.h"
#include "error.h"
typedef struct sbuffer_s ptrlist;
typedef sbuffer_iterator pli;

static inline int ptrlist_init(ptrlist* p, ureg initial_element_capacity)
{
    return sbuffer_init(p, initial_element_capacity * sizeof(void*));
}

static inline void ptrlist_fin(ptrlist* p)
{
    sbuffer_fin(p);
}

static inline int ptrlist_append(ptrlist* p, void* data)
{
    void** v = (void**)sbuffer_append(p, sizeof(void*));
    if (!v) return ERR;
    *v = data;
    return OK;
}

// invalidates pli->res, but revalidated after pli_next/prev
static inline void ptrlist_remove(ptrlist* p, pli* pli)
{
    sbuffer_remove(p, pli, sizeof(void*));
}

static inline void ptrlist_clear(ptrlist* p)
{
    sbuffer_clear(p);
}

static inline void* pli_next(pli* p)
{
    void** res = (void**)sbuffer_iterator_next(p, sizeof(void*));
    if (!res) return NULL;
    return *res;
}

static inline void* pli_prev(pli* p)
{
    void** res = (void**)sbuffer_iterator_previous(p, sizeof(void*));
    if (!res) return NULL;
    return *res;
}

static inline pli pli_begin(ptrlist* p)
{
    return sbuffer_iterator_begin(p);
}

static inline pli pli_rbegin(ptrlist* p)
{
    return sbuffer_iterator_begin_at_end(p);
}

#endif
