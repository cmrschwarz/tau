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
static inline int ptrlist_append_get_pos(ptrlist* p, void* data, void*** pos)
{
    void** v = (void**)sbuffer_append(p, sizeof(void*));
    if (!v) return ERR;
    *pos = v;
    *v = data;
    return OK;
}
static inline bool ptrlist_is_empty(ptrlist* p)
{
    if (p->first_seg != p->tail_seg) return false;
    void* start = ptradd(p->first_seg, sizeof(sbuffer_segment));
    return (ptrdiff(p->first_seg->tail, start) == 0);
}

// invalidates pli->res, but revalidated after pli_next/prev
static inline void ptrlist_remove_next(ptrlist* p, pli* pli)
{
    sbuffer_remove_next(p, pli, sizeof(void*));
}

static inline void ptrlist_remove_prev(ptrlist* p, pli* pli)
{
    sbuffer_remove_prev(p, pli, sizeof(void*));
}

static inline void* ptrlist_pop_back(ptrlist* p)
{
    void* res = *(void**)sbuffer_back(p, sizeof(void*));
    sbuffer_remove_back(p, sizeof(void*));
    return res;
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

static inline int ptrlist_append_copy(ptrlist* p, ptrlist* src)
{
    pli it = sbuffer_iterator_begin(p);
    for (void* i = pli_next(&it); i; i = pli_next(&it)) {
        if (ptrlist_append(p, i)) return ERR;
    }
    return OK;
}

#endif
