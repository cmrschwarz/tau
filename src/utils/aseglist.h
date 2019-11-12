#ifndef TAUC_UTILS_ASEGLIST_H
#define TAUC_UTILS_ASEGLIST_H

#include "atomic_pool.h"
#include "utils/error.h"
#include "math_utils.h"
#include "threading.h"

typedef atomic_ptr aseglist;

typedef struct aseglist_node_s {
    struct aseglist_node_s* prev;
    atomic_sreg space;
} aseglist_node;

#define ASEGLIST_CAPACITY_CONSUMPTION                                          \
    ((sizeof(aseglist_node) + sizeof(void*) - 1) / sizeof(void*))
#define ASEGLIST_ELEM_OFFSET (ASEGLIST_CAPACITY_CONSUMPTION * sizeof(void*))
// must be larger  than ASEGLIST_ELEM_OFFSET
#define ASEGLIST_INITIAL_SIZE (8 * sizeof(void*))

typedef struct aseglist_iterator_s {
    void** pos;
    void** end;
    aseglist_node* node;
} aseglist_iterator;

static inline void aseglist_iterator_begin(aseglist_iterator* it, aseglist* l)
{
    aseglist_node* n;
    do {
        n = (aseglist_node*)atomic_ptr_load(l);
    } while (!n);
    it->pos = (void**)ptradd(
        n, ASEGLIST_ELEM_OFFSET + atomic_sreg_load(&n->space) * sizeof(void*));
    ureg size = *(ureg*)ptradd(n, ASEGLIST_ELEM_OFFSET);
    it->end = (void**)ptradd(n, size);
    it->node = n;
}

static inline ureg aseglist_iterator_get_remaining_count(aseglist_iterator* it)
{
    ureg curr_node_size = ptrdiff(it->end, it->node);
    ureg node_count = ulog2(curr_node_size) - ulog2(ASEGLIST_INITIAL_SIZE / 2);
    ureg tot_size = 2 * curr_node_size - ASEGLIST_INITIAL_SIZE;
    ureg curr_node_unused =
        curr_node_size - ASEGLIST_ELEM_OFFSET - ptrdiff(it->end, it->pos);
    return (tot_size - curr_node_unused) / sizeof(void*) -
           (node_count * ASEGLIST_CAPACITY_CONSUMPTION);
}
static inline void* aseglist_iterator_next(aseglist_iterator* it)
{
    if (it->pos == it->end) {
        aseglist_node* prev = it->node->prev;
        if (prev == NULL) return NULL;
        ureg size_old = ptrdiff(it->end, it->node) / 2;
        it->pos = (void**)ptradd(prev, sizeof(aseglist_node));
        it->end = (void**)ptradd(prev, size_old);
        it->node = prev;
    };
    return *it->pos++;
}
static inline aseglist_node* aseglist_node_new(ureg size)
{
    aseglist_node* n = (aseglist_node*)tmalloc(size);
    if (!n) return NULL;
    int r = atomic_sreg_init(
        &n->space, (size - sizeof(aseglist_node)) / sizeof(void*));
    if (r) {
        tfree(n);
        return NULL;
    }
    *(ureg*)ptradd(n, ASEGLIST_ELEM_OFFSET) = size;
    return n;
}
static inline void aseglist_node_free(aseglist_node* n)
{
    atomic_sreg_fin(&n->space);
    tfree(n);
}

static inline int aseglist_init(aseglist* l)
{
    aseglist_node* n = aseglist_node_new(ASEGLIST_INITIAL_SIZE);
    if (!n) return ERR;
    int r = atomic_ptr_init(l, n);
    if (r) {
        aseglist_node_free(n);
        return r;
    }
    n->prev = NULL;
    return OK;
}

static inline void aseglist_fin(aseglist* l)
{

    aseglist_node* n = (aseglist_node*)atomic_ptr_load(l);
    aseglist_node* ntemp;
    do {
        ntemp = n;
        n = n->prev;
        aseglist_node_free(ntemp);
    } while (n != NULL);
}

static inline int aseglist_add(aseglist* l, void* data)
{
    while (true) {
        aseglist_node* node = (aseglist_node*)atomic_ptr_load(l);
        if (!node) continue;
        sreg space = atomic_sreg_dec(&node->space) - 1;
        if (space > 0) {
            ((void**)ptradd(node, ASEGLIST_ELEM_OFFSET))[space] = data;
            return OK;
        }
        if (space == 0) {
            atomic_ptr_store(l, NULL);
            ureg size_new = (*(ureg*)ptradd(node, ASEGLIST_ELEM_OFFSET)) * 2;
            aseglist_node* node_new = aseglist_node_new(size_new);
            if (!node_new) {
                atomic_ptr_store(l, node);
                return ERR;
            }
            atomic_ptr_store(l, node_new);
            *(void**)ptradd(node, ASEGLIST_ELEM_OFFSET) = data;
            node_new->prev = node;
            return OK;
        }
    }
}

#endif
