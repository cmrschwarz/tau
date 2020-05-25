#pragma once
#include "pool.h"
#include "error.h"
#include "zero.h"
#include "math_utils.h"
#include <assert.h>

// a small size optimized linked list for storing void* 's

// we use the lower LIST_SSO_BITS of list->head_node
// to store (LIST_SSO_CAPACITY - used sso slots)
// this way when all sso slots are used the head pointer is "normal"
#define LIST_SSO_BITS 2
#define LIST_SSO_CAPACITY 2 // must be smaller than 2^LIST_SSO_BITS
#define LIST_SSO_MASK ((1 << LIST_SSO_BITS) - 1)

// min capacity for the initial allocated node
#define LIST_NODE_MIN_SIZE (8 * sizeof(void*))
#define LIST_NODE_MIN_CAPACITY (LIST_NODE_MIN_SIZE - sizeof(list_node))

typedef struct list_node_s {
    void** head;
    void** end;
    struct list_node_s* prev; // TODO: maybe store xor of prev and next?
    struct list_node_s* next;
} list_node;

typedef struct list_s {
    list_node* first_node;
    list_node* head_node; // lower bits special, see above
    void* sso_slots[LIST_SSO_CAPACITY]; // slots for small size optimization
} list;

typedef struct list_it_s {
    list_node* curr_node;
    void** head;
} list_it;

typedef struct list_rit_s {
    list_node* prev_node;
    void** node_begin;
    void** head;
} list_rit;

ureg list_length(list* l);
int list_append_node(list* l, pool* alloc_pool, void* data);
void list_remove(list* l, list_it* it);
void list_remove_swap(list* l, list_it* it);

static inline int list_init(list* l)
{
    l->first_node = NULL;
    l->head_node = (list_node*)((ureg)(NULL_PTR_PTR) | LIST_SSO_CAPACITY);
    return OK;
}

void list_fin(list* l, bool tfree);

static inline int list_append(list* l, pool* alloc_pool, void* data)
{
    ureg sso_val = ((ureg)l->head_node) & LIST_SSO_MASK;
    if (sso_val) {
        l->sso_slots[LIST_SSO_CAPACITY - sso_val] = data;
        sso_val--;
        l->head_node =
            (list_node*)((((ureg)l->head_node) & ~LIST_SSO_MASK) | sso_val);
        return OK;
    }
    if (l->head_node->head == l->head_node->end) {
        return list_append_node(l, alloc_pool, data);
    }
    *l->head_node->head = data;
    l->head_node->head++;
    return OK;
}

static inline void list_it_begin(list_it* it, list* l)
{
    it->head = &l->sso_slots[0];
    it->curr_node = NULL;
}

static inline void* list_it_next(list_it* it, list* l)
{
    if (!it->curr_node) {
        ureg sso_val = ((ureg)l->head_node) & LIST_SSO_MASK;
        void** sso_end = &l->sso_slots[0] + (LIST_SSO_CAPACITY - sso_val);
        if (it->head == sso_end) {
            if (!l->first_node) return NULL;
            it->curr_node = l->first_node;
            it->head = (void**)ptradd(l->first_node, sizeof(list_node));
        }
    }
    while (it->head == it->curr_node->head) {
        if (!it->curr_node->next) return NULL;
        if (it->head != it->curr_node->end) return NULL;
        it->curr_node = it->curr_node->next;
        it->head = (void**)ptradd(it->curr_node, sizeof(list_node));
    }
    void* res = *it->head;
    it->head++;
    return res;
}

static inline void list_rit_begin_at_end(list_rit* rit, list* l)
{
    ureg sso_val = ((ureg)l->head_node) & LIST_SSO_MASK;
    if (sso_val) {
        rit->prev_node = NULL;
        rit->head = &l->sso_slots[LIST_SSO_CAPACITY - sso_val];
        rit->node_begin = &l->sso_slots[0];
        return;
    }
    rit->prev_node = l->head_node->prev;
    rit->head = l->head_node->head;
    rit->node_begin = (void**)ptradd(l->head_node, sizeof(list_node));
    return OK;
}

static inline void* list_rit_prev(list_rit* rit)
{
    if (rit->head == rit->node_begin) {
        if (!rit->prev_node) return NULL;
        rit->head = rit->prev_node->head;
        rit->node_begin = (void**)ptradd(rit->prev_node, sizeof(list_node));
        rit->prev_node = rit->prev_node->prev;
        return list_rit_prev(rit);
    }
    rit->head--;
    return *rit->head;
}

static inline void list_clear(list* l)
{
    if (l->first_node) {
        // we need to fix the head of the first node since otherwise
        // the logic breaks after we filled the sso slots
        l->first_node->head = (void**)ptradd(l->first_node, sizeof(list_node));
        l->head_node = (list_node*)(((ureg)l->first_node) | LIST_SSO_CAPACITY);
    }
    else {
        l->head_node = (list_node*)(((ureg)NULL_PTR_PTR) | LIST_SSO_CAPACITY);
    }
}

static inline void* list_pop_back(list* l)
{
    ureg sso_val = ((ureg)l->head_node) & LIST_SSO_MASK;
    if (sso_val || l->head_node == (list_node*)NULL_PTR_PTR) {
        assert(sso_val < LIST_SSO_CAPACITY);
        sso_val++;
        l->head_node =
            (list_node*)((((ureg)l->head_node) & ~LIST_SSO_MASK) | sso_val);
        return l->sso_slots[LIST_SSO_CAPACITY - sso_val];
    }
    if (l->head_node->head == ptradd(l->head_node, sizeof(list_node))) {
        if (!l->head_node->prev) {
            l->head_node =
                (list_node*)((((ureg)l->head_node) & ~LIST_SSO_MASK) | 1);
            return l->sso_slots[LIST_SSO_CAPACITY - 1];
        }
        l->head_node = l->head_node->prev;
        return list_pop_back(l);
    }
    l->head_node->head--;
    return *l->head_node->head;
}

static inline bool list_empty(list* l)
{
    return (((ureg)l->head_node) & LIST_SSO_MASK) == LIST_SSO_CAPACITY;
}
