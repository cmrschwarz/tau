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
    void** head;
    void** end;
    list_node* next_node;
} list_it;

ureg list_length(list* l);
int list_append_node(list* l, pool* alloc_pool, void* data);
void list_remove(list* l, list_it* it);
void list_remove_swap(list* l, list_it* it);

static inline void list_init(list* l)
{
    l->first_node = NULL;
    l->head_node = (list_node*)((ureg)(NULL_PTR_PTR) | LIST_SSO_CAPACITY);
}

static inline void list_fin(list* l)
{
}

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

static inline void list_it_init(list_it* it, list* l)
{
    ureg sso_val = ((ureg)l->head_node) & LIST_SSO_MASK;
    it->head = &l->sso_slots[0];
    it->end = &l->sso_slots[0] + (LIST_SSO_CAPACITY - sso_val);
    it->next_node = l->first_node;
}

static inline void* list_it_next(list_it* it)
{
    if (it->head == it->end) {
        if (it->next_node == NULL) return NULL;
        it->head = ptradd(it->next_node, sizeof(list_node));
        it->end = it->next_node->head;
        if (it->head == it->end) return NULL;
        it->next_node = it->next_node->next;
    }
    void* res = *it->head;
    it->head++;
    return res;
}

static inline void* list_it_start(list_it* it, list* l)
{
    list_it_init(it, l);
    return list_it_next(it);
}

static inline void list_clear(list* l)
{
    if (l->first_node) {
        l->first_node->head = ptradd(l->first_node, sizeof(list_node));
        l->head_node = (list_node*)(((ureg)l->first_node) | LIST_SSO_CAPACITY);
    }
    else {
        l->head_node = (list_node*)(((ureg)NULL_PTR_PTR) | LIST_SSO_CAPACITY);
    }
}

static inline void* list_pop_back(list* l)
{
    ureg sso_val = ((ureg)l->head_node) & LIST_SSO_MASK;
    if (sso_val) {
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
