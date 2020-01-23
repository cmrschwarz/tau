#include "list.h"
#include "panic.h"

int list_append_node(list* l, pool* alloc_pool, void* data)
{
    list_node* n = l->head_node->next;
    if (!n) {
        list_node** prev_next;
        ureg size;
        if (l->head_node == (list_node*)NULL_PTR_PTR) {
            assert(LIST_NODE_MIN_SIZE * sizeof(void*) > sizeof(list_node));
            size = LIST_NODE_MIN_SIZE;
            prev_next = &l->first_node;
        }
        else {
            size = 2 * ptrdiff(l->head_node->end, l->head_node);
            prev_next = &l->head_node->next;
        }
        n = pool_alloc(alloc_pool, size);
        if (!n) return ERR;
        n->end = ptradd(n, size);
        n->next = NULL;
        *prev_next = n;
        n->prev = l->head_node;
    }
    l->head_node = n;
    n->head = ptradd(n, sizeof(list_node));
    *n->head = data;
    n->head++;
    return OK;
}

ureg list_length(list* l)
{
    ureg sso_val = ((ureg)l->head_node) & LIST_SSO_MASK;
    if (sso_val) return LIST_SSO_CAPACITY - sso_val;
    if (l->head_node == (list_node*)NULL_PTR_PTR) return LIST_SSO_CAPACITY;
    ureg head_plus_sso_len =
        ptrdiff(l->head_node->head, ptradd(l->head_node, sizeof(list_node))) /
            sizeof(void*) +
        LIST_SSO_CAPACITY;
    ureg head_size = ptrdiff(l->head_node->end, l->head_node);
    const ureg initial_node_size = LIST_NODE_MIN_SIZE;
    const ureg initial_node_bits = ulog2(initial_node_size);
    ureg prev_nodes_count = ulog2(head_size) - initial_node_bits;
    if (!prev_nodes_count) return head_plus_sso_len;
    return head_plus_sso_len +
           (head_size - prev_nodes_count * sizeof(list_node) -
            LIST_NODE_MIN_SIZE) /
               sizeof(void*);
}

void list_remove_swap(list* l, list_it* it)
{
    ureg sso_val = ((ureg)l->head_node) & LIST_SSO_MASK;
    if (sso_val || l->head_node == (list_node*)NULL_PTR_PTR) {
        assert(sso_val < LIST_SSO_CAPACITY);
        sso_val++;
        l->head_node =
            (list_node*)((((ureg)l->head_node) & ~LIST_SSO_MASK) | sso_val);
        it->head--;
        it->end--;
        *it->head = l->sso_slots[LIST_SSO_CAPACITY - sso_val];
        return;
    }
    if (it->end == l->head_node->head) {
        it->end--;
    }
    l->head_node->head--;
    if (l->head_node->head == ptradd(l->head_node, sizeof(list_node))) {
        void* val = *l->head_node->head;
        if (it->end == l->head_node->head) {
            it->next_node = l->head_node;
            l->head_node = l->head_node->prev;
            if (l->head_node->prev != (list_node*)NULL_PTR_PTR) {
                it->end = l->head_node->head;
            }
            else {
                it->end = &l->sso_slots[0] + LIST_SSO_CAPACITY;
            }
            it->head = it->end - 1;
            *it->head = val;
            return;
        }
        it->head--;
        *it->head = val;
        l->head_node = l->head_node->prev;
        return;
    }
    it->head--;
    *it->head = *l->head_node->head;
    return;
}

void list_remove(list* l, list_it* it)
{
    panic("not implemented");
}
