#include "list_builder.h"
#include "math_utils.h"
#include "memory.h"
#include "error_log.h"

int list_builder_init(list_builder* b, pool* memsrc, ureg initial_capacity){
    b->memsrc = memsrc;
    ureg size = sizeof(list_build_segment) + initial_capacity * sizeof(void*);
    list_build_segment* first = pool_alloc(memsrc, size); 
    if(!first) return ERR;
    first->prev = NULL;
    first->next = NULL;
    first->end = ptradd(first, size);
    b->head_segment = first;
    b->head = ptradd(first, sizeof(list_build_segment));
    return OK;
}
void** list_builder_start(list_builder* b){
    return b->head;
}
int list_builder_add(list_builder* b, void* el){
    if(b->head < b->head_segment->end){
        *b->head = el;
        b->head++;
    }
    else{
        list_build_segment* next = b->head_segment->next;
        if(next == NULL){
            ureg size = ptrdiff(b->head_segment->end, b->head_segment) * 2;
            next = pool_alloc(b->memsrc, size);
            if(!next) return ERR; 
            next->end = ptradd(next, size);
            next->next = NULL;
            next->prev = b->head_segment;
            b->head_segment->next = next;
        }
        b->head_segment = next;
        b->head = ptradd(next, sizeof(list_build_segment));
        *b->head = el;
        b->head++;
    }
    return OK;
}
int list_builder_pop_list(
    list_builder* b, void** list_start, void*** tgt, pool* tgtmem
){
    ureg size = 0;
    list_build_segment* s = b->head_segment;
    if(s <= list_start && s->end > list_start){
        size = ptrdiff(b->head, list_start);
        *tgt = pool_alloc(tgtmem, size);
        if(!*tgt)return ERR;
        memcpy(*tgt, list_start, size);
        b->head = list_start;
        return OK;
    }
    size = ptrdiff(b->head, b->head_segment) - sizeof(list_build_segment);
    do{
        s = s->prev;
        size += ptrdiff(s->end, s) - sizeof(list_build_segment);       
    } while(list_start <= s || list_start >= s->end);
    *tgt = (void**)pool_alloc(tgtmem, size);
    if(!*tgt)return ERR;
    void** h = *tgt;
    size = ptrdiff(s->end, list_start);
    memcpy(h, list_start, size);
    list_build_segment* head_old = b->head_segment;
    b->head_segment = s;
    while(s != head_old->next){
        h += size;
        void** start = ptradd(s, sizeof(list_build_segment));
        size = ptrdiff(s->end, start);
        memcpy(h, start, size); 
        s = s->next;
    }
    b->head = list_start;
}