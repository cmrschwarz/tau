#include "../../allocator.h"
#ifdef USE_LIBC_ALLOCATOR
 
#include <memory.h> 
#include "../../math_utils.h"

//this is a really basic portable allocator using libc
//this is used as a fallback if no os specific one has been written yet
//to get rid of the libc dependency

static struct allocator{
    ureg segment_size;
}ALLOCATOR;

int allocator_init(){
    ALLOCATOR.segment_size = 4096;
    return 0;
}

//TODO(cmrs): debugging feature counting allocations
//that complains here if we had fewer or more deallocations --> leaks
void allocator_fin(){}

ureg allocator_get_segment_size(){
    return ALLOCATOR.segment_size;
}

int tal_init(thread_allocator* tal){
    return 0;
}

void tal_fin(thread_allocator* tal){}


int tal_alloc(thread_allocator* tal, ureg size, memblock* b){
    b->start = malloc(size);
    if(b->start == NULL)return -1;
    b->end = ptradd(b->start, size);
    return 0;
}

int tal_realloc(thread_allocator* tal, ureg used_size, ureg new_size, memblock* b) {
    void* new = realloc(b->start, new_size);
    if(!new)return -1;
    b->start = new;
    b->end = ptradd(new, new_size);
    return 0;
}

void tal_free(thread_allocator* tal, memblock* b){
    free(b->start);
}

#endif