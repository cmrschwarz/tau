#include "../../plattform.h"
#if OS_LINUX
#include "../../allocator.h"
#include <linux/mman.h> 
#include <sys/mman.h> 
#include <sys/unistd.h> 
#include <memory.h> 
#include "../../math_utils.h"
#include <signal.h>
//this is a really basic allocator using mmap
//this will eventually be replaced by allocator_linux_caching
//but for now, it's good enough

static struct allocator{
    ureg segment_size;
}ALLOCATOR;

int allocator_init(){
    ALLOCATOR.segment_size = sysconf(_SC_PAGESIZE);
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
    b->start = mmap(
        NULL, size, PROT_READ | PROT_WRITE,
        MAP_PRIVATE | MAP_ANONYMOUS | MAP_UNINITIALIZED, -1, 0
    );
    if(b->start == MAP_FAILED)return -1;
    b->end = ptradd(b->start, size);
    return 0;
}
int tal_allocz(thread_allocator* tal, ureg size, memblock* b){
    //removing the MAP_UNITIALIZED flag guarantees that the memory will be cleared
    b->start = mmap(
        NULL, size, PROT_READ | PROT_WRITE,
        MAP_PRIVATE | MAP_ANONYMOUS, -1, 0
    );
    if(b->start == MAP_FAILED)return -1;
    b->end = ptradd(b->start, size);
    return 0;
}

int tal_realloc(thread_allocator* tal, ureg used_size, ureg new_size, memblock* b) {
    memblock new;
    if(tal_alloc(tal, new_size, &new)) return -1;
    memcpy(new.start, b->start, used_size);
    tal_free(tal, b);
    *b = new;
    return 0;
}

void tal_free(thread_allocator* tal, memblock* b){
    int res = munmap(b->start, ptrdiff(b->end, b->start));
    //a failing free is UB --> trigger a segfault
    if(res)raise(SIGSEGV);
}

#endif