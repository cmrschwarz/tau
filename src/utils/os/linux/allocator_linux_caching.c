#include "../../plattform.h"

#include "../../c_extensions.h"
POSSIBLY_EMPTY_TL

#if OS_LINUX && 0 //disabled for now
#include "../../allocator.h"
#include "../../threading.h"
#include <linux/mman.h>
#include <sys/mman.h>
#include <sys/unistd.h> 
#include "../../math_utils.h"

static struct allocator{
    thread_allocator* threads;
    ureg segment_size;
    mutex lock;
}ALLOCATOR;

int allocator_init(){
    ALLOCATOR.threads = 0;
    if(mutex_init(&ALLOCATOR.lock)) return -1;
    ALLOCATOR.segment_size = sysconf(_SC_PAGESIZE);
    return 0;
}
void allocator_fin(){
    mutex_fin(&ALLOCATOR.lock);
}
ureg allocator_get_segment_size(){
    return ALLOCATOR.segment_size;
}

int tal_init(thread_allocator* tal){
    mutex_lock(&ALLOCATOR.lock);
    tal->next = ALLOCATOR.threads;
    ALLOCATOR.threads = tal;
    mutex_unlock(&ALLOCATOR.lock);
    return 0;
}

void tal_fin(thread_allocator* tal){
    mutex_lock(&ALLOCATOR.lock);
    thread_allocator** tals = &ALLOCATOR.threads;
    while(true){
        if(*tals == tal){
            (*tals) = tal->next;
             //TODO: free tal
            mutex_unlock(&ALLOCATOR.lock);
            return;
        }
        //might segfault, that's intended as any such case is UB
        tals = &(*tals)->next;  
    }
}

//TODO: make an efficient allocator
int tal_alloc(thread_allocator* tal, ureg size, memblock* b){
    b->start = mmap(
        NULL, size, PROT_READ | PROT_WRITE,
        MAP_PRIVATE | MAP_ANONYMOUS | MAP_UNINITIALIZED, -1, 0
    );
    if(b->start == MAP_FAILED)return -1;
    return 0;
}

int tal_realloc(thread_allocator* tal, ureg used_size, memblock* b, ureg new_size) {
    //TODO
    return 0;
}

void tal_free(thread_allocator* tal, memblock* b){
    int res = munmap(b->start, ptrdiff(b->end, b->start));
    if(res){ //UB: create a segfault. 
        b->start = *(void**)(NULL); 
    }
}

void tal_shrink();  //todo: think of a better name for this
#endif