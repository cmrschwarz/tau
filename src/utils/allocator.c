#include "allocator.h"
static allocator ALLOCATOR = {.threads = NULL};

int tal_init(thread_allocator* tal){
    tal->next = ALLOCATOR.threads;
    ALLOCATOR.threads = tal;
    tal->arenas = NULL;
    tal->free_list = NULL;
}
void tal_fin(thread_allocator* tal){
    thread_allocator* tas = ALLOCATOR.threads;
    while(true)
        if(tas->next == tal){
            arena* as = tal->arenas;
            while(as!=NULL){
                arena_fin(as);
            }
            tas->next = tal->next;
            
            return 0;
        }
        //this throws a segfault if the end of ALLOCATOR.threads is reached
        //before tas is found. This is intentional because a destructor
        //can't fail, so this is UB. An error in UB is better than no error
        tas = tas->next;  
    }
   
}
int arena_init(arena* a, thread_allocator* tal);
void arena_fin(arena* a, thread_allocator* tal);
void* arena_alloc(arena* a, ureg size);
void arena_clear(arena* a);