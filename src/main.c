#include <stdio.h>
#include "utils/types.h"
#include "utils/allocator.h"
#include "utils/math_utils.h"
#include "utils/timing.h"
#include "utils/debug_utils.h"


int main(int argc, char** argv){
  
    allocator_init();
    thread_allocator tal;
    tal_init(&tal);
    memblock b;

    const ureg iterations = 50;
    const ureg count = 100000;
    void* store[count];
    const ureg size[] = {4096, 4096, 8192, 8192, 4096, 8192, 16384, 65536};
    ureg size_stride = 8;
  
    TIME("malloc",
        for(int j=0;j<iterations;j++){
            for(ureg i=0;i<count;i++){
                store[i] = malloc(size[i % size_stride]);
                if(store[i] == NULL){
                    printf("ALLOCATION FAILIURE: %llu\n", i);
                    exit(-1);
                }
            }
            for(ureg i=count-1;i!=UREG_MAX;i--){
                free(store[i]);
            }
        }
    );
   
     TIME("tal",
        for(int j=0;j<iterations;j++){
            for(ureg i=0;i<count;i++){
                if(tal_alloc(&tal, size[i % size_stride], &b)){
                    printf("ALLOCATION FAILIURE: %llu\n", i);
                    exit(-1);
                }
                store[i] = b.start;
            }
            for(ureg i=count-1;i!=UREG_MAX;i--){
                b.start = store[i];
                b.end = ptradd(b.start, size[i % size_stride]); 
                tal_free(&tal, &b);
            }
        }
    );
    tal_fin(&tal);
    allocator_fin();
    
   
}
