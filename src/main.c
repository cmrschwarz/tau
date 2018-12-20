#include <stdio.h>
#include "utils/types.h"
#include "utils/allocator.h"
#include "utils/math_utils.h"
#include "utils/timing.h"
#include "utils/debug_utils.h"
#include "tokenizer.h"
#include "tauc.h"

int puts_or_mangle_error(int r, char* text){
    if(puts(text) == EOF) return  0xDEAD0000;
    return r;
}
int main(int argc, char** argv){
    //error code storage
    int r;

    //init master error log
    r = master_error_log_init();
    if(r){
        return puts_or_mangle_error(
            r,
            "Fatal Error: Failed to aquire synchronization primitives from OS"
        );
    }

    //init root allocator
    r = allocator_init();
    if(r){
        master_error_log_fin();
        return puts_or_mangle_error(
            r,
            "Fatal Error: Failed to initialize memory allocation on the OS"
        );
    }

    //main programm
    tauc tauc;
    r = tauc_init(&tauc, argc, argv);
    if(!r) tauc_fin(&tauc);
    
    //report any erros that occured
    r = master_error_log_unwind(r);

    //terminate gracefully
    allocator_fin();
    master_error_log_fin();
    return r;
}
