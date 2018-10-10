#include <stdio.h>
#include "utils/types.h"
#include "utils/allocator.h"
#include "utils/math_utils.h"
#include "utils/timing.h"
#include "utils/debug_utils.h"
#include "tokenizer.h"


int main(int argc, char** argv){
    thread_allocator tal;
    tokenizer tk;
    file f;
    allocator_init();
    tal_init(&tal);
    tk_init(&tk, &tal);

    string_set(&f.path, "/media/nas_mirror/projects/tau/test/test.tau");
    tk_open_file(&tk, &f);

    token* t;
    t = tk_peek(&tk);
    t = tk_peek_2nd(&tk);
    t = tk_peek_3rd(&tk);
    t = tk_peek_nth(&tk, 4);
    do{
        t = tk_peek_nth(&tk, 5);
        tk_void(&tk);
    }while(t->type != TT_EOF);

    tk_fin(&tk);
    tal_fin(&tal);
    allocator_fin();
    
   
}
