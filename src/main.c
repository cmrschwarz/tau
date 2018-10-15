#include <stdio.h>
#include "utils/types.h"
#include "utils/allocator.h"
#include "utils/math_utils.h"
#include "utils/timing.h"
#include "utils/debug_utils.h"
#include "tokenizer.h"
#include "tauc.h"
#define EO(arg) if(arg)return -1
int main(int argc, char** argv){
    tokenizer tk;
    thread_context tc;
    file f;
    EO(allocator_init());
    EO(thread_context_init(&tc));
    EO(tk_init(&tk, &tc));
    
    string_set(&f.path, "test/test.tau");
    EO(file_init(&f, &tc, f.path));
    EO(tk_open_file(&tk, &f));
//    EO(tk_open_stdin(&tk, &f));
    token* t;
    for (int i=1;i<5;i++){
        token_print(tk_peek_nth(&tk, i));
        putchar('\n');
        fflush(stdout);
    }
    do{
        t = tk_peek_nth(&tk, 5);
        token_print(t);
        src_pos p = src_map_get_pos(&f.src_map, t->start);
        printf("{%llu, %llu}", p.line, p.column);
        putchar('\n');
        fflush(stdout);
        tk_void(&tk);
    }while(t!= NULL && t->type != TT_EOF);
    tk_fin(&tk);
    file_fin(&f);
    thread_context_fin(&tc);
    allocator_fin();
}
