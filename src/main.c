#include <stdio.h>
#include "tokenizer.h"
int main(int argc, char** argv){
    tokenizer t;
    if(tk_init(&t)) return -1;
    tk_open_file(&t, "test/test.tau");
}
