#include "stdlib.h"
#include "stdio.h"
#include "plattform.h"
void panic(char* message){
    //exit with an error to indicate something went horribly wrong
    //this is only called if we have no idea what else to do, hence the name
    fputs("fatal error: ", stderr);
    fputs(message, stderr);
    fputc('\n', stderr);
    fflush(stderr);
    exit(-1);
}