#include "stdlib.h"
void panic(){
    //exit with an error to indicate something went horribly wrong
    //this is only called if we have no idea what else to do, hence the name
    exit(-1);
}