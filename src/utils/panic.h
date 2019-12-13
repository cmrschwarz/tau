#ifndef TAUC_UTILS_PANIC_H
#define TAUC_UTILS_PANIC_H

#include "utils/plattform.h"
#if DEBUG
#include <assert.h>
#else
#define assert(cond)
#endif
void panic(char* message);
void debugbreak();
#endif
