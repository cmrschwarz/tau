#pragma once
#include "utils/plattform.h"
#if DEBUG
#include <assert.h>
#else
#define assert(cond)
#endif
void panic(char* message);
