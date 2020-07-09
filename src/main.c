#include "parser.h"
#include "tauc.h"
#include "lexer.h"
#include "utils/allocator.h"
#include "utils/debug_utils.h"
#include "utils/math_utils.h"
#include "utils/plattform.h"
#include "utils/timing.h"
#include "utils/types.h"
#include <stdio.h>

int main(int argc, char** argv)
{
    // limit_mem(1024 * 1024 * 220);
    int r = tauc_run(argc, argv);
    return r ? EXIT_FAILURE : EXIT_SUCCESS;
}
