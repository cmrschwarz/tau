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

#if DEBUG
#include "../test/main_test.h"
#endif

int main_release(int argc, char** argv)
{
    int r;
    r = talloc_init();
    if (!r) {
        r = tauc_run(argc, argv);
        talloc_fin();
    }
    return r ? EXIT_FAILURE : EXIT_SUCCESS;
}

int main(int argc, char** argv)
{
#if DEBUG
    return main_test(argc, argv);
#else
    return main_release(argc, argv);
#endif
}
