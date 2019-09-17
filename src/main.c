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
    if (r) return EXIT_FAILURE;

    r = master_error_log_init();
    if (r) {
        talloc_fin();
        return EXIT_FAILURE;
    }

    // main programm
    r = tauc_init();
    if (!r) {
        tauc_run(argc, argv);
        // report any erros that occured
        master_error_log_unwind();
        r = tauc_fin(); // UGLY: most of tauc isn't required for error reporting
    }
    else {
        master_error_log_unwind();
    }

    // terminate gracefully
    master_error_log_fin();
    talloc_fin();
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
