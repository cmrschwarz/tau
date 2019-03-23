#include "parser.h"
#include "tauc.h"
#include "tokenizer.h"
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

int main(int argc, char** argv)
{
#if DEBUG
    return main_test(argc, argv);
#endif
    int r;
    r = allocator_init();
    if (r) return EXIT_FAILURE;

    r = master_error_log_init();
    if (r) {
        allocator_fin();
        return EXIT_FAILURE;
    }

    // main programm
    r = tauc_init();
    if (!r) {
        r = tauc_run(argc, argv);
        // report any erros that occured
        master_error_log_unwind(&TAUC.permmem);
        tauc_fin();
    }
    else {
        master_error_log_unwind(&TAUC.permmem);
    }

    // terminate gracefully
    allocator_fin();
    master_error_log_fin();
    return r ? EXIT_FAILURE : EXIT_SUCCESS;
}
