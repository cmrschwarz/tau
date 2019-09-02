#ifndef TAUC_UTILS_DEBUG_UTILS_H
#define TAUC_UTILS_DEBUG_UTILS_H

#include "timing.h"
#include <stdio.h>

static inline void pretty_print_timespan(timespan* ts)
{
    ureg hrs = timespan_get_hours(ts);
    ureg mnts = timespan_get_rminutes(ts);
    freg secs = timespan_get_frseconds(ts);
    freg millis = timespan_get_fmillis(ts);
    if (hrs > 0) {
        printf("%02zu:%02zu:%05.2f", hrs, mnts, secs);
    }
    else if (mnts > 0) {
        printf("%02zu:%05.2f", mnts, secs);
    }
    else if (secs >= 1) {
        printf("%04.2f s", secs);
    }
    else {
        printf("%.2f ms", millis);
    }
}
#if DEBUG

#define TIME(code)                                                             \
    do {                                                                       \
        timer ____timer_reserved_name_for_bench_macro;                         \
        timer_init(&____timer_reserved_name_for_bench_macro);                  \
        {                                                                      \
            code                                                               \
        }                                                                      \
        timer_stop(&____timer_reserved_name_for_bench_macro);                  \
        timespan ____timespan_reserved_name_for_bench_macro;                   \
        timer_get_elapsed(                                                     \
            &____timer_reserved_name_for_bench_macro,                          \
            &____timespan_reserved_name_for_bench_macro);                      \
        printf("    ");                                                        \
        pretty_print_timespan(&____timespan_reserved_name_for_bench_macro);    \
        puts(" elapsed");                                                      \
        fflush(stdout);                                                        \
    } while (false)
#else

#define TIME(code)                                                             \
    do {                                                                       \
        code                                                                   \
    } while (false)
#endif

#endif