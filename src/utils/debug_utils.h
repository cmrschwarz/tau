#ifndef TAUC_UTILS_DEBUG_UTILS_H
#define TAUC_UTILS_DEBUG_UTILS_H

#include "timing.h"
#include <stdio.h>

void pretty_print_timespan(timespan* ts)
{
    ureg hrs = timespan_get_hours(ts);
    ureg mnts = timespan_get_rminutes(ts);
    ureg secs = timespan_get_rseconds(ts);
    freg mlls = timespan_get_frmillis(ts);
    if (hrs > 0) {
        printf("%lluh %llum %llus %fms", hrs, mnts, secs, mlls);
    }
    else if (mnts > 0) {
        printf("%llum %llus %fms", mnts, secs, mlls);
    }
    else if (secs > 0) {
        printf("%llus %fms", secs, mlls);
    }
    else {
        printf("%fms", mlls);
    }
}

#define TIME(name, code)                                                       \
    do {                                                                       \
        puts("INITIATE TIMING " name);                                         \
        timer ____timer_reserved_name_for_bench_macro;                         \
        timer_init(&____timer_reserved_name_for_bench_macro);                  \
        {                                                                      \
            code                                                               \
        }                                                                      \
        timer_stop(&____timer_reserved_name_for_bench_macro);                  \
        fputs("FINISHED TIMING " name ", ", stdout);                           \
        timespan ____timespan_reserved_name_for_bench_macro;                   \
        timer_get_elapsed(                                                     \
            &____timer_reserved_name_for_bench_macro,                          \
            &____timespan_reserved_name_for_bench_macro);                      \
        pretty_print_timespan(&____timespan_reserved_name_for_bench_macro);    \
        puts(" elapsed");                                                      \
    } while (false)

#endif