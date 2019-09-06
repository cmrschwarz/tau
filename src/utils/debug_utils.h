#ifndef TAUC_UTILS_DEBUG_UTILS_H
#define TAUC_UTILS_DEBUG_UTILS_H

#include "timing.h"
#include <stdio.h>

void tprintf(const char* format, ...);
void tputs(const char* c);
void tput(const char* c);
void tputchar(const char c);
void tflush();
void tprintn(const char* c, ureg n);

void pretty_print_timespan(timespan* ts);
void debug_utils_free_res();

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
        tprintf("[");                                                          \
        pretty_print_timespan(&____timespan_reserved_name_for_bench_macro);    \
        tputs("]");                                                            \
    } while (false)
#else

#define TIME(code)                                                             \
    do {                                                                       \
        code                                                                   \
    } while (false)
#endif

#endif