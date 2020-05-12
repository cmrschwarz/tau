#pragma once

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

#define TIME_MSG(msg, after_msg, code)                                         \
    do {                                                                       \
        timer ____timer_reserved_name_for_bench_macro;                         \
        timer_init(&____timer_reserved_name_for_bench_macro);                  \
        code;                                                                  \
        timer_stop(&____timer_reserved_name_for_bench_macro);                  \
        timespan ____timespan_reserved_name_for_bench_macro;                   \
        timer_get_elapsed(                                                     \
            &____timer_reserved_name_for_bench_macro,                          \
            &____timespan_reserved_name_for_bench_macro);                      \
        tprintf("%s[", msg);                                                   \
        pretty_print_timespan(&____timespan_reserved_name_for_bench_macro);    \
        tprintf("]%s", after_msg);                                             \
    } while (false)
#else

#define TIME_MSG(msg, after_msg, code)                                         \
    do {                                                                       \
        code                                                                   \
    } while (false)
#endif
#define TIME(code) TIME_MSG("", "\n", code)

// for variables only used in asserts to get rid of -Wunused-variable
#define UNUSED(x) ((void)(x))

