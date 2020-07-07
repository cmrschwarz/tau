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
void pretty_print_timer_elapsed(timer* t);
void debug_utils_free_res();

#if DEBUG

#define TIME_MSG(code, code_before_msg)                                        \
    do {                                                                       \
        timer ____timer;                                                       \
        timer_init(&____timer);                                                \
        code;                                                                  \
        timer_stop(&____timer);                                                \
        timespan ____timespan;                                                 \
        timer_get_elapsed(&____timer, &____timespan);                          \
        code_before_msg;                                                       \
        tprintf("[");                                                          \
        pretty_print_timespan(&____timespan);                                  \
        tprintf("]");                                                          \
    } while (false)
#else

#define TIME_MSG(code, code_before_msg)                                        \
    do {                                                                       \
        code                                                                   \
    } while (false)
#endif
#define TIME(code) TIME_MSG(code, ;)
#define TIME_MSG_LN(code, code_before_msg)                                     \
    do {                                                                       \
        TIME_MSG(code, code_before_msg);                                       \
        tputs("");                                                             \
        tflush();                                                              \
    } while (false)

// for variables only used in asserts to get rid of -Wunused-variable
#define UNUSED(x) ((void)(x))
