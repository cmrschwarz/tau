#pragma once
#include <time.h>

typedef struct timer_s {
    struct timespec start;
    struct timespec end;
} timer;

typedef struct multi_timer_s {
    struct timespec thread_start;
    struct timespec thread_end;
    struct timespec process_start;
    struct timespec process_end;
    struct timespec real_start;
    struct timespec real_end;
} multi_timer;

