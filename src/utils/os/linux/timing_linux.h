#pragma once
#include <time.h>

//TODO: make this work with all the kinds of different architectures

typedef struct timer{
    struct timespec start;
    struct timespec end;
}timer;

typedef struct multi_timer{
    struct timespec thread_start;
    struct timespec thread_end;
    struct timespec process_start;
    struct timespec process_end;
    struct timespec real_start;
    struct timespec real_end;
}multi_timer;