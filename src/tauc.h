#pragma once
#include "thread_context.h"

struct tauc{
    thread_context main_thread_context;
    worker_thread* worker_threads;
    error_log error_log;
};

extern struct tauc TAUC;