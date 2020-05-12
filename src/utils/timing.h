#pragma once

#include "plattform.h"
#include "types.h"
#if HOST_OS_LINUX
#include "os/linux/timing_linux.h"
#else
#error no timing backend for configured plattform
#endif

typedef struct timespan_s {
    ureg nanos;
    ureg seconds;
} timespan;

int timer_init_bare(timer* t);
int timer_init(timer* t);
int timer_start(timer* t);
int timer_stop(timer* t);

int timer_get_elapsed(timer* t, timespan* ts);

int multi_timer_init_bare(multi_timer* t);
int multi_timer_init(multi_timer* t);
int multi_timer_start(multi_timer* t);
int multi_timer_stop(multi_timer* t);

int multi_timer_get_elapsed_real(multi_timer* t, timespan* ts);
int multi_timer_get_elapsed_process(multi_timer* t, timespan* ts);
int multi_timer_get_elapsed_thread(multi_timer* t, timespan* ts);
int multi_timer_get_elapsed_kernel(multi_timer* t, timespan* ts);

// timespan % 1 second
ureg timespan_get_rnanos(timespan* t);
ureg timespan_get_rmicros(timespan* t);
ureg timespan_get_rmillis(timespan* t);

// timespan % 1 minute
ureg timespan_get_rseconds(timespan* t);
// timespan % 1 hour
ureg timespan_get_rminutes(timespan* t);
// timespan % 1 day
ureg timespan_get_rhours(timespan* t);

// total timespan (in case of overflow returns UREG_MAX)
ureg timespan_get_nanos(timespan* t);
ureg timespan_get_micros(timespan* t);
ureg timespan_get_millis(timespan* t);
ureg timespan_get_seconds(timespan* t);
ureg timespan_get_minutes(timespan* t);
ureg timespan_get_hours(timespan* t);
ureg timespan_get_days(timespan* t);

// timespan % 1 second
freg timespan_get_frnanos(timespan* t);
freg timespan_get_frmicros(timespan* t);
freg timespan_get_frmillis(timespan* t);

// timespan % 1 minute
freg timespan_get_frseconds(timespan* t);
// timespan % 1 hour
freg timespan_get_frminutes(timespan* t);
// timespan % 1 day
freg timespan_get_frhours(timespan* t);

// total timespan
freg timespan_get_fnanos(timespan* t);
freg timespan_get_fmicros(timespan* t);
freg timespan_get_fmillis(timespan* t);
freg timespan_get_fseconds(timespan* t);
freg timespan_get_fminutes(timespan* t);
freg timespan_get_fhours(timespan* t);
freg timespan_get_fdays(timespan* t);

