#include "time_conversions.h"
#include "timing.h"
#include "types.h"

ureg timespan_get_rnanos(timespan* t)
{
    return t->nanos;
}
ureg timespan_get_rmicros(timespan* t)
{
    return t->nanos / microsecs_nanosec;
}
ureg timespan_get_rmillis(timespan* t)
{
    return t->nanos / millisecs_nanosec;
}
ureg timespan_get_rseconds(timespan* t)
{
    return t->seconds % secs_minute;
}
ureg timespan_get_rminutes(timespan* t)
{
    return (t->seconds / secs_minute) % minutes_hour;
}
ureg timespan_get_rhours(timespan* t)
{
    return (t->seconds / secs_hour) % hours_day;
}

static inline ureg get_max_fit(ureg large, ureg small, ureg scale)
{
    const ureg max_large = UREG_MAX / scale;
    const ureg max_small = UREG_MAX - max_large * scale;
    if (large < max_large) return large * scale + small;
    if (large > max_large) return UREG_MAX;
    if (small > max_small) return UREG_MAX;
    return large * scale + small;
}
ureg timespan_get_nanos(timespan* t)
{
    return get_max_fit(t->seconds, t->nanos, nanosecs_sec);
}
ureg timespan_get_micros(timespan* t)
{
    return get_max_fit(t->seconds, t->nanos / microsecs_nanosec, microsecs_sec);
}
ureg timespan_get_millis(timespan* t)
{
    return get_max_fit(t->seconds, t->nanos / millisecs_nanosec, microsecs_sec);
}
ureg timespan_get_seconds(timespan* t)
{
    return t->seconds;
}
ureg timespan_get_minutes(timespan* t)
{
    return t->seconds / secs_minute;
}
ureg timespan_get_hours(timespan* t)
{
    return t->seconds / secs_hour;
}
ureg timespan_get_days(timespan* t)
{
    return t->seconds / secs_day;
}

freg timespan_get_frnanos(timespan* t)
{
    return (freg)t->nanos;
}
freg timespan_get_frmicros(timespan* t)
{
    return (freg)t->nanos / (freg)microsecs_nanosec;
}
freg timespan_get_frmillis(timespan* t)
{
    return (freg)t->nanos / (freg)millisecs_nanosec;
}
freg timespan_get_frseconds(timespan* t)
{
    return (freg)t->nanos / (freg)nanosecs_sec +
           (freg)(t->seconds % secs_minute);
}
freg timespan_get_frminutes(timespan* t)
{
    return (freg)t->nanos / (freg)(nanosecs_sec * secs_minute) +
           (freg)(t->seconds % secs_hour) / (freg)secs_minute;
}
freg timespan_get_frhours(timespan* t)
{
    return (freg)t->nanos / (freg)(nanosecs_sec * secs_hour) +
           (freg)(t->seconds % secs_day) / (freg)secs_hour;
}

freg timespan_get_fnanos(timespan* t)
{
    return (freg)t->seconds * (freg)nanosecs_sec + (freg)t->nanos;
}
freg timespan_get_fmicros(timespan* t)
{
    return (freg)t->seconds * (freg)microsecs_sec +
           (freg)t->nanos / (freg)microsecs_nanosec;
}
freg timespan_get_fmillis(timespan* t)
{
    return (freg)t->seconds * (freg)millisecs_sec +
           (freg)t->nanos / (freg)millisecs_nanosec;
}
freg timespan_get_fseconds(timespan* t)
{
    return (freg)t->seconds + (freg)t->nanos / (freg)nanosecs_sec;
}
freg timespan_get_fminutes(timespan* t)
{
    return (freg)t->seconds / (freg)secs_minute +
           (freg)t->nanos / (freg)(nanosecs_sec * secs_minute);
}
freg timespan_get_fhours(timespan* t)
{
    return (freg)t->seconds / (freg)secs_hour +
           (freg)t->nanos / (freg)(nanosecs_sec * secs_hour);
}
freg timespan_get_fdays(timespan* t)
{
    return (freg)t->seconds / (freg)secs_day +
           (freg)t->nanos / (freg)(nanosecs_sec * secs_day);
}
