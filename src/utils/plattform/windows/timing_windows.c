#include "../../plattform.h"
#if HOST_OS_WINDOWS
#include "../../time_conversions.h"
#include "../../timing.h"
#include <time.h>

int timer_init_bare(timer* t)
{
    return 0;
}
int timer_init(timer* t)
{
    return timer_start(t);
}
int timer_start(timer* t)
{
    return 0;
}
int timer_stop(timer* t)
{
    return 0;
}
int timer_get_elapsed(timer* t, timespan* ts)
{
    return 0;
}

// TODO
int multi_timer_init_bare(multi_timer* t)
{
    return 0;
}
int multi_timer_init(multi_timer* t)
{
    return multi_timer_start(t);
}
int multi_timer_start(multi_timer* t)
{
    return 0;
}
int multi_timer_stop(multi_timer* t)
{
    return 0;
}

int multi_timer_get_elapsed_real(multi_timer* t, timespan* ts)
{
    return -1;
}
int multi_timer_get_elapsed_thread(multi_timer* t, timespan* ts)
{
    return -1;
}
int multi_timer_get_elapsed_process(multi_timer* t, timespan* ts)
{
    return -1;
}
int multi_timer_get_elapsed_sys(multi_timer* t, timespan* ts)
{
    return -1;
}
#endif
