#include "debug_utils.h"
#include "utils/threading.h"
#include <stdarg.h>
#include <string.h>
#include <assert.h>
#include "math_utils.h"
#include "dbuffer.h"

#if DEBUG
THREAD_LOCAL dbuffer buff; // guraranteed to be zero
static void init_buffers()
{
    if (buff.start) return;
    dbuffer_init(&buff);
}
void debug_utils_free_res()
{
    if (!buff.start) return;
    dbuffer_fin(&buff);
    buff.start = NULL;
}
static void debugflush()
{
    if (!(plattform_get_virt_core_count() == 1)) return;
    tflush();
}
void tprintf(const char* format, ...)
{
    init_buffers();
    ureg size = 256;
    sreg len = 0;
    char* tgt = dbuffer_claim(&buff, size);
    while (true) {
        va_list args;
        va_start(args, format);
        len = vsnprintf(tgt, size, format, args);
        va_end(args);
        assert(len > 0);
        if (len <= size) break;
        tgt = ptrsub(dbuffer_claim(&buff, len - size + 1), size);
        size = len + 1;
    }
    buff.head -= (size - len);
}
void tput(const char* c)
{
    init_buffers();
    dbuffer_append(&buff, c, strlen(c));
    debugflush();
}
void tputs(const char* c)
{
    init_buffers();
    dbuffer_append(&buff, c, strlen(c));
    *(char*)dbuffer_claim(&buff, 1) = '\n';
    debugflush();
}
void tputchar(const char c)
{
    init_buffers();
    dbuffer_append(&buff, &c, 1);
    debugflush();
}
void tprintn(const char* c, ureg n)
{
    init_buffers();
    dbuffer_append(&buff, c, n);
    debugflush();
}
void tflush()
{
    ureg len = dbuffer_get_size(&buff);
    fwrite(buff.start, len, 1, stdout);
    dbuffer_clear(&buff);
    fflush(stdout);
}

void pretty_print_timespan(timespan* ts)
{
    ureg hrs = timespan_get_hours(ts);
    ureg mnts = timespan_get_rminutes(ts);
    freg secs = timespan_get_frseconds(ts);
    freg millis = timespan_get_fmillis(ts);
    if (hrs > 0) {
        tprintf("%02zu:%02zu:%05.2f", hrs, mnts, secs);
    }
    else if (mnts > 0) {
        tprintf("%02zu:%02.0f", mnts, secs);
    }
    else if (secs >= 1) {
        tprintf("%04.2f s", secs);
    }
    else {
        tprintf("%.2f ms", millis);
    }
}
#else
void tprintf(const char* format, ...)
{
}
void tputs(const char* c)
{
}
void tput(const char* c)
{
}
void tputchar(const char c)
{
}
void tflush()
{
}
void tprintn(const char* c, ureg n)
{
}

void pretty_print_timespan(timespan* ts)
{
}
void debug_utils_free_res()
{
}
#endif