#include "debug_utils.h"
#include "utils/threading.h"
#include <stdarg.h>
#include <string.h>
#include <assert.h>
#include "math_utils.h"
#include "dbuffer.h"
#include "error.h"

// guraranteed to be zero
THREAD_LOCAL dbuffer buff;
atomic_ureg mtx_ref_count;
mutex flush_mtx;

static int init_buffers()
{
    if (buff.start) return OK;
    if (buff.end == 1) return ERR;
    if (!talloc_initialized()) return ERR;
    buff.end = (void*)1;
    dbuffer_init(&buff);
    ureg rc = atomic_ureg_inc(&mtx_ref_count) + 1;
    while (rc < (ureg)UREGH_MAX + 1) {
        if (rc == 1) {
            mutex_init(&flush_mtx);
            atomic_ureg_add(&mtx_ref_count, UREGH_MAX);
            break;
        }
        rc = atomic_ureg_load(&mtx_ref_count);
    }
    return OK;
}
void debug_utils_free_res()
{
    if (!buff.start) return;
    tflush();
    dbuffer_fin(&buff);
    buff.start = NULL;
    buff.end = NULL;
    ureg rc = atomic_ureg_dec(&mtx_ref_count) - 1;
    while (rc == UREGH_MAX) {
        // loop needed because of spurious failiure
        if (atomic_ureg_cas(&mtx_ref_count, &rc, 0)) {
            mutex_fin(&flush_mtx);
            break;
        }
        // back off if somebody "subscribed" in the mean time
        if (rc > UREG_MAX) break;
    }
}
void tprintf(const char* format, ...)
{
    if (init_buffers()) {
        va_list args;
        va_start(args, format);
        vprintf(format, args);
        va_end(args);
        return;
    }
    sreg size = 256;
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
    if (init_buffers()) {
        fputs(c, stdout);
        return;
    }
    dbuffer_append(&buff, c, strlen(c));
}
void tputs(const char* c)
{
    if (init_buffers()) {
        puts(c);
        return;
    }
    dbuffer_append(&buff, c, strlen(c));
    *(char*)dbuffer_claim(&buff, 1) = '\n';
}
void tputchar(const char c)
{
    if (init_buffers()) {
        putchar(c);
        return;
    }
    dbuffer_append(&buff, &c, 1);
}
void tprintn(const char* c, ureg n)
{
    if (init_buffers()) {
        fwrite(c, n, 1, stdout);
        return;
    }
    dbuffer_append(&buff, c, n);
}
void tflush()
{
    if (init_buffers()) {
        fflush(stdout);
        return;
    }
    ureg len = dbuffer_get_size(&buff);
    mutex_lock(&flush_mtx);
    fflush(stdout);
    if (len == 1 && buff.start[0] == '\n') {
        printf("lols\n");
    }
    else {
        fwrite(buff.start, len, 1, stdout);
    }
    fflush(stdout);
    mutex_unlock(&flush_mtx);
    dbuffer_clear(&buff);
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
void pretty_print_timer_elapsed(timer* t)
{
    timer tc = *t;
    timer_stop(&tc);
    timespan ts;
    timer_get_elapsed(&tc, &ts);
    pretty_print_timespan(&ts);
}

#include <unistd.h>
void limit_mem(ureg max_bytes)
{
    int pid = getpid();
    char buf[256];
    sprintf(buf, "/usr/bin/prlimit --as=%zu --pid=%i", max_bytes, pid);
    system(buf);
}
