#include "evmap2.h"
#include "error.h"
#include <stdio.h>
#define STRIDE (UREG_MAX / 2)

int evmap2_init(evmap2* m, ureg max_change_count)
{
    int r = mutex_init(&m->write_lock);
    if (r) return r;
    atomic_ureg_init(&m->pending_writers, 0);
    atomic_ureg_init(&m->counter, 0);
    atomic_ureg_init(&m->done_counts[0], 0);
    atomic_ureg_init(&m->done_counts[1], 0);
    m->reader_id = 0;
    m->prev_readers = 0;
    m->swapped = false;
    m->change_count = 0;
    m->max_change_count = max_change_count;
    return OK;
}
void evmap2_fin(evmap2* m)
{
    mutex_fin(&m->write_lock);
}

ureg evmap2_start_read(evmap2* m)
{
    ureg id = atomic_ureg_inc(&m->counter);
    return id / STRIDE;
}
void evmap2_end_read(evmap2* m, ureg id)
{
    atomic_ureg_inc(&m->done_counts[id]);
}

ureg evmap2_start_write(evmap2* m, ureg* id)
{
    atomic_ureg_inc(&m->pending_writers);
    mutex_lock(&m->write_lock);
    *id = (m->reader_id + 1) % 2;
    if (m->swapped) {
        atomic_ureg* write_dc = &m->done_counts[*id];
        while (atomic_ureg_load(write_dc) != m->prev_readers) {
            thread_yield();
        }
        atomic_ureg_store(write_dc, 0);
        m->swapped = false;
        ureg old_cc = m->change_count;
        m->change_count = 1;
        return old_cc;
    }
    m->change_count++;
    return 0;
}
void evmap2_add_extra_changes(evmap2* m, ureg n)
{
    m->change_count += n;
}
void evmap2_end_write(evmap2* m)
{
    ureg pw = atomic_ureg_dec(&m->pending_writers) - 1;
    if (pw == 0 || m->change_count >= m->max_change_count) {
        if (m->reader_id == 0) {
            ureg cntr = atomic_ureg_add(&m->counter, STRIDE - m->prev_readers);
            m->prev_readers = cntr - m->prev_readers;
            m->reader_id = 1;
        }
        else {
            ureg cntr = atomic_ureg_sub(&m->counter, STRIDE + m->prev_readers);
            m->prev_readers = cntr - (STRIDE + m->prev_readers);
            m->reader_id = 0;
        }
        m->swapped = true;
    }
    mutex_unlock(&m->write_lock);
}
