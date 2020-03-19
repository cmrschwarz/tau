#ifndef TAUC_UTILS_EVMAP2_H
#define TAUC_UTILS_EVMAP2_H

#include "threading.h"
typedef struct evmap2_s {
    atomic_ureg counter;
    atomic_ureg pending_writers;
    mutex write_lock;
    ureg prev_readers;
    bool swapped;
    atomic_ureg done_counts[2];
    ureg reader_id;
    ureg change_count;
    ureg max_change_count;
} evmap2;

int evmap2_init(evmap2* m, ureg max_change_count);
void evmap2_fin(evmap2* m);
ureg evmap2_start_read(evmap2* m);
void evmap2_end_read(evmap2* m, ureg i);

ureg evmap2_start_write(evmap2* m, ureg* id);
void evmap2_add_extra_changes(evmap2* m, ureg n);
void evmap2_end_write(evmap2* m);
#endif
