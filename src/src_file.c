#include "src_file.h"
#include "error_log.h"
int src_file_partial_fin(src_file* f, int i, int r)
{
    switch (i) {
        case 0: atomic_bool_fin(&f->parsed);
        case 1: rwslock_fin(&f->lock);
        case 2: src_map_fin(&f->src_map);
    }
    return r;
}

// THINK: who manages memory of the string here
int src_file_init(src_file* f, thread_context* tc, char* path)
{
    int r = src_map_init(&f->src_map, tc);
    if (r) return r;
    r = rwslock_init(&f->lock);
    if (r) return src_file_partial_fin(f, 2, r);
    r = atomic_bool_init(&f->parsed, false);
    if (r) return src_file_partial_fin(f, 1, r);
    f->path = path;
    return OK;
}
void src_file_fin(src_file* f)
{
    src_file_partial_fin(f, 0, 0);
}