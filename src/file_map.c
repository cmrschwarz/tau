#include "file_map.h"
#include "utils/error.h"
#include "utils/fnv_hash.h"
#include "utils/math_utils.h"

static inline int file_map_head_init(
    file_map_head* h, file_map* fm, src_dir* parent, string name, bool is_dir)
{
    ureg name_len = string_len(name);
    h->name.start = pool_alloc(&fm->string_mem_pool, name_len);
    if (!h->name.start) return ERR;
    h->name.end = h->name.start + name_len;
    memcpy(h->name.start, name.start, name_len);
    h->parent = parent;
    h->next = NULL;
    h->is_directory = is_dir;
    return OK;
}

static inline void file_map_head_fin(file_map_head* h)
{
}

static inline int
src_file_init(src_file* f, file_map* fm, src_dir* parent, string name)
{
    int r = rwslock_init(&f->lock);
    if (r) return ERR;
    r = atomic_ureg_init(&f->stage, SFS_UNPARSED);
    if (r) {
        rwslock_fin(&f->lock);
        return ERR;
    }
    r = file_map_head_init(&f->head, fm, parent, name, false);
    if (r) {
        atomic_ureg_fin(&f->stage);
        rwslock_fin(&f->lock);
    }
    return OK;
}

static inline void src_file_fin(src_file* f)
{
    src_file_stage s = atomic_ureg_load(&f->stage);
    if (s >= SFS_PARSING) {
        src_map_fin(&f->src_map);
    }
    atomic_ureg_fin(&f->stage);
    rwslock_fin(&f->lock);
    file_map_head_fin(&f->head);
}

static inline int
src_dir_init(src_dir* d, file_map* fm, src_dir* parent, string name)
{
    return file_map_head_init(&d->head, fm, parent, name, true);
}

static inline void src_dir_fin(src_dir* d)
{
    file_map_head_fin(&d->head);
}

ureg src_file_get_path_len(src_file* f)
{
    file_map_head* n = &f->head;
    ureg len = 0;
    while (n) {
        len += string_len(n->name);
        n = &n->parent->head;
    }
    return len;
}
static char* file_map_head_write_path(file_map_head* h, char* tgt)
{
    if (h->parent != NULL) {
        tgt = file_map_head_write_path(&h->parent->head, tgt);
        *tgt = '/';
        tgt++;
    }
    ureg len = string_len(h->name);
    memcpy(tgt, h->name.start, len);
    return tgt + len;
}
void src_file_write_path(src_file* f, char* tgt)
{
    *file_map_head_write_path(&f->head, tgt) = '\0';
}
static void src_map_head_print_path(file_map_head* h, bool to_stderr)
{
    if (h->parent) {
        src_map_head_print_path(&h->parent->head, to_stderr);
        fputc('/', to_stderr ? stderr : stdout);
    }
    to_stderr ? string_print_stderr(h->name) : string_print(h->name);
}
void src_file_print_path(src_file* f, bool to_stderr)
{
    src_map_head_print_path(&f->head, to_stderr);
}
int src_file_start_parse(src_file* f, thread_context* tc)
{
    atomic_ureg_store(&f->stage, SFS_PARSING);
    return src_map_init(&f->src_map, tc);
}

// FILE MAP
int file_map_init(file_map* fm)
{
    if (pool_init(&fm->file_mem_pool)) return ERR;
    if (pool_init(&fm->string_mem_pool)) {
        pool_fin(&fm->file_mem_pool);
        return ERR;
    }
    // we alloc zero initialized so all values are NULL pointers
    static const ureg START_SIZE = PAGE_SIZE;
    static const ureg START_CAPACITY = PAGE_SIZE / sizeof(file_map_head**);
    fm->table_start = (file_map_head**)tmallocz(START_SIZE);
    if (!fm->table_start) {
        pool_fin(&fm->string_mem_pool);
        pool_fin(&fm->file_mem_pool);
        return ERR;
    }
    fm->table_end = (file_map_head**)ptradd(fm->table_start, START_SIZE);
    // this limits the "used size" to a power of 2
    // elements after that size will be used for colliding elements,
    // but the hash function will not give out their indices directly
    fm->size_bits = ulog2(START_CAPACITY);
    fm->hash_mask = (1 << fm->size_bits) - 1;
    fm->elem_count = 0;
    fm->grow_on_elem_count = START_CAPACITY / 4 * 3; // grow on 75 %
    return OK;
}
void file_map_fin(file_map* fm)
{
    for (file_map_head** i = fm->table_start; i != fm->table_end; i++) {
        if (*i) {
            if ((**i).is_directory) {
                src_dir_fin((src_dir*)*i);
            }
            else {
                src_file_fin((src_file*)*i);
            }
        }
    }
    pool_fin(&fm->string_mem_pool);
    pool_fin(&fm->file_mem_pool);
    tfree(fm->table_start);
}
static inline file_map_head**
file_map_get_raw(file_map* fm, src_dir* parent, string name)
{
    ureg hash = fnv_hash_pointer(FNV_START_HASH, parent);
    hash = fnv_hash_string(hash, name);
    hash = fnv_fold(hash, fm->size_bits, fm->hash_mask);
    file_map_head** pos = fm->table_start + hash;
    while (true) {
        if (!*pos) return pos;
        if (string_cmp(name, (**pos).name) == 0) return pos;
        pos = &(**pos).next;
    }
}
static inline int file_map_grow(file_map* fm)
{
    ureg capacity_new = 2 * (fm->table_end - fm->table_start);
    ureg size_new = capacity_new * sizeof(file_map_head*);

    file_map_head** fmnew = tmallocz(size_new);
    if (!fmnew) return ERR;
    file_map_head** old = fm->table_start;
    file_map_head** old_end = fm->table_end;
    fm->table_start = fmnew;
    fm->table_end = fmnew + capacity_new;
    fm->size_bits += 1;
    fm->hash_mask = (fm->size_bits << 1) & 1;
    fm->elem_count = 0;
    fm->grow_on_elem_count *= 2;
    while (old != old_end) {
        file_map_head* next = *old;
        while (next) {
            *file_map_get_raw(fm, next->parent, next->name) = next;
            next = next->next;
        }
        old++;
    }
    // PERF: we could give this memory region to the pools
    tfree(old);
    return OK;
}
src_file* file_map_get_file(file_map* fm, src_dir* parent, string name)
{
    mutex_lock(&fm->lock);
    src_file** fp = (src_file**)file_map_get_raw(fm, parent, name);
    if (*fp) {
        src_file* f = *fp;
        mutex_unlock(&fm->lock);
        return f;
    }
    src_file* f = pool_alloc(&fm->file_mem_pool, sizeof(src_file));
    if (!f) {
        mutex_unlock(&fm->lock);
        return NULL;
    }
    int r = src_file_init(f, fm, parent, name);
    if (r) {
        mutex_unlock(&fm->lock);
        return NULL;
    }
    fm->elem_count++;
    if (fm->elem_count == fm->grow_on_elem_count) {
        if (file_map_grow(fm)) {
            mutex_unlock(&fm->lock);
            return NULL;
        }
    }
    mutex_unlock(&fm->lock);
    return f;
}

src_dir* file_map_get_dir(file_map* fm, src_dir* parent, string name)
{
    mutex_lock(&fm->lock);
    src_dir** dirp = (src_dir**)file_map_get_raw(fm, parent, name);
    if (*dirp) {
        mutex_unlock(&fm->lock);
        return *dirp;
    }
    src_dir* dir = pool_alloc(&fm->file_mem_pool, sizeof(src_dir));
    if (!dir) {
        mutex_unlock(&fm->lock);
        return NULL;
    }
    int r = src_dir_init(dir, fm, parent, name);
    if (r) {
        mutex_unlock(&fm->lock);
        return NULL;
    }
    fm->elem_count++;
    if (fm->elem_count == fm->grow_on_elem_count) {
        if (file_map_grow(fm)) {
            mutex_unlock(&fm->lock);
            return NULL;
        }
        *(src_dir**)file_map_get_raw(fm, parent, name) = dir;
    }
    else {
        *dirp = dir;
    }
    mutex_unlock(&fm->lock);
    return dir;
}

src_file* file_map_get_file_from_path(file_map* fm, string path)
{
    src_dir* parent = NULL;
    char* curr_start = path.start;
    char* curr_end = path.start;
    while (true) {
        while (curr_end != path.end && *curr_end != '/') {
            curr_end++;
        }
        if (curr_end == path.end) {
            string x;
            x.start = curr_start;
            x.end = curr_end;
            return file_map_get_file(fm, parent, x);
        }
        else {
            string x;
            x.start = curr_start;
            x.end = curr_end;
            parent = file_map_get_dir(fm, parent, x);
            if (!parent) return NULL;
            curr_end++;
            curr_start = curr_end;
        }
    }
}