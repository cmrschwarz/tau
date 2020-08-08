#include "file_map.h"
#include "tauc.h"
#include "utils/error.h"
#include "utils/fnv_hash.h"
#include "utils/math_utils.h"
#include "utils/panic.h"
#include <assert.h>

static inline int file_map_head_init(
    file_map_head* h, file_map* fm, src_dir* parent, string name,
    ast_node_kind kind)
{
    ureg name_len = string_len(name);
    h->name.start = pool_alloc(&fm->string_mem_pool, name_len);
    if (!h->name.start) return ERR;
    h->name.end = h->name.start + name_len;
    memcpy(h->name.start, name.start, name_len);
    h->parent = parent;
    h->next = NULL;
    h->elem.kind = kind;
    return OK;
}

static inline void file_map_head_fin(file_map_head* h)
{
}

void file_map_iterator_begin(file_map_iterator* it, file_map* fm)
{
    it->head = fm->table_start;
    it->end = fm->table_end;
}
src_file* file_map_iterator_next_file(file_map_iterator* it)
{
    while (it->head != it->end) {
        if (*it->head) {
            if ((**it->head).elem.kind == ELEM_SRC_FILE) {
                src_file* f = (src_file*)*it->head;
                it->head++;
                return f;
            }
        }
        it->head++;
    }
    return NULL;
}
src_dir* file_map_iterator_next_dir(file_map_iterator* it)
{
    while (it->head != it->end) {
        if (*it->head) {
            if ((**it->head).elem.kind == ELEM_SRC_DIR) {
                src_dir* d = (src_dir*)*it->head;
                it->head++;
                return d;
            }
        }
        it->head++;
    }
    return NULL;
}
file_map_head* file_map_iterator_next(file_map_iterator* it)
{
    while (it->head != it->end) {
        if (*it->head) {
            file_map_head* h = *it->head;
            it->head++;
            return h;
        }
        it->head++;
    }
    return NULL;
}

static inline int src_lib_init(
    src_lib* l, file_map* fm, src_dir* parent, string name, bool is_dynamic)
{

    int r = aseglist_init(&l->requiring_modules);
    if (r) return r;
    atomic_boolean_init(&l->loaded, false);
    atomic_boolean_init(&l->loaded_for_pp, false);
    l->dynamic = is_dynamic;
    return file_map_head_init(
        (file_map_head*)l, fm, parent, name, ELEM_SRC_LIB);
}

static inline void src_lib_fin(src_lib* l)
{
    aseglist_fin(&l->requiring_modules);
    file_map_head_fin(&l->head);
}

static inline int
src_dir_init(src_dir* d, file_map* fm, src_dir* parent, string name)
{
    return file_map_head_init(
        (file_map_head*)d, fm, parent, name, ELEM_SRC_DIR);
}

static inline void src_dir_fin(src_dir* d)
{
    file_map_head_fin(&d->head);
}

static inline int
src_file_init(src_file* f, file_map* fm, src_dir* parent, string name)
{
    int r =
        file_map_head_init((file_map_head*)f, fm, parent, name, ELEM_SRC_FILE);
    if (r) return r;
    r = rwslock_init(&f->stage_lock);
    if (r) return ERR;

    r = aseglist_init(&f->requiring_modules);
    if (r) {
        rwslock_fin(&f->stage_lock);
        return ERR;
    }
    if (r) {
        aseglist_fin(&f->requiring_modules);
        rwslock_fin(&f->stage_lock);
        return ERR;
    }
    f->stage = SFS_UNNEEDED;
    f->file_stream = NULL;
    return OK;
}

int src_file_require(
    src_file* f, tauc* t, src_map* requiring_smap, src_range requiring_srange,
    mdg_node* requiring_mdgn, bool requirer_needed)
{
    src_file_stage prev_stage;
    rwslock_write(&f->stage_lock);
    prev_stage = f->stage;
    if (prev_stage == SFS_UNNEEDED && requirer_needed) {
        f->stage = SFS_UNPARSED;
    }
    // we do this inside the lock because if the stage finishes we need our
    // notification
    atomic_ureg_inc(&requiring_mdgn->unparsed_files);
    aseglist_add(&f->requiring_modules, requiring_mdgn);
    rwslock_end_write(&f->stage_lock);
    if (prev_stage == SFS_UNNEEDED && requirer_needed) {
        return tauc_request_parse(t, f, requiring_smap, requiring_srange);
    }
    return OK;
}

int src_lib_require(
    src_lib* l, tauc* t, src_map* requiring_smap, src_range requiring_srange,
    bool in_pp)
{
    if (!in_pp) {
        bool prev = atomic_boolean_swap(&l->loaded, true);
        if (prev) return OK;
        mutex_lock(&t->filemap.lock);
        int r = ptrlist_append(&t->filemap.rt_src_libs, l);
        mutex_unlock(&t->filemap.lock);
        return r;
    }
    else {
        bool prev = atomic_boolean_swap(&l->loaded_for_pp, true);
        if (prev) return OK;
        char* file_path = file_map_head_tmalloc_path(&l->head);
        if (!file_path) return ERR;
        int r = llvm_backend_link_for_pp(l->dynamic, file_path);
        tfree(file_path);
        return r;
    }
}

int file_map_head_require(
    file_map_head* h, tauc* t, src_map* requiring_smap,
    src_range requiring_srange, mdg_node* requiring_mdgn, bool in_pp,
    bool requirer_needed)
{
    if (h->elem.kind == ELEM_SRC_FILE) {
        return src_file_require(
            (src_file*)h, t, requiring_smap, requiring_srange, requiring_mdgn,
            requirer_needed);
    }
    else if (h->elem.kind == ELEM_SRC_LIB) {
        if (!requirer_needed) return OK;
        return src_lib_require(
            (src_lib*)h, t, requiring_smap, requiring_srange, in_pp);
    }
    else {
        assert(false);
    }
    return ERR;
}

static inline void src_file_fin(src_file* f)
{
    if (f->stage >= SFS_PARSING) {
        src_map_fin(&f->smap);
    }
    file_map_head_fin(&f->head);
    aseglist_fin(&f->requiring_modules);
    rwslock_fin(&f->stage_lock);
}

ureg file_map_head_get_path_len(file_map_head* h)
{
    ureg len = string_len(h->name);
    file_map_head* n = &h->parent->head;
    while (n) {
        len += 1;
        len += string_len(n->name);
        n = &n->parent->head;
    }
    return len;
}
static char* file_map_head_write_path_internal(file_map_head* h, char* tgt)
{
    if (h->parent != NULL) {
        tgt = file_map_head_write_path_internal(&h->parent->head, tgt);
        *tgt = '/';
        tgt++;
    }
    ureg len = string_len(h->name);
    memcpy(tgt, h->name.start, len);
    return tgt + len;
}
void file_map_head_write_path(file_map_head* h, char* tgt)
{
    *file_map_head_write_path_internal(h, tgt) = '\0';
}
char* file_map_head_tmalloc_path(file_map_head* h)
{
    char* file_path = tmalloc(file_map_head_get_path_len(h) + 1);
    if (!file_path) return NULL;
    file_map_head_write_path(h, file_path);
    return file_path;
}
void file_map_head_print_path(file_map_head* h, bool to_stderr)
{
    if (h->parent) {
        file_map_head_print_path(&h->parent->head, to_stderr);
        fputc('/', to_stderr ? stderr : stdout);
    }
    to_stderr ? string_print_stderr(h->name) : string_print(h->name);
}
int src_file_start_parse(src_file* f, thread_context* tc)
{
    // this is called from the parser, so no need to recheck whether the file is
    // actually unparsed
    rwslock_write(&f->stage_lock);
    f->stage = SFS_PARSING;
    rwslock_end_write(&f->stage_lock);
    return src_map_init(&f->smap, (ast_elem*)f, tc);
}
int src_file_done_parsing(src_file* f, thread_context* tc)
{
    rwslock_write(&f->stage_lock);
    f->stage = SFS_PARSED;
    aseglist_iterator it;
    aseglist_iterator_begin(&it, &f->requiring_modules);
    rwslock_end_write(&f->stage_lock);
    aseglist_add(&tc->t->mdg.root_node->module_frames, &f->root);
    while (true) {
        mdg_node* m = aseglist_iterator_next(&it);
        if (!m) break;
        int r = mdg_node_file_parsed(&tc->t->mdg, m, tc);
        if (r) return r;
    }
    return OK;
}

// FILE MAP
int file_map_init(file_map* fm)
{
    if (pool_init(&fm->file_mem_pool)) return ERR;
    if (pool_init(&fm->string_mem_pool)) {
        pool_fin(&fm->file_mem_pool);
        return ERR;
    }
    if (mutex_init(&fm->lock)) {
        pool_fin(&fm->string_mem_pool);
        pool_fin(&fm->file_mem_pool);
        return ERR;
    }
    if (ptrlist_init(&fm->rt_src_libs, 4)) {
        mutex_fin(&fm->lock);
        pool_fin(&fm->string_mem_pool);
        pool_fin(&fm->file_mem_pool);
        return ERR;
    }
    const ureg START_CAPACITY = 16;
    const ureg START_SIZE = START_CAPACITY * sizeof(file_map_head**);
    // we alloc zero initialized so all values are NULL pointers
    fm->table_start = (file_map_head**)tmallocz(START_SIZE);
    if (!fm->table_start) {
        ptrlist_fin(&fm->rt_src_libs);
        mutex_fin(&fm->lock);
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
        file_map_head* h = *i;
        while (h) {
            file_map_head* next = h->next;
            if (h->elem.kind == ELEM_SRC_DIR) {
                src_dir_fin((src_dir*)h);
            }
            else if (h->elem.kind == ELEM_SRC_LIB) {
                src_lib_fin((src_lib*)h);
            }
            else {
                assert(h->elem.kind == ELEM_SRC_FILE);
                src_file_fin((src_file*)h);
            }
            h = next;
        }
    }
    tfree(fm->table_start);
    ptrlist_fin(&fm->rt_src_libs);
    mutex_fin(&fm->lock);
    pool_fin(&fm->string_mem_pool);
    pool_fin(&fm->file_mem_pool);
}

static inline file_map_head**
file_map_get_head(file_map* fm, src_dir* parent, string name)
{
    ureg hash = fnv_hash_pointer(FNV_START_HASH, parent);
    hash = fnv_hash_string(hash, name);
    hash = fnv_fold(hash, fm->size_bits, fm->hash_mask);
    file_map_head** pos = fm->table_start + hash;
    while (true) {
        if (!*pos) return pos;
        if ((**pos).parent == parent && string_cmp(name, (**pos).name) == 0) {
            return pos;
        }
        pos = &(**pos).next;
    }
}

static inline int file_map_grow(file_map* fm)
{
    ureg capacity_new = 2 * (fm->table_end - fm->table_start);
    ureg size_new = capacity_new * sizeof(file_map_head*);

    file_map_head** fmnew = tmallocz(size_new);
    if (!fmnew) return ERR;
    file_map_head** old_start = fm->table_start;
    file_map_head** old_end = fm->table_end;
    fm->table_start = fmnew;
    fm->table_end = fmnew + capacity_new;
    fm->size_bits += 1;
    fm->hash_mask = (1 << fm->size_bits) - 1;
    fm->elem_count = 0;
    fm->grow_on_elem_count *= 2;

    for (file_map_head** i = old_start; i != old_end; i++) {
        file_map_head* next = *i;
        while (next) {
            file_map_head** tgt =
                file_map_get_head(fm, next->parent, next->name);
            *tgt = next;
            next = next->next;
            (**tgt).next = NULL;
        }
    }
    // PERF: we could give this memory region to the pools
    tfree(old_start);
    return OK;
}

static inline src_file* file_map_get_file_from_head(
    file_map* fm, file_map_head** head, src_dir* parent, string name)
{
    if (!head) return NULL;
    if (*head) {
        assert((**head).elem.kind == ELEM_SRC_FILE);
        return (src_file*)*head;
    }
    src_file* f = pool_alloc(&fm->file_mem_pool, sizeof(src_file));
    if (!f) return NULL;
    int r = src_file_init(f, fm, parent, name);
    if (r) return NULL;
    *head = (file_map_head*)f;
    fm->elem_count++;
    if (fm->elem_count == fm->grow_on_elem_count) {
        if (file_map_grow(fm)) {
            src_file_fin(f);
            return NULL;
        }
    }
    return f;
}

static inline src_dir* file_map_get_dir_from_head(
    file_map* fm, file_map_head** head, src_dir* parent, string name)
{
    if (!head) return NULL;
    if (*head) {
        assert((**head).elem.kind == ELEM_SRC_DIR);
        return (src_dir*)*head;
    }
    src_dir* dir = pool_alloc(&fm->file_mem_pool, sizeof(src_dir));
    if (!dir) return NULL;
    int r = src_dir_init(dir, fm, parent, name);
    if (r) return NULL;
    *head = (file_map_head*)dir;
    fm->elem_count++;
    if (fm->elem_count == fm->grow_on_elem_count) {
        if (file_map_grow(fm)) {
            src_dir_fin(dir);
            return NULL;
        }
    }
    return dir;
}

static inline src_lib* file_map_get_lib_from_head(
    file_map* fm, file_map_head** head, src_dir* parent, string name,
    bool is_dynamic)
{
    if (!head) return NULL;
    if (*head) {
        assert((**head).elem.kind == ELEM_SRC_LIB);
        return (src_lib*)*head;
    }
    src_lib* dir = pool_alloc(&fm->file_mem_pool, sizeof(src_lib));
    if (!dir) return NULL;
    int r = src_lib_init(dir, fm, parent, name, is_dynamic);
    if (r) return NULL;
    *head = (file_map_head*)dir;
    fm->elem_count++;
    if (fm->elem_count == fm->grow_on_elem_count) {
        if (file_map_grow(fm)) {
            src_lib_fin(dir);
            return NULL;
        }
    }
    return dir;
}

file_map_head** file_map_get_head_from_path_unlocked(
    file_map* fm, src_dir* parent_dir, string path, string* name,
    src_dir** parent)
{
    char* curr_start = path.start;
    char* curr_end = path.start;
    if (*curr_start == '/') {
        parent_dir = NULL; // TODO: a better way to do this?
    }
    while (true) {
        while (curr_end != path.end && *curr_end != '/') {
            curr_end++;
        }
        if (curr_end == path.end) {
            string x;
            x.start = curr_start;
            x.end = curr_end;
            *name = x;
            *parent = parent_dir;
            return file_map_get_head(fm, parent_dir, x);
        }
        string x;
        x.start = curr_start;
        x.end = curr_end;
        parent_dir = file_map_get_dir_from_head(
            fm, file_map_get_head(fm, parent_dir, x), parent_dir, x);
        if (!parent_dir) {
            return NULL;
        }
        curr_end++;
        curr_start = curr_end;
    }
}

src_file*
file_map_get_file_from_path(file_map* fm, src_dir* parent, string path)
{
    mutex_lock(&fm->lock);
    string name;
    src_dir* p;
    file_map_head** head =
        file_map_get_head_from_path_unlocked(fm, parent, path, &name, &p);
    src_file* f = file_map_get_file_from_head(fm, head, p, name);
    mutex_unlock(&fm->lock);
    return f;
}

src_dir* file_map_get_dir_from_path(file_map* fm, src_dir* parent, string path)
{
    mutex_lock(&fm->lock);
    string name;
    src_dir* p;
    file_map_head** head =
        file_map_get_head_from_path_unlocked(fm, parent, path, &name, &p);
    src_dir* f = file_map_get_dir_from_head(fm, head, p, name);
    mutex_unlock(&fm->lock);
    return f;
}

src_lib* file_map_get_lib_from_path(
    file_map* fm, src_dir* parent, string path, bool is_dynamic)
{
    mutex_lock(&fm->lock);
    string name;
    src_dir* p;
    file_map_head** head =
        file_map_get_head_from_path_unlocked(fm, parent, path, &name, &p);
    src_lib* l = file_map_get_lib_from_head(fm, head, p, name, is_dynamic);
    mutex_unlock(&fm->lock);
    return l;
}
