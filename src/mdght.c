#include "mdght.h"
#include "mdg.h"
#include "string.h"
#include "utils/fnv_hash.h"
#include "utils/math_utils.h"
static mdg_node tombstone_node = {0};

ureg mdght_get_hash(mdg_node* parent, const char* str)
{
    return fnv_hash_str(fnv_hash_pointer(FNV_START_HASH, parent), str);
}

ureg mdght_get_hash_str(mdg_node* parent, string str)
{
    return fnv_hash_string(fnv_hash_pointer(FNV_START_HASH, parent), str);
}

static inline ureg mdght_fold(mdght* h, ureg hash)
{
    return fnv_fold(hash, h->size_bits, h->hash_mask);
}

int mdght_init_with_capacity(mdght* h, ureg capacity)
{
    // we alloc zero initialized so all values are NULL pointers
    ureg size = capacity * sizeof(mdg_node*);
    h->table_start = (mdg_node**)tmallocz(size);
    if (!h->table_start) return ERR;
    h->table_end = (mdg_node**)ptradd(h->table_start, size);

    // this limits the "used size" to a power of 2
    // elements after that size will be used for colliding elements,
    // but the hash function will not give out their indices directly
    h->size_bits = ulog2(capacity);

    h->hash_mask = (1 << h->size_bits) - 1;
    h->elem_count = 0;
    h->grow_on_elem_count = capacity / 4 * 3; // grow on 75 %
    return OK;
}

int mdght_init(mdght* h)
{
    return mdght_init_with_capacity(
        h, plattform_get_page_size() / sizeof(mdg_node*));
}
void mdght_fin(mdght* h)
{
    tfree(h->table_start);
}
void mdght_fin_contained_nodes(mdght* h)
{
    for (mdg_node** i = h->table_start; i != h->table_end; i++) {
        if (*i != NULL && *i != &tombstone_node) {
            mdg_node_fin(*i);
        }
    }
}
mdg_node** mdght_insert_ph(mdght* h, ureg phash, mdg_node* n)
{
    if (h->elem_count == h->grow_on_elem_count) {
        if (mdght_grow(h)) return NULL;
    }
    h->elem_count++;
    ureg pos = mdght_fold(h, phash);
    mdg_node** ni = &h->table_start[pos];
    while (*ni != NULL) {
        ni++;
        // we realloc at a certain fillpercentage
        // so we will always find a space -> no infinite loop
        if (ni == h->table_end) ni = h->table_start;
    }
    *ni = n;
    return ni;
}
mdg_node** mdght_insert(mdght* h, mdg_node* n)
{
    return mdght_insert_ph(h, mdght_get_hash(n->parent, n->name), n);
}
mdg_node** mdght_insert_at(mdght* h, ureg pos, mdg_node* n)
{
    if (h->elem_count == h->grow_on_elem_count) {
        if (mdght_grow(h)) return NULL;
    }
    h->elem_count++;
    h->table_start[pos] = n;
    return &h->table_start[pos];
}

mdg_node* mdght_get_ph(mdght* h, ureg hash, mdg_node* parent, const char* name)
{
    ureg pos = mdght_fold(h, hash);
    mdg_node** n = &h->table_start[pos];
    while (*n != NULL) {
        if ((**n).parent == parent && strcmp((**n).name, name) == 0)
            return (*n);
        n++;
        if (n == h->table_end) n = h->table_start;
    }
    return NULL;
}
mdg_node* mdght_get(mdght* h, mdg_node* parent, const char* name)
{
    return mdght_get_ph(h, mdght_get_hash(parent, name), parent, name);
}
mdg_node**
mdght_get_str_raw_ph(mdght* h, ureg hash, mdg_node* parent, string name)
{
    ureg pos = mdght_fold(h, hash);
    mdg_node** n = &h->table_start[pos];
    while (*n != NULL) {
        if ((**n).parent == parent && string_cmp_cstr(name, (**n).name) == 0)
            return n;
        n++;
        if (n == h->table_end) n = h->table_start;
    }
    return n;
}
mdg_node* mdght_get_str_ph(mdght* h, ureg hash, mdg_node* parent, string name)
{
    return *mdght_get_str_raw_ph(h, hash, parent, name);
}
mdg_node* mdght_get_str(mdght* h, mdg_node* parent, string name)
{
    return *mdght_get_str_raw_ph(
        h, mdght_get_hash_str(parent, name), parent, name);
}

mdg_node*
mdght_remove_ph(mdght* h, ureg hash, mdg_node* parent, const char* name)
{
    ureg pos = mdght_fold(h, hash);
    mdg_node** n = &h->table_start[pos];
    while (*n == NULL) {
        if ((**n).parent == parent && strcmp((**n).name, name) == 0) {
            h->elem_count--;
            *n = &tombstone_node;
            return *n;
        }
        n++;
        if (n == h->table_end) n = h->table_start;
    }
    return NULL;
}
mdg_node* mdght_remove(mdght* h, mdg_node* parent, const char* name)
{
    return mdght_remove_ph(h, mdght_get_hash(parent, name), parent, name);
}

mdg_node* mdght_remove_node_ph(mdght* h, ureg hash, mdg_node* n)
{
    ureg pos = mdght_fold(h, hash);
    mdg_node** ni = &h->table_start[pos];
    while (*ni != n) {
        if (*ni == NULL) return NULL;
        ni++;
        if (ni == h->table_end) ni = h->table_start;
    }
    h->elem_count--;
    n = *ni;
    *ni = &tombstone_node;
    return n;
}
mdg_node* mdght_remove_node(mdght* h, mdg_node* n)
{
    return mdght_remove_node_ph(h, mdght_get_hash(n->parent, n->name), n);
}

int mdght_grow(mdght* h)
{
    ureg size_new = ptrdiff(h->table_end, h->table_start) * 2;
    void* memnew = tmallocz(size_new);
    if (!memnew) return -1;
    mdg_node** old = h->table_start;
    mdg_node** old_end = h->table_end;
    h->table_start = (mdg_node**)memnew;
    h->table_end = (mdg_node**)ptradd(memnew, size_new);
    ureg capacity_new = size_new / sizeof(mdg_node**);
    h->size_bits = ulog2(capacity_new);
    h->hash_mask = capacity_new - 1;
    h->grow_on_elem_count = capacity_new / 4 * 3;
    mdg_node** z = old;
    h->elem_count = 0;
    while (z != old_end) {
        if (*z != NULL && *z != &tombstone_node) {
            mdght_insert(h, *z);
        }
        z++;
    }
    tfree(old);
    return 0;
}
