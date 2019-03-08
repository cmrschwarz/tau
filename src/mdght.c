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

int mdght_init_with_capacity(mdght* h, ureg capacity, thread_allocator* tal)
{
    memblock b;
    // we alloc zero initialized so all values are NULL pointers
    if (tal_allocz(tal, capacity * sizeof(mdg_node*), &b)) return -1;
    h->tal = tal;
    // this limits the "used size" to a power of 2
    // elements after that size will be used for colliding elements,
    // but the hash function will not give out their indices directly
    h->size_bits = ulog2(capacity);
    h->table_start = (mdg_node**)b.start;
    h->table_end = (mdg_node**)b.end;
    h->hash_mask = (1 << h->size_bits) - 1;
    h->elem_count = 0;
    h->grow_on_elem_count = capacity / 4 * 3; // grow on 75 %
    return 0;
}

int mdght_init(mdght* h, thread_allocator* tal)
{
    return mdght_init_with_capacity(h, PAGE_SIZE / sizeof(mdg_node*), tal);
}
void mdght_fin(mdght* h)
{
    memblock b;
    b.start = h->table_start;
    b.end = h->table_end;
    tal_free(h->tal, &b);
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
    memblock b;
    if (tal_allocz(h->tal, ptrdiff(h->table_end, h->table_start) * 2, &b))
        return -1;
    mdg_node** old = h->table_start;
    mdg_node** old_end = h->table_end;
    h->table_start = (mdg_node**)b.start;
    h->table_end = (mdg_node**)b.end;
    ureg size = ptrdiff(b.end, b.start) / sizeof(mdg_node**);
    h->size_bits = ulog2(size);
    h->hash_mask = size - 1;
    h->grow_on_elem_count = size / 4 * 3;
    mdg_node** z = old;
    h->elem_count = 0;
    while (z != old_end) {
        if (*z != NULL && *z != &tombstone_node) {
            mdght_insert(h, *z);
        }
        z++;
    }
    b.start = old;
    b.end = old_end;
    tal_free(h->tal, &b);
    return 0;
}
