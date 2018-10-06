#include "hms.h"
#include "string.h"
#include "stdio.h"
#include "math_utils.h"
#ifdef __linux__
#   include <linux/mman.h>
#   include <sys/mman.h>
#elif defined(_WIN32) || defined(_WIN16)
#   include <windows.h>
#else
#   include <malloc.h>
#endif

// we use this value to indicate a removed key so hms_get doesn't return
// on nodes that were removed and are potentially followed by valid content
static const char* TOMBSTONE_KEY = "";

static inline ureg hms_hash_pos(hms* h, const char* key){
    //this uses the FNV-1a algorithm
    //TODO: search for a better one :D
#if UREG_MAX == U64_MAX
    ureg hash = 14695981039346656037ULL;
    const ureg prime = 1099511628211ULL;
#else
    ureg hash = 2166136261U;
    const ureg prime = 16777619U;
#endif
    while(*key != '\0'){
        hash = (hash ^ *key) * prime;
        key++;
    }
    //xor folding like described here:
    //www.isthe.com/chongo/tech/comp/fnv/index.html
    hash = ((hash >> h->size_bits) ^ hash) & h->hash_mask;
    return hash;
}

int hms_init_with_capacity(hms* h, ureg capacity, thread_allocator* tal){
    memblock b;
    //THINK whether allocating without z and then zeroing the keys is faster 
    if(tal_allocz(tal, capacity * sizeof(hms_node), &b)) return -1;
    h->tal = tal;
    //this limits the "used size" to a power of 2
    //elements after that size will be used for colliding elements,
    //but the hash function will not give out their indices directly
    h->size_bits = ulog2(capacity);    
    h->map = b.start;
    h->map_end = b.end;
    h->hash_mask = (1 << h->size_bits) - 1;
    h->elem_count = 0;
    h->grow_on_elem_count = capacity / 4 * 3;
    return 0;
}
int hms_init(hms* h, thread_allocator* tal){
    return hms_init_with_capacity(
        h,
        allocator_get_segment_size() / sizeof(hms_node),
        tal
    );
}
void hms_fin(hms* h){
    memblock b;
    b.start = h->map;
    b.end = h->map_end;
    tal_free(h->tal, &b);;
}
int hms_set(hms* h, const char* key, void* value){
    if(h->elem_count == h->grow_on_elem_count){
        if(hms_grow(h)) return -1;
    }
    h->elem_count++;
    ureg pos = hms_hash_pos(h, key);
    hms_node* n = &h->map[pos];
    while(n->key != NULL){
        if(n->key == TOMBSTONE_KEY)break;
        n++;
        if(n == h->map_end)n = h->map;
    }
    n->key = key;
    n->value = value;
    return 0;
}

void* hms_get(hms* h, const char* key){
    ureg pos = hms_hash_pos(h, key);
    hms_node* n = &h->map[pos];
    while(true){
        if(n->key == NULL)return NULL;
        if(strcmp(n->key, key) == 0){
            //if we don't want "" to be a valid key, we
            //can remove this check for performance
            if(n->key != TOMBSTONE_KEY)break;
        }
        n++;
        if(n == h->map_end) n = h->map;
    }
    return n->value;
}
hms_node* hms_acquire(hms* h, const char* key){
    ureg pos = hms_hash_pos(h, key);
    hms_node* n = &h->map[pos];
    while(true){
        if(n->key == NULL){
            n->key = key;
            n->value = NULL;
            return n;
        }
        if(strcmp(n->key, key) == 0)break;
        n++;
        if(n == h->map_end) n = h->map;
    }
    return n;
}
void* hms_remove(hms* h, const char* key){
    ureg pos = hms_hash_pos(h, key);
    hms_node* n = &h->map[pos];
    while(true){
        if(n->key == NULL)return NULL;
        if(strcmp(n->key, key) == 0)break;
        n++;
        if(n == h->map_end) n = h->map;
    }
    h->elem_count--;
    n->key = TOMBSTONE_KEY;
    return n->value;
}

int hms_grow(hms* h){
    memblock b;
    if(tal_alloc(h->tal, ptrdiff(h->map_end, h->map) * 2, &b)) return -1;
    hms_node* old = h->map;
    hms_node* old_end = h->map_end;
    h->map = b.start;
    h->map_end = b.end;
    ureg size = ptrdiff(b.end, b.start);
    h->size_bits = ulog2(size);
    h->hash_mask = size - 1;
    h->grow_on_elem_count = size / 4 * 3;
    hms_node* z = h->map;
    while(z!=h->map_end){
        z->key = NULL;
        z++;
    }
    z = old;
    while(z != old_end){
        if(z->key != NULL && z->key != TOMBSTONE_KEY){
            ureg pos = hms_hash_pos(h, z->key);
            hms_node* n = &h->map[pos];
            while(n->key != NULL){
                n++;
                if(n == h->map_end)n = h->map;
            }
            n->key = z->key;
            n->value = z->value;
        }
        z++;
    }
    b.start = old;
    b.end = old_end;
    tal_free(h->tal, &b);
    return 0;
}
