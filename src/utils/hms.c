#include "hms.h"
#include "string.h"
#include "stdio.h"

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

void* hms_alloc(ureg size){
    void* tgt;
#   if defined(__linux__)
        tgt = mmap(NULL, size, PROT_READ | PROT_WRITE,
                   MAP_PRIVATE | MAP_ANONYMOUS | MAP_UNINITIALIZED, -1, 0);
        if(tgt == MAP_FAILED)return NULL;
#   else    //TODO: windows
        tgt = malloc(size);
        if (tgt == NULL) return NULL;
#   endif
    return tgt;
}

void hms_free(void* pos, ureg size){
#   if defined(__linux__)
        munmap(pos, size);
#   else
        free(pos);
#   endif
}

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

int hms_init_with_n_bits(hms* h, ureg n){
    ureg size = (ureg)1 << n;
    h->map = hms_alloc(size * sizeof(hms_node));
    if(!h->map)return -1;
    h->size_bits = n;
    h->hash_mask = ((ureg)1 << n) - 1;
    h->elem_count = 0;
    h->grow_on_elem_count = size / 4 * 3;
    h->map_end = h->map + size;
    hms_node* z = h->map;
    while(z!=h->map_end){
        z->key = NULL;
        z++;
    }
    return 0;
}
int hms_init(hms* h){
    //3 bits hash for eight slots
    return hms_init_with_n_bits(h, 3);
}
void hms_fin(hms* h){
    hms_free(h->map, (h->map_end - h->map) * sizeof(hms_node));
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
        if(strcmp(n->key, key) == 0)break;
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
    hms_node* old = h->map;
    hms_node* old_end = h->map_end;
    ureg size_old = old_end - old;
    ureg size_new = size_old * 2;
    h->map = hms_alloc(size_new * sizeof(hms_node));
    if (h->map == NULL){
        h->map  = old;
        return -1;
    }
    h->map_end = h->map + size_new;
    h->size_bits++;
    h->hash_mask = ((ureg)1 << h->size_bits) - 1;
     //keep wasted space below 25%
    h->grow_on_elem_count = size_old + size_old / 2;
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
    hms_free(old, size_old * sizeof(hms_node));
    return 0;
}
