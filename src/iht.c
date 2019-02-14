#include "iht.h"
#include "string.h"
#include "utils/math_utils.h"

#if UREG_MAX == U64_MAX
    #define IHT_START_HASH 14695981039346656037ULL
    #define IHT_HASH_PRIME 1099511628211ULL
#else
    #define IHT_START_HASH 2166136261U
    #define IHT_HASH_PRIME 16777619U
#endif

static named_ast_node tombstone_node = {0};

static inline ureg iht_hash_string(ureg hash, const char* key){
    //this uses the FNV-1a algorithm
    //TODO: search for a better one :D
    while(*key != '\0'){
        hash = (hash ^ *key) * IHT_HASH_PRIME;
        key++;
    }
    return hash;
}

static inline ureg iht_hash_pointer(ureg hash, void* parent){
    char* rape_pointer = (char*)&parent; 
    char* rape_end = rape_pointer + REG_BYTES;
    while(rape_pointer != rape_end){
        hash = (hash ^ *rape_pointer) * IHT_HASH_PRIME;
        rape_pointer++;
    }
    return hash;
}

parent_hash iht_get_parent_hash(named_ast_node* parent){
    return iht_hash_pointer(IHT_START_HASH, parent);
}


static inline ureg iht_hashpos(iht* h, parent_hash phash, const char* name){
    iht_hash_string(phash, name);
    //xor folding like described here:
    //www.isthe.com/chongo/tech/comp/fnv/index.html
    phash = ((phash >> h->size_bits) ^ phash) & h->hash_mask;
    return phash;
}


int iht_init_with_capacity(iht* h, ureg capacity, thread_allocator* tal){
    memblock b;
    //we alloc zero initialized so all values are NULL pointers 
    if(tal_allocz(tal, capacity * sizeof(named_ast_node*), &b)) return -1;
    h->tal = tal;
    //this limits the "used size" to a power of 2
    //elements after that size will be used for colliding elements,
    //but the hash function will not give out their indices directly
    h->size_bits = ulog2(capacity);    
    h->table_start = (named_ast_node**)b.start;
    h->table_end = (named_ast_node**)b.end;
    h->hash_mask = (1 << h->size_bits) - 1;
    h->elem_count = 0;
    h->grow_on_elem_count = capacity / 4 * 3; // grow on 75 %
    return 0;
}

int iht_init(iht* h, thread_allocator* tal){
    return iht_init_with_capacity(
        h,
        allocator_get_segment_size() / sizeof(named_ast_node*),
        tal
    );
}
void iht_fin(iht* h){
    memblock b;
    b.start = h->table_start;
    b.end = h->table_end;
    tal_free(h->tal, &b);;
}
int iht_insert_pph(iht* h, parent_hash phash, named_ast_node* val){
    if(h->elem_count == h->grow_on_elem_count){
        if(iht_grow(h)) return -1;
    }
    h->elem_count++;
    ureg pos = iht_hashpos(h, phash, val->name);
    named_ast_node** n = &h->table_start[pos];
    while(*n != NULL){
        n++;
        // we realloc at a certain fillpercentage
        // so we will always find a space -> no infinite loop 
        if(n == h->table_end)n = h->table_start;
    }
    *n = val;
    return 0;
}
int iht_insert(iht* h, named_ast_node* val){
    return iht_insert_pph(h, iht_get_parent_hash(val->parent), val);
}

named_ast_node* iht_get_pph(iht* h, parent_hash phash, named_ast_node* parent, const char* name){
    ureg pos = iht_hashpos(h, phash, name);
    named_ast_node** n = &h->table_start[pos];
    while(*n != NULL){
        if(*n == NULL)return NULL;
        if((**n).parent == parent && strcmp((**n).name, name) == 0) return (*n);
        n++;
        if(n == h->table_end) n = h->table_start;
    }
    return NULL;
}
named_ast_node* iht_get(iht* h, named_ast_node* parent, const char* name){
    return iht_get_pph(h, iht_get_parent_hash(parent), parent, name); 
}

named_ast_node* iht_remove_pph(iht* h, parent_hash phash, named_ast_node* parent, const char* name){
    ureg pos = iht_hashpos(h, phash, name);
    named_ast_node** n = &h->table_start[pos];
    while(*n == NULL){
        if((**n).parent == parent && strcmp((**n).name, name) == 0){
            h->elem_count--;
            *n = &tombstone_node;
            return *n;
        }
        n++;
        if(n == h->table_end) n = h->table_start;
    }
    return NULL;
}
named_ast_node* iht_remove(iht* h, named_ast_node* parent, const char* name){
    return iht_remove_pph(h, iht_get_parent_hash(parent), parent, name); 
}

named_ast_node* iht_remove_node_pph(iht* h, parent_hash phash, named_ast_node* node){
    ureg pos = iht_hashpos(h, phash, node->name);
    named_ast_node** n = &h->table_start[pos];
    while(*n != node){
        if(*n == NULL)return NULL;
        n++;
        if(n == h->table_end) n = h->table_start;
    }
    h->elem_count--;
    *n = &tombstone_node;
    return *n;
}
named_ast_node* iht_remove_node(iht* h, named_ast_node* node){
    return iht_remove_node_pph(h, iht_get_parent_hash(node->parent), node); 
}

int iht_grow(iht* h){
    memblock b;
    if(tal_allocz(h->tal, ptrdiff(h->table_end, h->table_start) * 2, &b)) return -1;
    named_ast_node** old = h->table_start;
    named_ast_node** old_end = h->table_end;
    h->table_start = (named_ast_node**)b.start;
    h->table_end = (named_ast_node**)b.end;
    ureg size = ptrdiff(b.end, b.start) / sizeof(named_ast_node**);
    h->size_bits = ulog2(size);
    h->hash_mask = size - 1;
    h->grow_on_elem_count = size / 4 * 3;
    named_ast_node** z = old;
    h->elem_count=0;
    while(z != old_end){
        if(*z != NULL && *z != &tombstone_node){
            iht_insert(h, *z);
        }
        z++;
    }
    b.start = old;
    b.end = old_end;
    tal_free(h->tal, &b);
    return 0;
}
