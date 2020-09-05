#pragma once
#include "types.h"
#include "string.h"
// the FNV-1a hash algorithm

#if UREG_MAX == U64_MAX
#define FNV_START_HASH 14695981039346656037ULL
#define FNV_HASH_PRIME 1099511628211ULL
#else
#define FNV_START_HASH 2166136261U
#define FNV_HASH_PRIME 16777619U
#endif

static inline ureg fnv_hash_str(ureg hash, const char* str)
{
    while (*str != '\0') {
        hash = (hash ^ *str) * FNV_HASH_PRIME;
        str++;
    }
    return hash;
}
static inline ureg fnv_hash_string(ureg hash, string s)
{
    while (s.start != s.end) {
        hash = (hash ^ *s.start) * FNV_HASH_PRIME;
        s.start++;
    }
    return hash;
}

static inline ureg fnv_hash_ureg(ureg hash, ureg val)
{
    char* ptr_bytes = (char*)&val;
    char* ptr_bytes_end = ptr_bytes + REG_BYTES;
    while (ptr_bytes != ptr_bytes_end) {
        hash = (hash ^ *ptr_bytes) * FNV_HASH_PRIME;
        ptr_bytes++;
    }
    return hash;
}

static inline ureg fnv_hash_pointer(ureg hash, void* ptr)
{
    return fnv_hash_ureg(hash, (ureg)ptr);
}

static inline ureg fnv_fold(ureg hash, ureg bitcount, ureg bitmask)
{
    // xor folding as described here:
    // www.isthe.com/chongo/tech/comp/fnv/index.html
    return ((hash >> bitcount) ^ hash) & bitmask;
}

static inline ureg fnv_fold_bc(ureg hash, ureg bitcount)
{
    return fnv_fold(hash, bitcount, (1 << bitcount) - 1);
}
