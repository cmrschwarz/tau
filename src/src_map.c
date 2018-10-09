#include "src_map.h"
#include "tauc.h"
#include "utils/pool.h"
#include "utils/math_utils.h"

#define LINE_STORE_MIN_LINES 32
static const ureg LINE_STORE_MIN_SIZE = 
    LINE_STORE_MIN_LINES * sizeof(ureg) + sizeof(line_store);

#define SRC_RANGE_LENGTH_BITS 7
static const ureg SRC_RANGE_MAX_LENGTH = (((ureg)1) << SRC_RANGE_LENGTH_BITS) - 1;
#define SRC_RANGE_START_BITS (REG_BITS - SRC_RANGE_LENGTH_BITS - 1)
static const ureg SRC_RANGE_MAX_START = (((ureg)1) << SRC_RANGE_START_BITS) - 1;
static const ureg SRC_RANGE_NEW_MAP_BIT = ((ureg)1) << (SRC_RANGE_START_BITS - 2);
static const ureg SRC_RANGE_EXTERN_BIT = ((ureg)1) << (SRC_RANGE_START_BITS - 1);

static inline int append_line_store(src_map* m, thread_context* tc, ureg size){
    line_store* s = pool_alloc(
        &tc->permmem,
        size
    );
    if(!s)return -1;
    s->end = ptradd(s, size);
    s->prev = m->last_line_store;
    m->last_line_store = s;
    m->last_line = ptradd(s, sizeof(line_store));
    return 0;
}
int src_map_init(src_map* m, thread_context* tc, bool is_paste_area){
    m->is_paste_area = is_paste_area;
    m->last_line_store = NULL;
    return append_line_store(m, tc, LINE_STORE_MIN_SIZE);
}

int src_map_fin(src_map* m){
    //nothing to do here for now
    return 0;
}

int src_map_add_line(src_map* m, thread_context* tc, ureg line_start){
    if(m->last_line + 1 > m->last_line_store->end){
        if(append_line_store(
            m, tc,
            ptrdiff(m->last_line_store->end, m->last_line_store) * 2
        )){
            return -1;
        } 
    }
    *m->last_line = line_start;
    m->last_line++;
    return 0;
}

src_range src_map_create_src_range(thread_context* tc, src_range_data d){
    //PERF: maybe allocate these somewhere else
    if(!d.map){
        ureg len = d.end - d.start;
        if(len > SRC_RANGE_MAX_LENGTH || d.start > SRC_RANGE_MAX_START){
            ureg* tgt = pool_alloc(&tc->permmem, sizeof(ureg) * 2);
            if(!tgt)return SRC_RANGE_INVALID;
            *tgt = d.start;
            *(tgt + 1) = d.end;
            return SRC_RANGE_EXTERN_BIT |  ( ((ureg)tgt) >> 2 );
        }
        else{
            return (d.start << SRC_RANGE_LENGTH_BITS) | len;
        }
    }
    else{
        src_map** tgt = pool_alloc(&tc->permmem, sizeof(ureg) * 3);
        if(!tgt)return SRC_RANGE_INVALID;
        *tgt = d.map;
        ureg* range = (void*)(tgt+1);
        *range = d.start;
        range++;
        *range = d.end;
        return SRC_RANGE_EXTERN_BIT | SRC_RANGE_NEW_MAP_BIT | ( ((ureg)tgt) >> 2 );
    }
}

src_range_data src_range_get_data(src_range r){
    src_range_data d;
    if(r & SRC_RANGE_EXTERN_BIT){
        if(r & SRC_RANGE_NEW_MAP_BIT){
            src_map** tgt = (void*)(r << 2); 
            d.map = *tgt;
            ureg* range = (ureg*)(tgt+1);
            d.start = *range;
            range++;
            d.end = *range;
        }
        else{
            d.map = NULL;
            ureg* tgt = (ureg*)(r << 2);
            d.start = *tgt;
            tgt++;
            d.end = *tgt;
        }
    }
    else{
        d.map = NULL;
        d.start = r >> SRC_RANGE_LENGTH_BITS;
        ureg len = (r & SRC_RANGE_MAX_LENGTH);
        d.end = d.start + len;
    }
    return d;
}