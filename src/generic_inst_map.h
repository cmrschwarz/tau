#pragma once
#include "utils/types.h"
#include "utils/rwslock.h"

typedef struct sc_func_generic_inst_s sc_func_generic_inst;
typedef struct expr_call_s expr_call;
// gim: generic instance map
typedef struct generic_inst_map_s {
    ureg mask;
    ureg count;
    rwslock lock;
    sc_func_generic_inst** instances;
} generic_inst_map;

int gim_init(generic_inst_map* g);
