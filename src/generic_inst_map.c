#include "generic_inst_map.h"
#include "utils/threading.h"
#include "ast.h"
#include "utils/fnv_hash.h"
#include "utils/panic.h"

int gim_init(generic_inst_map* g, ast_node_kind instances_kind)
{
    g->bitcount = 2;
    ureg initial_cap = 1 << g->bitcount;
    g->instances = tmallocz(sizeof(symbol*) * initial_cap);
    if (!g->instances) return ERR;
    if (mutex_init(&g->lock)) {
        tfree(g->instances);
    }
    g->count = 0;
    g->instances_kind = instances_kind;
    return OK;
}

void gim_fin(generic_inst_map* g)
{
    mutex_fin(&g->lock);
    tfree(g->instances);
}

void gim_lock(generic_inst_map* g)
{
    mutex_lock(&g->lock);
}
void gim_unlock(generic_inst_map* g)
{
    mutex_unlock(&g->lock);
}

ureg gim_hash(ast_elem** args, ureg arg_count)
{
    ureg hash = FNV_START_HASH;
    for (ast_elem** i = args; i != args + arg_count; i++) {
        hash = fnv_hash_pointer(hash, *i);
    }
    return hash;
}
static inline void generic_inst_get_generic_args(
    symbol* s, sym_param_generic_inst** args, ureg* arg_count)
{
    switch (s->node.kind) {
        case SC_STRUCT_GENERIC_INST:
            *args = ((sc_struct_generic_inst*)s)->generic_args;
            *arg_count = ((sc_struct_generic_inst*)s)->generic_arg_count;
            break;
        case SC_FUNC_GENERIC_INST:
            *args = ((sc_func_generic_inst*)s)->generic_args;
            *arg_count = ((sc_func_generic_inst*)s)->generic_arg_count;
            break;
        default: panic("compiler bug"); break;
    }
}

int gim_grow(generic_inst_map* g, ureg* cap_new_p)
{
    ureg bc_new = g->bitcount + 1;
    ureg cap_new = 1 << bc_new;
    ureg mask_new = cap_new - 1;
    symbol** table_new = tmallocz(cap_new * sizeof(symbol*));
    if (!table_new) return ERR;
    ureg cap_old = 1 << g->bitcount;
    for (ureg i = 0; i < cap_old; i++) {
        symbol* s = g->instances[i];
        if (!s) continue;
        sym_param_generic_inst* args;
        ureg arg_count;
        assert(g->instances_kind == s->node.kind);
        generic_inst_get_generic_args(s, &args, &arg_count);
        ureg idx = fnv_fold(gim_hash(args, arg_count), bc_new, mask_new);
        while (table_new[idx]) {
            idx++;
            if (idx == cap_new) idx = 0;
        }
        table_new[idx] = s;
    }
    tfree(g->instances);
    g->instances = table_new;
    g->bitcount = bc_new;
    *cap_new_p = cap_new;
    return OK;
}

symbol** gim_get_element(generic_inst_map* g, ast_elem** args, ureg arg_count)
{
    ureg hash = gim_hash(args, arg_count);
    ureg cap = 1 << g->bitcount;
    ureg idx = fnv_fold(hash, g->bitcount, cap - 1);
    while (true) {
        symbol** s = &g->instances[idx];
        if (!*s) {
            g->count++;
            if (g->count + (g->count >> 1) < cap) return s;
            if (gim_grow(g, &cap)) return NULL;
            idx = fnv_fold(hash, g->bitcount, cap - 1);
        }
        else {
            sym_param_generic_inst* s_args;
            ureg s_arg_count;
            assert(g->instances_kind == (**s).node.kind);
            generic_inst_get_generic_args(*s, &s_args, &s_arg_count);
            if (s_arg_count == arg_count) {
                bool diff = false;
                for (ureg i = 0; i != arg_count; i++) {
                    // TODO: better value comparison for non types!
                    if (s_args->value != args[i]) {
                        diff = true;
                        break;
                    }
                }
                if (!diff) return s;
            }
            idx++;
            if (idx == cap) idx = 0;
        }
    }
}
sc_func_generic_inst**
gim_get_func(generic_inst_map* g, ast_elem** args, ureg arg_count)
{
    assert(g->instances_kind == SC_FUNC_GENERIC_INST);
    return (sc_func_generic_inst**)gim_get_element(g, args, arg_count);
}

sc_struct_generic_inst**
gim_get_struct(generic_inst_map* g, ast_elem** args, ureg arg_count)
{
    assert(g->instances_kind == SC_STRUCT_GENERIC_INST);
    return (sc_struct_generic_inst**)gim_get_element(g, args, arg_count);
}
