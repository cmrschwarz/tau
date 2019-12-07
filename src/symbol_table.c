#include "symbol_table.h"

#include "ast.h"
#include "stddef.h"
#include "utils/allocator.h"
#include "utils/error.h"
#include "utils/fnv_hash.h"
#include "utils/math_utils.h"
#include "file_map.h"
#include "utils/zero.h"
#include <assert.h>
#include "utils/error.h"
#include "tauc.h"
#define USING_BIT (((ureg)1) << (REG_BITS - 1))

symbol_table* GLOBAL_SYMTAB = NULL;

int symbol_table_init(
    symbol_table** tgt, ureg decl_count, ureg using_count, bool force_unique,
    ast_elem* owning_node, ureg ppl)
{
    assert(*tgt == NULL);
    if (!force_unique && decl_count == 0 && using_count == 0) {
        *tgt = NULL;
    }
    symbol_table* st = tmalloc(sizeof(symbol_table));
    if (!st) return ERR;
    st->pp_symtab = NULL;
    st->decl_count = decl_count;
    if (decl_count) {
        ureg table_capacity = (decl_count == 1) ? 2 : ceil_to_pow2(decl_count);
        st->hash_mask = table_capacity - 1;
        st->table = tmallocz(table_capacity * sizeof(symbol*));
        if (!st->table) {
            tfree(st);
            return ERR;
        }
        memset(st->table, 0, table_capacity * sizeof(symbol*));
    }
    else {
        if (force_unique) st->decl_count = UREG_MAX;
        st->hash_mask = 0;
        st->table = (symbol**)NULL_PTR_PTR;
    }
    st->owning_node = owning_node;
    st->ppl = ppl;
    *tgt = st;

    if (using_count != 0) {
        ureg usings_size =
            using_count * sizeof(symbol_table*) + sizeof(usings_table);
        st->usings = tmalloc(usings_size);
        if (!st->usings) {
            if (decl_count) tfree(st->table);
            tfree(st);
            return ERR;
        }
        for (ureg i = 0; i < AM_ENUM_ELEMENT_COUNT; i++) {
            st->usings->using_ends[i] = (symbol_table**)(st->usings + 1);
        }
    }
    else {
        st->usings = NULL;
    }

    return OK;
}
static inline ureg symbol_table_get_capacity(symbol_table* st)
{
    return st->hash_mask ? st->hash_mask + 1 : 0;
}
void symbol_table_insert_using(
    symbol_table* st, access_modifier am, ast_node* used_node,
    symbol_table* used_symtab)
{
    while (st->decl_count == UREG_MAX) st = st->parent;
    // reverse the am so it goes from public to unspecified upwars in memory
    am = AM_ENUM_ELEMENT_COUNT - am;
    usings_table* ut = st->usings;
    ureg usings_size = st->usings->usings_count * sizeof(symbol_table*);
    assert(st->usings);
    for (int i = AM_ENUM_ELEMENT_COUNT - 1; i != am; i--) {
        *ut->using_ends[i] = *ut->using_ends[i - 1];
        *(ast_node**)ptradd(ut->using_ends[i], usings_size) =
            *(ast_node**)ptradd(ut->using_ends[i - 1], usings_size);
        ut->using_ends[i]++;
    }
    *ut->using_ends[am] = used_symtab;
    *(ast_node**)ptradd(ut->using_ends[am], usings_size) = used_node;
    ut->using_ends[am]++;
}

void symbol_table_fin(symbol_table* st)
{
    if (st != NULL) {
        symbol_table_fin(st->pp_symtab);
        // avoid throwing of our allocation counter
        if (st->usings) tfree(st->usings);
        // avoid freeing NULL_PTR_PTR
        if (st->hash_mask != 0) tfree(st->table);
        tfree(st);
    }
}
int symbol_table_amend(symbol_table* st, ureg decl_count, ureg usings)
{
    while (st->decl_count == UREG_MAX) st = st->parent;
    st->decl_count += decl_count;
    if (st->decl_count > symbol_table_get_capacity(st)) {
        ureg cap_new = (st->decl_count == 1) ? 2 : ceil_to_pow2(st->decl_count);
        symtab_it it;
        if (st->hash_mask) it = symtab_it_make(st);
        symbol** table_new = tmallocz(cap_new * sizeof(symbol*));
        if (!table_new) return ERR;
        ureg mask_old = st->hash_mask;
        st->hash_mask = cap_new - 1;
        symbol** table_old = st->table;
        st->table = table_new;
        if (mask_old) {
            for (symbol* s = symtab_it_next(&it); s; s = symtab_it_next(&it)) {
                symbol** r = symbol_table_insert(st, s);
                assert(r == NULL);
            }
            tfree(table_old);
        }
    }
    if (usings) {
        assert(false); // TODO
    }
    return OK;
}
symbol** symbol_table_find_insert_position(symbol_table* st, char* name)
{
    while (st->decl_count == UREG_MAX) st = st->parent;
    ureg hash = fnv_hash_str(FNV_START_HASH, name) & st->hash_mask;
    symbol** tgt = ptradd(st->table, hash * sizeof(symbol*));
    while (*tgt) {
        if (strcmp((**tgt).name, name) == 0) return tgt;
        tgt = (symbol**)&(**tgt).next;
    }
    return tgt;
}
symbol** symbol_table_insert(symbol_table* st, symbol* s)
{
    while (st->decl_count == UREG_MAX) st = st->parent;
    ureg hash = fnv_hash_str(FNV_START_HASH, s->name) & st->hash_mask;
    symbol** tgt = ptradd(st->table, hash * sizeof(symbol*));
    while (*tgt) {
        if (strcmp((**tgt).name, s->name) == 0) return tgt;
        tgt = (symbol**)&(**tgt).next;
    }
    *tgt = s;
    s->next = NULL;
    return NULL;
}
symbol** symbol_table_lookup_limited(
    symbol_table* st, ureg ppl, access_modifier am, symbol_table* stop_at,
    const char* s)
{
    // TODO: test if this makes perf better or worse
    // while (st->decl_count == UREG_MAX) st = st->parent;
    ureg hash = fnv_hash_str(FNV_START_HASH, s);
    do {
        symbol_table* curr_st = st;
        while (curr_st->ppl < ppl) {
            if (!curr_st->pp_symtab) break;
            curr_st = curr_st->pp_symtab;
        }
        while (true) {
            // PERF: get rid of this check somehow
            if (curr_st->decl_count != 0) {
                ureg idx = hash & curr_st->hash_mask;
                symbol** tgt =
                    (symbol**)ptradd(curr_st->table, idx * sizeof(symbol*));
                while (*tgt) {
                    if (strcmp((**tgt).name, s) == 0) {
                        return tgt;
                    }
                    tgt = (symbol**)&(**tgt).next;
                }
            }
            if (st->usings) {
                symbol_table** i = (symbol_table**)(st->usings + 1);
                symbol_table** end =
                    st->usings->using_ends[AM_ENUM_ELEMENT_COUNT - am];
                // for pub usings we can look at their pub and prot symbols
                symbol** res = symbol_table_lookup(*i, AM_PROTECTED, ppl, s);
                if (res) return res;
                i++;
                // for prot to unspecified usings we can look at their pub
                // symbols and if they are used directly by us at ther protected
                // symbols
                access_modifier tgt_am =
                    (am < AM_PROTECTED) ? AM_PROTECTED : AM_PUBLIC;
                while (i != end) {
                    res = symbol_table_lookup(*i, tgt_am, ppl, s);
                    if (res) return res;
                }
            }
            if (!curr_st->parent ||
                curr_st->owning_node != curr_st->parent->owning_node) {
                break;
            }
            curr_st = curr_st->parent;
        }
        st = st->parent;
    } while (st != stop_at);
    return NULL;
}
symbol** symbol_table_lookup(
    symbol_table* st, ureg ppl, access_modifier am, const char* s)
{
    return symbol_table_lookup_limited(st, ppl, am, NULL, s);
}
void symbol_table_inc_decl_count(symbol_table* st)
{
    // while (st->decl_count == UREG_MAX) st = st->parent;
    // st->decl_count++;
    // assert(st->decl_count <= symbol_table_get_capacity(st));
}
src_map* symbol_table_get_smap(symbol_table* st)
{
    // TODO: this can happen if neither the func nor the osc have a single
    // symbol
    assert(st->owning_node->kind != ELEM_MDG_NODE);
    src_map* smap = src_range_get_smap(((ast_node*)st->owning_node)->srange);
    if (smap) return smap;
    // because pp tables parent the postprocessing table
    // we naturally transition out of pp levels
    if (st->parent) return symbol_table_get_smap(st->parent);
    return NULL;
}

int init_root_symtab(symbol_table** root_symtab)
{
    // to avoid an assertion (which makes sense for all other cases)
    *root_symtab = NULL;
    if (symbol_table_init(root_symtab, PRIMITIVE_COUNT + 1, 0, true, NULL, 0))
        return ERR;
    (**root_symtab).parent = NULL;
    for (int i = 0; i < PRIMITIVE_COUNT; i++) {
        if (symbol_table_insert(*root_symtab, (symbol*)&PRIMITIVES[i])) {
            fin_root_symtab(*root_symtab);
            return ERR;
        }
        PRIMITIVES[i].sym.declaring_st = *root_symtab;
    }
    (**root_symtab).decl_count = PRIMITIVE_COUNT;
    return OK;
}
void fin_root_symtab(symbol_table* root_symtab)
{
    symbol_table_fin(root_symtab);
}
void symtab_it_begin(symtab_it* stit, symbol_table* st)
{
    stit->pos = st->table;
    stit->subpos = *stit->pos;
    stit->end = stit->pos + symbol_table_get_capacity(st);
    if (stit->pos == stit->end) {
        stit->pos--;
        stit->subpos = NULL;
    }
}
symtab_it symtab_it_make(symbol_table* st)
{
    symtab_it it;
    symtab_it_begin(&it, st);
    return it;
}
symbol* symtab_it_next(symtab_it* stit)
{
    while (true) {
        if (stit->subpos) {
            symbol* res = stit->subpos;
            stit->subpos = stit->subpos->next;
            return res;
        }
        stit->pos++;
        if (stit->pos == stit->end) {
            stit->pos--;
            return NULL;
        }
        stit->subpos = *stit->pos;
    }
}
