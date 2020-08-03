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
#define EMPTY_TABLE ((symbol**)&NULL_BYTES[0])
#define META_TABLE ((symbol**)&NULL_BYTES[1])
static inline bool symbol_table_should_be_meta(ast_elem* owning_node)
{
    return owning_node->kind == STMT_PASTE_EVALUATION;
}
void symbol_table_init_dummy(
    symbol_table* st, symbol_table* parent, ast_elem* owning_node)
{
    st->table = META_TABLE;
    st->owning_node = owning_node;
    st->parent = parent;
    st->hash_mask = 0;
    st->decl_count = 0;
    st->usings = NULL;
    st->non_meta_parent = symbol_table_nonmeta(parent);
}
int symbol_table_init(
    symbol_table** tgt, ureg decl_count, ureg using_count, bool force_unique,
    ast_elem* owning_node)
{

    if (!force_unique && decl_count == 0 && using_count == 0) {
        *tgt = NULL;
    }
    symbol_table* st = tmalloc(sizeof(symbol_table));
    if (!st) return ERR;
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
        st->hash_mask = 0;
        if (symbol_table_should_be_meta(owning_node)) {
            st->table = META_TABLE;
        }
        else {
            st->table = EMPTY_TABLE;
        }
    }
    st->owning_node = owning_node;
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
static inline bool symbol_table_is_metatable(symbol_table* st)
{
    return (st->table == META_TABLE);
}
symbol_table* symbol_table_nonmeta(symbol_table* st)
{
    return (st->table == META_TABLE) ? st->non_meta_parent : st;
}
symbol_table* symbol_table_get_module_table(symbol_table* st)
{
    while (st->owning_node->kind != ELEM_MDG_NODE) {
        st = st->parent;
        assert(st);
    }
    return st;
}
static inline ureg symbol_table_get_capacity(symbol_table* st)
{
    return st->hash_mask ? st->hash_mask + 1 : 0;
}
void symbol_table_set_parent(symbol_table* st, symbol_table* parent)
{
    st->parent = parent;
    if (symbol_table_is_metatable(st)) {
        st->non_meta_parent = symbol_table_nonmeta(parent);
    }
}
void symbol_table_insert_use(
    symbol_table* st, access_modifier am, ast_node* using_node,
    symbol_table* used_symtab)
{
    st = symbol_table_nonmeta(st);
    // reverse the am so it goes from public to unspecified upwars in
    // memory
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
    *(ast_node**)ptradd(ut->using_ends[am], usings_size) = using_node;
    ut->using_ends[am]++;
}

symbol_table** symbol_table_get_uses_start(symbol_table* st, access_modifier am)
{
    assert(st->usings);
    if (am == 0)
        return (symbol_table**)ptradd(st->usings, sizeof(usings_table));
    return st->usings->using_ends[am - 1];
}
symbol_table** symbol_table_get_uses_end(symbol_table* st, access_modifier am)
{
    assert(st->usings);
    return st->usings->using_ends[am];
}
ast_node** symbol_table_get_use_node(symbol_table* st, symbol_table** using_st)
{
    assert(st->usings);
    return (ast_node**)ptradd(
        using_st, st->usings->usings_count * sizeof(symbol_table**));
}
void symbol_table_fin(symbol_table* st)
{
    if (st != NULL) {
        // avoid throwing of our allocation counter
        if (st->usings) tfree(st->usings);
        // avoid freeing NULL_PTR_PTR
        if (!symbol_table_is_metatable(st) && st->decl_count) {
            tfree(st->table);
        }
        tfree(st);
    }
}
int symbol_table_amend(symbol_table* st, ureg decl_count, ureg usings)
{
    st = symbol_table_nonmeta(st);
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
                UNUSED(r); // make Release build happy
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
    st = symbol_table_nonmeta(st);
    ureg idx = fnv_hash_str(FNV_START_HASH, name) & st->hash_mask;
    symbol** tgt = &st->table[idx];
    while (*tgt) {
        if (strcmp((**tgt).name, name) == 0) return tgt;
        tgt = (symbol**)&(**tgt).next;
    }
    return tgt;
}
symbol** symbol_table_insert(symbol_table* st, symbol* s)
{
    st = symbol_table_nonmeta(st);
    ureg idx = fnv_hash_str(FNV_START_HASH, s->name) & st->hash_mask;
    symbol** tgt = &st->table[idx];
    while (*tgt) {
        if (strcmp((**tgt).name, s->name) == 0) return tgt;
        tgt = (symbol**)&(**tgt).next;
    }
    *tgt = s;
    s->next = NULL;
    return NULL;
}
ureg symbol_table_prehash(const char* s)
{
    return fnv_hash_str(FNV_START_HASH, s);
}

symbol* symbol_table_lookup_raw(symbol_table* st, ureg hash, const char* name)
{
    ureg idx = hash & st->hash_mask;
    symbol* tgt = st->table[idx];
    while (tgt) {
        if (cstr_eq(tgt->name, name)) return tgt;
        tgt = tgt->next;
    }
    return tgt;
}

src_map* symbol_table_get_smap(symbol_table* st)
{
    // TODO: this can happen if neither the func nor the mf have a single
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
    if (symbol_table_init(root_symtab, PRIMITIVE_COUNT + 1, 0, true, NULL)) {
        return ERR;
    }
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
    ureg cap = symbol_table_get_capacity(st);
    if (cap == 0) {
        stit->end = stit->pos;
        stit->subpos = NULL;
        return;
    }
    stit->end = stit->pos + cap - 1;
    stit->subpos = *stit->pos;
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
        if (stit->pos == stit->end) {
            return NULL;
        }
        stit->pos++;
        stit->subpos = *stit->pos;
    }
}
