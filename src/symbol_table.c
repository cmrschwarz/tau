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
static inline symbol_table_with_usings* get_stwu(symbol_table* st)
{
    assert(st->usings_start);
    return (symbol_table_with_usings*)ptrsub(
        st, offsetof(symbol_table_with_usings, table));
}
int symbol_table_init(
    symbol_table** tgt, ureg decl_count, ureg using_count, bool force_unique,
    ast_elem* owning_node)
{
    assert(*tgt == NULL);
    // we don't worry about ceiling the size to a power of two since
    // we will eventually pool allocate the symtabs which removes this
    // size constraint
    if (!force_unique && decl_count == 0 && using_count == 0) {
        *tgt = NULL;
        return OK;
    }
    symbol_table* st;
    if (using_count != 0) {
        ureg usings_size =
            using_count * (sizeof(symbol_table*) + sizeof(ast_node*));
        ureg table_size = decl_count * sizeof(symbol*);
        void* alloc = tmalloc(
            sizeof(symbol_table_with_usings) + usings_size + table_size);
        if (!alloc) return ERR;
        symbol_table_with_usings* stwu = ptradd(alloc, usings_size);
        st = &stwu->table;
        st->usings_start =
            (symbol_table**)ptradd(alloc, using_count * sizeof(ast_node*));
        memset(st->usings_start, 0, using_count * sizeof(symbol*));
        for (ureg i = 0; i < AM_ENUM_ELEMENT_COUNT; i++) {
            stwu->using_ends[i] = st->usings_start;
        }
    }
    else {
        st = tmalloc(decl_count * sizeof(symbol*) + sizeof(symbol_table));
        if (!st) return ERR;
        st->usings_start = NULL;
    }
    memset(ptradd(st, sizeof(symbol_table)), 0, decl_count * sizeof(symbol*));
    st->pp_symtab = NULL;
    st->decl_count = decl_count;
    st->owning_node = owning_node;
    *tgt = st;
    return OK;
}

void symbol_table_insert_using(
    symbol_table* st, access_modifier am, ast_node* use, symbol_table* ust)
{
    // reverse the am so it goes from public to unspecified upwars in memory
    am = AM_ENUM_ELEMENT_COUNT - am;
    symbol_table_with_usings* stwu = get_stwu(st);
    ureg usings_size = ptrdiff(st->usings_start, stwu);
    for (int i = AM_ENUM_ELEMENT_COUNT - 1; i != am; i--) {
        *stwu->using_ends[i] = *stwu->using_ends[i - 1];
        *(ast_node**)ptrsub(stwu->using_ends[i], usings_size) =
            *(ast_node**)ptrsub(stwu->using_ends[i - 1], usings_size);
        stwu->using_ends[i]++;
    }
    *stwu->using_ends[am] = ust;
    *(ast_node**)ptrsub(stwu->using_ends[am], usings_size) = use;
    stwu->using_ends[am]++;
}

void symbol_table_fin(symbol_table* st)
{
    if (st != NULL) {
        symbol_table_fin(st->pp_symtab);
        if (st->usings_start != NULL) {
            symbol_table_with_usings* stwu = get_stwu(st);
            ureg usings_size = ptrdiff(stwu, st->usings_start) *
                               ((sizeof(symbol_table*) + sizeof(ast_node*)) /
                                sizeof(symbol_table*));
            tfree(ptrsub(stwu, usings_size));
        }
        else {
            tfree(st);
        }
    }
}
symbol** symbol_table_find_insert_position(symbol_table* st, char* name)
{
    ureg hash = fnv_hash_str(FNV_START_HASH, name) % st->decl_count;
    symbol** tgt = ptradd(st, sizeof(symbol_table) + hash * sizeof(symbol*));
    while (*tgt) {
        if (strcmp((**tgt).name, name) == 0) return tgt;
        tgt = (symbol**)&(**tgt).next;
    }
    return tgt;
}
symbol** symbol_table_insert(symbol_table* st, symbol* s)
{
    ureg hash = fnv_hash_str(FNV_START_HASH, s->name) % st->decl_count;
    symbol** tgt = ptradd(st, sizeof(symbol_table) + hash * sizeof(symbol*));
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
    const char* s, ureg* decl_ppl)
{
    ureg hash = fnv_hash_str(FNV_START_HASH, s);
    do {
        symbol_table* curr_st = st;
        ureg curr_ppl = 0;
        while (curr_ppl < ppl) {
            if (!curr_st->pp_symtab) break;
            curr_st = curr_st->pp_symtab;
            curr_ppl++;
        }
        while (true) {
            // PERF: get rid of this check somehow
            if (curr_st->decl_count != 0) {
                ureg idx = hash % curr_st->decl_count;
                symbol** tgt = (symbol**)ptradd(
                    curr_st, sizeof(symbol_table) + idx * sizeof(symbol*));
                while (*tgt) {
                    if (strcmp((**tgt).name, s) == 0) {
                        if (decl_ppl) *decl_ppl = curr_ppl;
                        return tgt;
                    }
                    tgt = (symbol**)&(**tgt).next;
                }
            }
            if (st->usings_start) {
                symbol_table** i = st->usings_start;
                symbol_table** end =
                    get_stwu(st)->using_ends[AM_ENUM_ELEMENT_COUNT - am];
                // for pub usings we can look at their pub and prot symbols
                symbol** res =
                    symbol_table_lookup(*i, AM_PROTECTED, ppl, s, decl_ppl);
                if (res) return res;
                i++;
                // for prot to unspecified usings we can look at their pub
                // symbols and if they are used directly by us at ther protected
                // symbols
                access_modifier tgt_am =
                    (am < AM_PROTECTED) ? AM_PROTECTED : AM_PUBLIC;
                while (i != end) {
                    res = symbol_table_lookup(*i, tgt_am, ppl, s, decl_ppl);
                    if (res) return res;
                }
            }
            if (curr_ppl == 0) break;
            curr_st = curr_st->parent;
            curr_ppl--;
        }
        st = st->parent;
    } while (st != stop_at);
    return NULL;
}
symbol** symbol_table_lookup(
    symbol_table* st, ureg ppl, access_modifier am, const char* s,
    ureg* decl_ppl)
{
    return symbol_table_lookup_limited(st, ppl, am, NULL, s, decl_ppl);
}
src_file* symbol_table_get_file(symbol_table* st)
{
    assert(st->owning_node->kind != ELEM_MDG_NODE);
    src_file* f = src_range_get_file(((ast_node*)st->owning_node)->srange);
    if (f) return f;
    // because pp tables parent the postprocessing table
    // we naturally transition out of pp levels
    if (st->parent) return symbol_table_get_file(st->parent);
    return NULL;
}

int init_global_symtab()
{
    if (symbol_table_init(&GLOBAL_SYMTAB, PRIMITIVE_COUNT + 1, 0, true, NULL))
        return ERR;
    GLOBAL_SYMTAB->parent = NULL;
    for (int i = 0; i < PRIMITIVE_COUNT; i++) {
        if (symbol_table_insert(GLOBAL_SYMTAB, (symbol*)&PRIMITIVES[i])) {
            symbol_table_fin(GLOBAL_SYMTAB);
            return ERR;
        }
    }
    return OK;
}
void fin_global_symtab()
{
    symbol_table_fin(GLOBAL_SYMTAB);
}
void symtab_it_begin(symtab_it* stit, symbol_table* st)
{
    stit->pos = (symbol**)(st + 1);
    stit->subpos = *stit->pos;
    stit->end = stit->pos + st->decl_count;
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
