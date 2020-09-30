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
#include "trait_table.h"

ureg symbol_table_has_usings(symbol_table* st)
{
    return st->table_offset != sizeof(symbol_table) / sizeof(void*);
}
ureg symbol_table_get_symbol_capacity(symbol_table* st)
{
    assert(st->table_bitcount);
    return (1 << st->table_bitcount) - st->table_offset;
}
ureg symbol_table_get_using_capacity(symbol_table* st)
{
    assert(symbol_table_has_usings(st));
    return (st->table_offset * sizeof(void*) - sizeof(usings_table)) /
           (2 * sizeof(void*));
}
ureg symbol_table_get_using_count(symbol_table* st)
{
    if (!symbol_table_has_usings(st)) return 0;
    usings_table* ut = (usings_table*)st;
    return (ptrdiff(ut->using_ends[AM_ENUM_ELEMENT_COUNT - 1], ut) -
            sizeof(usings_table)) /
           sizeof(ast_body*);
}
ureg symbol_table_get_symbol_count(symbol_table* st)
{
    return st->sym_count;
}

void symbol_table_insert_use(
    symbol_table* st, access_modifier am, ast_node* using_node,
    ast_body* using_body, bool no_syms, bool no_impls)
{
    assert(((ureg)using_body & 0x3) == 0);
    assert(!no_syms || !no_impls);
    usings_table* ut = (usings_table*)st;
    ureg usings_cap = symbol_table_get_using_capacity(st) * sizeof(ast_body*);
    assert(symbol_table_get_using_count(st) * sizeof(void*) < usings_cap);
    for (access_modifier i = AM_ENUM_ELEMENT_COUNT - 1; i != am; i--) {
        *ut->using_ends[i] = *ut->using_ends[i - 1];
        *(ast_node**)ptradd(ut->using_ends[i], usings_cap) =
            *(ast_node**)ptradd(ut->using_ends[i - 1], usings_cap);
        ut->using_ends[i]++;
    }
    *ut->using_ends[am] =
        (ast_body*)((ureg)using_body | (no_syms ? 2 : 0) | (no_impls ? 1 : 0));
    *(ast_node**)ptradd(ut->using_ends[am], usings_cap) = using_node;
    ut->using_ends[am]++;
}

ast_body** symbol_table_get_uses_start(symbol_table* st, access_modifier am)
{
    assert(symbol_table_has_usings(st));
    usings_table* ut = (usings_table*)st;
    if (am == 0) return (ast_body**)ptradd(ut, sizeof(usings_table));
    return ut->using_ends[am - 1];
}
ast_body** symbol_table_get_uses_end(symbol_table* st, access_modifier am)
{
    assert(symbol_table_has_usings(st));
    usings_table* ut = (usings_table*)st;
    return ut->using_ends[am];
}
ast_node** symbol_table_get_use_node(symbol_table* st, ast_body** using_st)
{
    assert(symbol_table_has_usings(st));
    ureg usings_cap = symbol_table_get_using_capacity(st) * sizeof(ast_body*);
    return (ast_node**)ptradd(using_st, usings_cap * sizeof(ast_body*));
}
void symbol_table_unwrap_use(
    ast_body* use_body_wrapped, ast_body** use_body, bool* no_syms,
    bool* no_impls)
{
    *use_body = (ast_body*)((ureg)use_body_wrapped & ~((ureg)0x2));
    *no_syms = (((ureg)use_body_wrapped) & 0x2) != 0;
    *no_impls = (((ureg)use_body_wrapped) & 0x1) != 0;
}
symbol** symbol_table_calculate_position(symbol_table* st, ureg hash)
{
    ureg sym_mask = (1 << st->sym_bitcount) - 1;
    ureg table_mask = (1 << st->table_bitcount) - 1;
    ureg hash_folded = fnv_fold(hash, st->sym_bitcount, sym_mask);
    ureg idx = (hash_folded + st->table_offset) & table_mask;
    if (idx < st->table_offset) {
        idx = st->table_offset + sym_mask - hash_folded;
    }
    return (symbol**)ptradd(st, idx * sizeof(void*));
}

symbol* symbol_table_insert(symbol_table* st, symbol* s)
{
    symbol** tgt = symbol_table_lookup(st, s->name);
    if (*tgt) return *tgt;
    *tgt = s;
    s->next = NULL;
    st->sym_count++;
    return NULL;
}

ureg symbol_table_prehash(const char* s)
{
    return fnv_hash_str(FNV_START_HASH, s);
}
symbol** symbol_table_lookup_raw(symbol_table* st, ureg hash, const char* name)
{
    symbol** tgt = symbol_table_calculate_position(st, hash);
    while (*tgt) {
        if (strcmp((**tgt).name, name) == 0) return tgt;
        tgt = (symbol**)&(**tgt).next;
    }
    return tgt;
}
symbol** symbol_table_lookup(symbol_table* st, char* name)
{
    return symbol_table_lookup_raw(st, symbol_table_prehash(name), name);
}
void symbol_table_inc_sym_count(symbol_table* st)
{
    st->sym_count++;
    assert(st->sym_count <= symbol_table_get_symbol_capacity(st));
}

void symtab_it_init(symtab_it* stit, symbol_table* st)
{
    stit->pos = ptradd(st, st->table_offset * sizeof(void*));
    stit->last =
        ptradd(st, (((ureg)1 << st->table_bitcount) - 1) * sizeof(void*));
    stit->subpos = *stit->pos;
}
symtab_it symtab_it_make(symbol_table* st)
{
    symtab_it it;
    symtab_it_init(&it, st);
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
        if (stit->pos == stit->last) {
            return NULL;
        }
        stit->pos++;
        stit->subpos = *stit->pos;
    }
}

symbol_table* symbol_table_create(
    ureg sym_count, ureg using_count, ureg impl_count, ureg generic_impl_count)
{
    assert(sym_count || using_count);
    ureg size = sym_count * sizeof(symbol*);
    ureg size_ceiled;
    symbol_table* st;
    if (using_count) {
        size += using_count * sizeof(void*) * 2;
        size += sizeof(usings_table);
        size_ceiled = ceil_to_pow2(size);
        ureg cap_space = (size_ceiled - size) / sizeof(void*);
        ureg using_cap;
        if (using_count < cap_space) {
            using_cap = using_count * 2;
        }
        else {
            using_cap = using_count + cap_space / 2;
        }
        usings_table* ut = tmalloc(size_ceiled);
        if (!ut) return NULL;
        st = (symbol_table*)ut;
        ureg using_part_size =
            sizeof(usings_table) + using_cap * sizeof(void*) * 2;
        st->table_offset = using_part_size / sizeof(void*);
        // if table size is not a multiple of sizeof(void*) everything breaks
        assert(st->table_offset * sizeof(void*) == using_part_size);
        st = &ut->symtab;
        ast_body** using_ends = ptradd(ut, sizeof(usings_table));
        for (ureg i = 0; i < AM_ENUM_ELEMENT_COUNT; i++) {
            ut->using_ends[i] = using_ends;
        }
    }
    else {
        size += sizeof(symbol_table);
        size_ceiled = ceil_to_pow2(size);
        st = tmalloc(size_ceiled);
        if (!st) return NULL;
        st->table_offset = sizeof(symbol_table) / sizeof(void*);
    }
    symbol** start = ptradd(st, st->table_offset * sizeof(void*));
    memset(start, 0, size_ceiled - ptrdiff(start, st));
    st->sym_count = 0;
    ureg size_in_ptrs = size_ceiled / sizeof(void*);
    st->table_bitcount = ulog2(size_in_ptrs);
    st->sym_bitcount = ulog2(ceil_to_pow2(size_in_ptrs - st->table_offset));
    if (impl_count || generic_impl_count) {
        st->tt = trait_table_create(impl_count, generic_impl_count);
        if (!st->tt) {
            symbol_table_destroy(st);
            return NULL;
        }
    }
    else {
        st->tt = NULL;
    }
    return st;
}
void symbol_table_destroy(symbol_table* st)
{
    if (st->tt) trait_table_destroy(st->tt);
    tfree(st);
}
int symbol_table_amend(symbol_table** stp, ureg sym_count, ureg using_count)
{
    symbol_table* st = *stp;
    ureg new_using_count = symbol_table_get_using_count(st) + using_count;
    st->sym_count += sym_count;
    ureg using_cap =
        symbol_table_has_usings(st) ? symbol_table_get_using_capacity(st) : 0;
    ureg sym_cap = symbol_table_get_symbol_capacity(st);
    if (st->sym_count <= sym_cap && new_using_count <= using_cap) return OK;
    symbol_table* st_new =
        symbol_table_create(st->sym_count, new_using_count, 0, 0);
    symtab_it it;
    symtab_it_init(&it, st);
    for (symbol* s = symtab_it_next(&it); s; s = symtab_it_next(&it)) {
        symbol* r = symbol_table_insert(st_new, s);
        if (r != NULL) assert(false);
    }
    if (symbol_table_has_usings(st)) {
        // PERF: memcopy the entire thing instead
        for (access_modifier am = 0; am < AM_ENUM_ELEMENT_COUNT; am++) {
            ast_body** start = symbol_table_get_uses_start(st, am);
            ast_body** end = symbol_table_get_uses_end(st, am);
            while (start != end) {
                bool no_syms;
                bool no_impls;
                ast_body* body_unwrapped;
                symbol_table_unwrap_use(
                    *start, &body_unwrapped, &no_syms, &no_impls);
                symbol_table_insert_use(
                    st_new, am, *symbol_table_get_use_node(st, start),
                    body_unwrapped, no_syms, no_impls);
                start++;
            }
        }
    }
    st_new->tt = st->tt;
    st->tt = NULL;
    symbol_table_destroy(st);
    *stp = st_new;
    return OK;
}
