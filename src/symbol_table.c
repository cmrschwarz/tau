#include "symbol_table.h"

#include "ast.h"
#include "stddef.h"
#include "utils/allocator.h"
#include "utils/error.h"
#include "utils/fnv_hash.h"
#include "utils/math_utils.h"

#define USING_BIT (((ureg)1) << (REG_BITS - 1))
static symbol_table EMPTY_ST = {0, NULL, &EMPTY_ST};

symbol_table* symbol_table_new(ureg decl_count, ureg using_count)
{
    if (decl_count == 0 && using_count == 0) return &EMPTY_ST;
    symbol_table* st;
    if (using_count == 0) {
        symbol_table_with_usings* stwu = tmalloc(
            decl_count * sizeof(symbol*) + sizeof(symbol_table_with_usings));
        if (!stwu) return NULL;
        st = &stwu->table;
    }
    else {
        st = tmalloc(decl_count * sizeof(symbol*) + sizeof(symbol_table));
        if (!st) return NULL;
        st->usings = NULL;
    }
    memset(ptradd(st, sizeof(symbol_table)), 0, decl_count * sizeof(symbol*));
    st->pp_symtab = &EMPTY_ST;
    st->decl_count = decl_count;
    return st;
}

void symbol_table_delete(symbol_table* st)
{
    if (st != &EMPTY_ST) {
        symbol_table_delete(st->pp_symtab);
        tfree(st);
    }
}

symbol* symbol_table_insert(symbol_table* st, symbol* s)
{
    ureg hash = fnv_hash_str(FNV_START_HASH, s->name) % st->decl_count;
    symbol** tgt = ptradd(st, sizeof(symbol_table) + hash * sizeof(symbol*));
    while (*tgt) {
        if (strcmp((**tgt).name, s->name) == 0) return *tgt;
        tgt = (symbol**)&(**tgt).stmt.next;
    }
    *tgt = s;
    s->stmt.next = NULL;
    return NULL;
}
symbol* symbol_table_lookup(symbol_table* st, const char* s)
{
    ureg hash = fnv_hash_str(FNV_START_HASH, s) % st->decl_count;
    symbol* tgt =
        *(symbol**)ptradd(st, sizeof(symbol_table) + hash * sizeof(symbol*));
    while (tgt) {
        if (strcmp(tgt->name, s) == 0) return tgt;
        tgt = (symbol*)tgt->stmt.next;
    }
    return NULL;
}

symbol_table* symbol_table_new(ureg decl_count, ureg using_count);
void symbol_table_delete(symbol_table* st);
