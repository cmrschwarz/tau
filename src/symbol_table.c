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
#define USING_BIT (((ureg)1) << (REG_BITS - 1))
symbol_table EMPTY_ST = {0, NULL, NULL, NULL, NULL};

int symbol_table_init(
    symbol_table** tgt, ureg decl_count, ureg using_count, bool force_unique,
    ast_node* owning_node)
{
    if (!force_unique && decl_count == 0 && using_count == 0) {
        *tgt = &EMPTY_ST;
        return OK;
    }
    symbol_table* st;
    if (using_count != 0) {
        symbol_table_with_usings* stwu = tmalloc(
            decl_count * sizeof(symbol*) + sizeof(symbol_table_with_usings));
        if (!stwu) return ERR;
        st = &stwu->table;
        st->usings = (stmt*)NULL_PTR_PTR;
    }
    else {
        st = tmalloc(decl_count * sizeof(symbol*) + sizeof(symbol_table));
        if (!st) return ERR;
        st->usings = NULL;
    }
    memset(ptradd(st, sizeof(symbol_table)), 0, decl_count * sizeof(symbol*));
    st->pp_symtab = &EMPTY_ST;
    st->decl_count = decl_count;
    st->owning_node = owning_node;
    *tgt = st;
    return OK;
}

void symbol_table_fin(symbol_table* st)
{
    if (st != NULL && st != &EMPTY_ST) {
        symbol_table_fin(st->pp_symtab);
        if (st->usings != NULL) {
            tfree(ptrsub(st, offsetof(symbol_table_with_usings, table)));
        }
        else {
            tfree(st);
        }
    }
}

symbol* symbol_table_insert(symbol_table* st, symbol* s)
{
    ureg hash = fnv_hash_str(FNV_START_HASH, s->name) % st->decl_count;
    symbol** tgt = ptradd(st, sizeof(symbol_table) + hash * sizeof(symbol*));
    while (*tgt) {
        if (strcmp((**tgt).name, s->name) == 0) return *tgt;
        tgt = (symbol**)&(**tgt).next;
    }
    *tgt = s;
    s->next = NULL;
    return NULL;
}
symbol* symbol_table_lookup(symbol_table* st, const char* s)
{
    ureg hash = fnv_hash_str(FNV_START_HASH, s) % st->decl_count;
    symbol* tgt =
        *(symbol**)ptradd(st, sizeof(symbol_table) + hash * sizeof(symbol*));
    while (tgt) {
        if (strcmp(tgt->name, s) == 0) return tgt;
        tgt = (symbol*)tgt->next;
    }
    return NULL;
}
src_file* symbol_table_get_file(symbol_table* st)
{
    src_file* f = src_range_get_file(st->owning_node->srange);
    if (f) return f;
    if (st->parent) return symbol_table_get_file(st->parent);
    return NULL;
}
