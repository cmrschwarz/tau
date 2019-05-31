#include "symbol_store.h"
#include "ast.h"
#include "stddef.h"
#include "utils/allocator.h"
#include "utils/error.h"
#include "utils/fnv_hash.h"
#include "utils/math_utils.h"
#define USING_BIT (((ureg)1) << (REG_BITS - 1))
static symbol_table EMPTY_ST = {0, NULL, {0}};
void symbol_store_init(symbol_store* ss)
{
    ss->decl_count = 0;
}
void symbol_store_inc_decl_count(symbol_store* ss, ureg dc)
{
    ss->decl_count += dc;
}
ureg symbol_store_get_decl_count(symbol_store ss)
{
    return ss.decl_count & ~USING_BIT;
}
void symbol_store_merge_decls(symbol_store* tgt, symbol_store src)
{
    tgt->decl_count |= (src.decl_count & USING_BIT);
    tgt->decl_count += (src.decl_count & ~USING_BIT);
}
void symbol_store_require_unnamed_usings(symbol_store* ss)
{
    ss->decl_count |= USING_BIT;
}
bool symbol_store_requires_unnamed_usings(symbol_store* ss)
{
    return (ss->decl_count & USING_BIT) != 0;
}
static inline symbol_table_with_usings*
symbol_store_get_table_with_usings(symbol_store ss)
{
    return (symbol_table_with_usings*)ptrsub(
        ss.table, offsetof(symbol_table_with_usings, table));
}
int symbol_store_setup_table(symbol_store* ss)
{
    ureg decl_count = ss->decl_count;
    if (ss->decl_count == 0) {
        ss->table = &EMPTY_ST;
        return OK;
    }
    if (decl_count & USING_BIT) {
        decl_count &= ~USING_BIT;
        symbol_table_with_usings* stwu = tmalloc(
            decl_count * sizeof(symbol*) + sizeof(symbol_table_with_usings));
        if (!stwu) {
            ss->table = NULL;
            return ERR;
        }
        ss->table = &stwu->table;
    }
    else {
        ss->table =
            tmalloc(decl_count * sizeof(symbol*) + sizeof(symbol_table));
        if (!ss->table) return ERR;
    }
    memset(
        ptradd(ss->table, sizeof(symbol_table)), 0,
        decl_count * sizeof(symbol*));
    ss->table->usings = NULL;
    ss->table->decl_count = decl_count;
    symbol_store_init(&ss->table->ppst);
    return OK;
}
int symbol_store_ensure_unique(symbol_store* ss)
{
    if (ss->table == &EMPTY_ST) {
        ss->table = tmalloc(sizeof(symbol_table));
        if (!ss->table) return ERR;
        ss->table->decl_count = 0;
        ss->table->usings = NULL;
        ss->table->ppst.decl_count = 0;
    }
    return OK;
}
void symbol_store_destruct_table(symbol_store* ss)
{
    if (ss->table != &EMPTY_ST) {
        if (ss->table->ppst.table != NULL) {
            symbol_store_destruct_table(&ss->table->ppst);
        }
        tfree(ss->table);
    }
    ss->table = NULL;
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
