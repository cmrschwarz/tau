#include "symbol_table.h"
#include "ast.h"
#include "utils/allocator.h"
#include "utils/error.h"
#include "utils/fnv_hash.h"
#include "utils/math_utils.h"

int symbol_table_setup(symbol_table* t)
{
    ureg decl_count = t->decl_count;
    t->table = tmalloc(decl_count * sizeof(symbol*) + sizeof(ureg));
    if (!t->table) return ERR;
    *(ureg*)t->table = decl_count;
    return OK;
}
void symbol_table_destruct(symbol_table* t)
{
    tfree(t->table);
}
void symbol_table_unpack(symbol_table t, symbol_table_unpacked* stu)
{
    stu->decl_count = *(ureg*)t.table;
    stu->start = ptradd(t.table, sizeof(ureg*));
}
void symbol_table_insert(symbol_table_unpacked* stu, symbol* s)
{
    ureg hash = fnv_hash_str(FNV_START_HASH, s->name) % stu->decl_count;
    symbol** tgt = stu->start + hash;
    while (*tgt) {
        tgt = (symbol**)&(**tgt).stmt.next;
    }
    *tgt = s;
    s->stmt.next = NULL;
}
symbol* symbol_table_lookup(symbol_table_unpacked* stu, const char* s)
{
    ureg hash = fnv_hash_str(FNV_START_HASH, s) % stu->decl_count;
    symbol* tgt = *(stu->start + hash);
    while (tgt) {
        if (strcmp(tgt->name, s) == 0) return tgt;
        tgt = (symbol*)tgt->stmt.next;
    }
    return NULL;
}
