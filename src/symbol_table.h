#pragma once
#include "utils/string.h"
typedef struct symbol symbol;

typedef union symbol_table {
    symbol** table;
    ureg decl_count;
} symbol_table;

typedef struct symbol_table_unpacked {
    symbol** start;
    ureg decl_count;
} symbol_table_unpacked;

int symbol_table_setup(symbol_table* t);
void symbol_table_destruct(symbol_table* t);
void symbol_table_unpack(symbol_table t, symbol_table_unpacked* stu);
void symbol_table_insert(symbol_table_unpacked* stu, symbol* s);
symbol* symbol_table_lookup(symbol_table_unpacked* stu, const char* s);
