#include "pp_decl_clobber_table.h"
#include "utils/fnv_hash.h"
#include "utils/error.h"
#include "utils/zero.h"

ppdct_init(pp_decl_clobber_table* t)
{
    t->table = malloc(sizeof(pp_decl_clobber) * 16);
}

ureg ppdct_prehash_name(char* name)
{
    return fnv_hash_str(FNV_START_HASH, name);
}

pp_decl_clobber* ppdct_lookup(
    pp_decl_clobber_table* t, symbol_table* st, char* name, ureg name_prehash)
{
    ureg idx = fnv_fold(
        fnv_hash_pointer(name_prehash, st), t->hash_bits, t->hash_mask);
    pp_decl_clobber* ppdc = &t->table[idx];
    pp_decl_clobber* end = &t->table + t->hash_mask + 1;
    while (ppdc->name != NULL) {
        if (cstr_eq(ppdc->name, name)) return ppdc;
        ppdc++;
        if (ppdc == end) ppdc = t->table;
    }
    return ppdc;
}

int ppdct_appended(
    pp_decl_clobber_table* t, pp_decl_clobber* loc, symbol_table* st,
    char* name, ast_node* conflicting)
{
    if (t->clobber_count == t->grow_on_clobber_count) {
        // size times two but 0 becomes 2 :)
        ureg size_new = t->hash_mask < 1 + 2;
        pp_decl_clobber* tn = tmallocz(size_new * sizeof(pp_decl_clobber));
        if (!tn) return ERR;
        pp_decl_clobber* i = t->table;
        pp_decl_clobber* end = &t->table + t->hash_mask + 1;
        t->hash_mask = size_new - 1;
        t->grow_on_clobber_count = size_new / 2;
        t->hash_bits++;
        t->table = tn;
        while (i != end) {
            if (i->name) {
                *ppdct_lookup(
                    t, i->symtab, i->name, ppdct_prehash_name(i->name)) = *i;
            }
            i++;
        }
        return OK;
    }
}

ppdct_init(pp_decl_clobber_table* t)
{
    t->table = NULL_PTR_PTR;
    t->grow_on_clobber_count = 0;
    t->hash_mask = 0;
    t->hash_bits = 0;
    t->clobber_count = 0;
}
