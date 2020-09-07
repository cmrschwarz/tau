#include "pp_decl_clobber_table.h"
#include "utils/fnv_hash.h"
#include "utils/error.h"
#include "utils/zero.h"

int ppdct_init(pp_decl_clobber_table* t)
{
    t->table = tmalloc(sizeof(pp_decl_clobber) * 16);
}

ureg ppdct_prehash_name(char* name)
{
    return fnv_hash_str(FNV_START_HASH, name);
}

pp_decl_clobber* ppdct_lookup_raw(
    pp_decl_clobber_table* t, symbol_table* st, char* name, ureg name_prehash,
    ureg cap, ureg mask)
{
    ureg idx = fnv_fold(fnv_hash_pointer(name_prehash, st), t->hash_bits, mask);
    pp_decl_clobber* end = &t->table + cap;
    while (true) {
        pp_decl_clobber* ppdc = &t->table[idx];
        if (!ppdc->symtab) return NULL;
        if (ppdc->symtab == st) {
            if (cstr_eq(ppdc->name, name)) return ppdc;
        }
        ppdc++;
        if (ppdc == end) ppdc = t->table;
    }
}

int ppdct_appended(
    pp_decl_clobber_table* t, pp_decl_clobber* loc, symbol_table* st,
    char* name, ast_node* conflicting)
{
    ureg cap = 1 << t->hash_bits;
    if (t->clobber_count + (t->clobber_count >> 1) >= cap) {
        // size times two but 0 becomes 2 :)
        ureg cap_new = cap << 2;
        pp_decl_clobber* tn = tmallocz(cap_new * sizeof(pp_decl_clobber));
        if (!tn) return ERR;
        pp_decl_clobber* i = t->table;
        pp_decl_clobber* end = &t->table + cap;
        t->hash_bits++;
        t->table = tn;
        while (i != end) {
            if (i->name) {
                *ppdct_lookup_raw(
                    t, i->symtab, i->name, cap_new, cap_new - 1,
                    ppdct_prehash_name(i->name)) = *i;
            }
            i++;
        }
        return OK;
    }
}

ppdct_init(pp_decl_clobber_table* t)
{
    t->table = NULL_PTR_PTR;
    t->hash_bits = 0;
    t->clobber_count = 0;
}
