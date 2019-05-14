#include "resolver.h"
#include "thread_context.h"
#include "utils/error.h"
int resolver_init(resolver* r, thread_context* tc)
{
    r->tc = tc;
    return OK;
}
void resolver_fin(resolver* r)
{
}
resolve_error osc_add_sym(resolver* r, open_scope* osc, symbol* sym)
{
    symbol* res = symbol_store_insert(osc->scope.body.ss, sym);
    if (res) {
        src_range_large sr_new, sr_old;
        src_range_unpack(sym->stmt.srange, &sr_new);
        src_range_unpack(res->stmt.srange, &sr_old);
        error_log_report_annotated_twice(
            &r->tc->error_log, ES_RESOLVER, false, "symbol redeclaration",
            open_scope_get_file(osc), sr_new.start, sr_new.end,
            "a symbol with this name is already defined", sr_old.start,
            sr_old.end, "previously defined here");
        return RE_SYMBOL_REDECLARATION;
    }
    return RE_OK;
}
resolve_error map_module_declarations(resolver* r, mdg_node* m)
{
    symbol_store_init(&m->ss);
    open_scope* osc = atomic_ptr_load_flat(&m->targets);
    open_scope* osci = osc;
    while (osci) {
        symbol_store_merge_decls(&m->ss, osci->shared_decl_count);
        osci = (open_scope*)osci->scope.symbol.stmt.next;
    }
    if (symbol_store_setup_table(&m->ss)) return RE_FATAL;
    osci = osc;
    while (osci) {
        if (symbol_store_setup_table(&osci->scope.body.ss)) return RE_FATAL;
        stmt* si = osci->scope.body.children;
        while (si) {
            switch (si->type) {
                case OSC_MODULE:
                case SYM_FUNC:
                case SYM_FUNC_GENERIC:
                case SYM_VAR:
                case SYM_VAR_UNINITIALIZED:
                case SYM_NAMED_USING: {
                    access_modifier am = stmt_flags_get_access_mod(si->flags);
                    if (am == AM_SCOPE_LOCAL) {
                        osc_add_sym(r, osci, (symbol*)si);
                    }
                    else {
                    }
                }
                default: break;
            }
            si = si->next;
        }
        osci = (open_scope*)osci->scope.symbol.stmt.next;
    }
    return OK;
}
resolve_error
resolver_resolve_multiple(resolver* r, mdg_node** start, mdg_node** end)
{
    r->start = start;
    r->end = end;
    // DEBUG
    printf("resolving {");
    mdg_node** i = start;
    while (i + 1 != end) {
        printf("%s, ", (**i).name);
        i++;
    }
    printf("%s}\n", (**i).name);
    // TODO: resolve
    return mdg_nodes_resolved(start, end, r->tc);
}

int resolver_resolve_single(resolver* r, mdg_node* node)
{
    mdg_node* node_buff[2];
    node_buff[0] = node;
    node_buff[1] = NULL;
    return resolver_resolve_multiple(r, node_buff, &node_buff[1]);
}
