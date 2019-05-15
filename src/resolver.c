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
        src_file* f = open_scope_get_file(osc);
        error_log_report_annotated_twice(
            &r->tc->error_log, ES_RESOLVER, false, "symbol redeclaration", f,
            sr_new.start, sr_new.end,
            "a symbol with this name is already defined", f, sr_old.start,
            sr_old.end, "previously defined here");
        return RE_SYMBOL_REDECLARATION;
    }
    return RE_OK;
}
resolve_error mdg_node_add_sym(resolver* r, mdg_node* m, symbol* sym)
{
    symbol* res = symbol_store_insert(m->ss, sym);
    if (res) {
        src_range_large sr_new, sr_old;
        src_range_unpack(sym->stmt.srange, &sr_new);
        src_range_unpack(res->stmt.srange, &sr_old);
        error_log_report_annotated_twice(
            &r->tc->error_log, ES_RESOLVER, false, "symbol redeclaration",
            sr_new.file, sr_new.start, sr_new.end,
            "a symbol with this name is already defined", sr_old.file,
            sr_old.start, sr_old.end, "previously defined here");
        return RE_SYMBOL_REDECLARATION;
    }
    return RE_OK;
}
resolve_error body_add_declarations(resolver* r, symbol* parent, body* tgt)
{
    // TODO
}
resolve_error mdg_node_add_declarations(resolver* r, mdg_node* m)
{
    symbol_store_init(&m->ss);
    aseglist_iterator tgti;
    aseglist_iterator_begin(&tgti, &m->targets);
    open_scope* osci = aseglist_iterator_next(&tgti);
    while (osci) {
        symbol_store_merge_decls(&m->ss, osci->shared_decl_count);
        osci = aseglist_iterator_next(&tgti);
    }
    if (symbol_store_setup_table(&m->ss)) return RE_FATAL;
    aseglist_iterator_begin(&tgti, &m->targets);
    while (true) {
        osci = aseglist_iterator_next(&tgti);
        if (!osci) break;
        if (symbol_store_setup_table(&osci->scope.body.ss)) {
            do {
                // to prevent the uninitialized tables from being freed
                osci->scope.body.ss.table = NULL;
                osci = aseglist_iterator_next(&tgti);
            } while (osci);
            return RE_FATAL;
        }
        stmt* si = osci->scope.body.children;
        while (si) {
            if (ast_node_is_symbol((ast_node*)si)) {
                symbol* sym = (symbol*)si;
                access_modifier am = stmt_flags_get_access_mod(sym->stmt.flags);
                if (am == AM_SCOPE_LOCAL) {
                    osc_add_sym(r, osci, sym);
                }
                else {
                    mdg_node_add_sym(r, m, sym);
                }
                if (ast_node_is_scope((ast_node*)si)) {
                    body_add_declarations(r, sym, &((scope*)sym)->body);
                }
            }
            else if (si->type == STMT_EXPRESSION) {
                // TODO
            }
            si = si->next;
        }
    }
    return OK;
}
resolve_error
resolver_resolve_multiple(resolver* r, mdg_node** start, mdg_node** end)
{
    // DEBUG
    printf("resolving {");
    mdg_node** i = start;
    i = start;
    while (i + 1 != end) {
        printf("%s, ", (**i).name);
        i++;
    }
    printf("%s}\n", (**i).name);
    // TODO: resolve
    r->start = start;
    r->end = end;
    resolve_error re;
    for (mdg_node** i = start; i != end; i++) {
        re = mdg_node_add_declarations(r, *i);
        if (re) {
            if (re == RE_FATAL) {
            }
            for (mdg_node** j = i + 1; j != end; j++) {
                (**j).ss.table = NULL;
            }
            return re;
        }
    }
    return mdg_nodes_resolved(start, end, r->tc);
}

int resolver_resolve_single(resolver* r, mdg_node* node)
{
    mdg_node* node_buff[2];
    node_buff[0] = node;
    node_buff[1] = NULL;
    return resolver_resolve_multiple(r, node_buff, &node_buff[1]);
}
