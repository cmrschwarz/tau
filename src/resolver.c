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
resolve_error symbol_redefinition_error(
    resolver* r, src_range_large sr_old, src_range_large sr_new)
{
    error_log_report_annotated_twice(
        &r->tc->error_log, ES_RESOLVER, false, "symbol redeclaration",
        sr_new.file, sr_new.start, sr_new.end,
        "a symbol with this name is already defined", sr_old.file, sr_old.start,
        sr_old.end, "previously defined here");
    return RE_SYMBOL_REDECLARATION;
}
resolve_error body_add_sym(resolver* r, body* b, src_file* f, symbol* sym)
{
    symbol* res = symbol_store_insert(b->ss, sym);
    if (res) {
        src_range_large sr_new, sr_old;
        src_range_unpack(sym->stmt.srange, &sr_new);
        src_range_unpack(res->stmt.srange, &sr_old);
        sr_old.file = f;
        sr_new.file = f;
        return symbol_redefinition_error(r, sr_old, sr_new);
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
        return symbol_redefinition_error(r, sr_old, sr_new);
    }
    return RE_OK;
}
resolve_error body_add_declarations(resolver* r, body* tgt, src_file* f);
resolve_error expr_add_declarations(resolver* r, expr* ex, src_file* f)
{
    switch (ex->type) {
        case EXPR_OP_UNARY:
            return expr_add_declarations(r, ((expr_op_unary*)ex)->child, f);
        case EXPR_OP_BINARY: {
            expr_op_binary* eob = (expr_op_binary*)ex;
            resolve_error re = expr_add_declarations(r, eob->lhs, f);
            if (re) return re;
            re = expr_add_declarations(r, eob->rhs, f);
            return re;
        }
        case EXPR_BLOCK:
            return body_add_declarations(r, &((expr_block*)ex)->body, f);
        case EXPR_ARRAY: {
            expr** fst = ((expr_array*)ex)->elements;
            while (*fst) {
                resolve_error re = expr_add_declarations(r, *fst, f);
                if (re) return re;
            }
            return RE_OK;
        }
        case EXPR_DO: {
        }
        default: return RE_OK;
    }
}
resolve_error
compound_assign_add_declarations(resolver* r, stmt_compound_assignment* sc)
{
    // TODO
    return RE_OK;
}
resolve_error osc_run_preprocessor(resolver* r, mdg_node* m, open_scope* osc)
{
    // TODO
    return RE_OK;
}
resolve_error osc_add_delarations(resolver* r, mdg_node* mdgn, open_scope* osc)
{
    resolve_error re = symbol_store_setup_table(&osc->scope.body.ss);
    if (re) return re;
    stmt** prev_next = &osc->scope.body.children;
    stmt* si = *prev_next;
    src_file* f = open_scope_get_file(osc);
    while (si) {
        if (ast_node_is_symbol((ast_node*)si)) {
            symbol* sym = (symbol*)si;
            access_modifier am = stmt_flags_get_access_mod(sym->stmt.flags);
            if (am == AM_SCOPE_LOCAL) {
                re = body_add_sym(r, &osc->scope.body, f, sym);
            }
            else {
                re = mdg_node_add_sym(r, mdgn, sym);
            }
            if (re) break;
        }
        else if (si->type == STMT_PP_STMT) {
            stmt_pp_stmt* pps = (stmt_pp_stmt*)si;
            if (ast_node_is_symbol((ast_node*)pps->pp_stmt)) {
                symbol* sym = (symbol*)pps->pp_stmt;
                access_modifier am = stmt_flags_get_access_mod(sym->stmt.flags);
                if (am == AM_SCOPE_LOCAL) {
                    symbol_store_inc_decl_count(
                        &osc->scope.body.ss.table->ppst, 1);
                }
                else {
                    symbol_store_inc_decl_count(&mdgn->ss.table->ppst, 1);
                }
            }
        }
        else if (si->type == STMT_USING) {
            stmt_using* su = (stmt_using*)si;
            access_modifier am = stmt_flags_get_access_mod(su->stmt.flags);
            if (am == AM_SCOPE_LOCAL) {
                su->stmt.next = osc->scope.body.ss.table->usings;
                osc->scope.body.ss.table->usings = (stmt*)su;
            }
            else {
                su->stmt.next = mdgn->ss.table->usings;
                mdgn->ss.table->usings = (stmt*)su;
            }
        }
        else {
            *prev_next = si;
            prev_next = &si->next;
        }
        si = si->next;
    }
    return re;
}
resolve_error body_add_declarations(resolver* r, body* tgt, src_file* f)
{
    resolve_error re = symbol_store_setup_table(&tgt->ss);
    if (re) return re;
    stmt** prev_next = &tgt->children;
    stmt* si = *prev_next;
    while (si) {
        if (ast_node_is_symbol((ast_node*)si)) {
            symbol* sym = (symbol*)si;
            si = si->next;
            *prev_next = si;
            re = body_add_sym(r, tgt, f, sym);
            if (re) break;
            if (ast_node_is_scope((ast_node*)sym)) {
                re = body_add_declarations(r, &((scope*)sym)->body, f);
                if (re) break;
            }
        }
        else if (si->type == STMT_EXPRESSION) {
            re = expr_add_declarations(r, ((stmt_expr*)si)->expr, f);
            if (re) break;
            si = si->next;
            *prev_next = si;
        }
        else {
            *prev_next = si;
            prev_next = &si->next;
            si = si->next;
        }
    }
    return re;
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
    resolve_error re = RE_OK;
    while (true) {
        osci = aseglist_iterator_next(&tgti);
        if (!osci) break;
        re = osc_add_delarations(r, m, osci);
        if (re) break;
    }
    if (re) {
        while (true) {
            osci = aseglist_iterator_next(&tgti);
            if (!osci) break;
            // to prevent the uninitialized tables from being freed
            osci->scope.body.ss.table = NULL;
        }
        return re;
    }
    return re;
}
resolve_error mdg_node_run_preprocessor(resolver* r, mdg_node* mdgn)
{
    aseglist_iterator tgti;
    aseglist_iterator_begin(&tgti, &mdgn->targets);
    while (true) {
        open_scope* osci = aseglist_iterator_next(&tgti);
        if (!osci) break;
        resolve_error re = osc_run_preprocessor(r, mdgn, osci);
        if (re) return re;
    }
    return RE_OK;
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
            for (mdg_node** j = i + 1; j != end; j++) {
                (**j).ss.table = NULL;
            }
            return re;
        }
    }
    for (mdg_node** i = start; i != end; i++) {
        re = mdg_node_run_preprocessor(r, *i);
        if (re) return re;
    }
    for (mdg_node** i = start; i != end; i++) {
        re = mdg_node_resolved(*i, r->tc);
        if (re) return re;
    }
    return RE_OK;
}

int resolver_resolve_single(resolver* r, mdg_node* node)
{
    mdg_node* node_buff[2];
    node_buff[0] = node;
    node_buff[1] = NULL;
    return resolver_resolve_multiple(r, node_buff, &node_buff[1]);
}
