#include "resolver.h"
#include "thread_context.h"
#include "utils/error.h"
#include "utils/panic.h"

resolve_error add_delarations(
    resolver* r, symbol_table* sst, symbol_store* ss, src_file* f,
    stmt** stmt_list);
static inline resolve_error
resolve_ast_node(resolver* r, stack* s, symbol* p, ast_node* n);

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
resolve_error
st_add_sym(resolver* r, symbol_table* st, src_file* f, symbol* sym)
{
    symbol* res = symbol_table_insert(st, sym);
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
resolve_error sst_add_sym(resolver* r, symbol_table* sst, symbol* sym)
{
    symbol* res = symbol_table_insert(sst, sym);
    if (res) {
        src_range_large sr_new, sr_old;
        src_range_unpack(sym->stmt.srange, &sr_new);
        src_range_unpack(res->stmt.srange, &sr_old);
        return symbol_redefinition_error(r, sr_old, sr_new);
    }
    return RE_OK;
}
resolve_error expr_add_declarations(resolver* r, expr* ex, src_file* f)
{
    switch (ex->kind) {
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
            return add_delarations(
                r, NULL, &((expr_block*)ex)->body.ss, f,
                &((expr_block*)ex)->body.children);
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
resolve_error add_delarations(
    resolver* r, symbol_table* sst, symbol_store* ss, src_file* f,
    stmt** stmt_list)
{
    resolve_error re = symbol_store_setup_table(ss);
    if (re) return re;
    symbol_table* st = ss->table;
    stmt* si = *stmt_list;
    while (si) {
        if (ast_node_is_symbol((ast_node*)si)) {
            symbol* sym = (symbol*)si;
            if (!sst ||
                stmt_flags_get_access_mod(sym->stmt.flags) == AM_SCOPE_LOCAL) {
                re = st_add_sym(r, st, f, sym);
            }
            else {
                re = sst_add_sym(r, sst, sym);
            }
            if (re) break;
        }
        else if (si->kind == STMT_PP_STMT) {
            stmt_pp_stmt* pps = (stmt_pp_stmt*)si;
            if (ast_node_is_symbol((ast_node*)pps->pp_stmt)) {
                symbol* sym = (symbol*)pps->pp_stmt;
                if (!sst ||
                    stmt_flags_get_access_mod(sym->stmt.flags) ==
                        AM_SCOPE_LOCAL) {
                    symbol_store_inc_decl_count(&st->ppst, 1);
                }
                else {
                    symbol_store_inc_decl_count(&sst->ppst, 1);
                }
            }
        }
        else if (si->kind == STMT_USING) {
            stmt_using* su = (stmt_using*)si;
            access_modifier am = stmt_flags_get_access_mod(su->stmt.flags);
            if (am == AM_SCOPE_LOCAL) {
                su->stmt.next = st->usings;
                st->usings = (stmt*)su;
            }
            else {
                su->stmt.next = sst->usings;
                sst->usings = (stmt*)su;
            }
        }
        else {
            *stmt_list = si->next;
            stmt_list = &si->next;
        }
        si = si->next;
    }
    *stmt_list = NULL;
    return re;
}
static inline resolve_error add_resolve_group_declarations(resolver* r)
{
    resolve_error re = RE_OK;
    mdg_node** i;
    for (i = r->start; i != r->end; i++) {
        symbol_store_init(&(**i).ss);
        aseglist_iterator tgti;
        aseglist_iterator_begin(&tgti, &(**i).targets);
        open_scope* osci = aseglist_iterator_next(&tgti);
        while (osci) {
            symbol_store_merge_decls(&(**i).ss, osci->shared_decl_count);
            osci = aseglist_iterator_next(&tgti);
        }
        if (symbol_store_setup_table(&(**i).ss)) return RE_FATAL;
        aseglist_iterator_begin(&tgti, &(**i).targets);
        while (true) {
            osci = aseglist_iterator_next(&tgti);
            if (!osci) break;
            re = add_delarations(
                r, (**i).ss.table, &osci->scope.body.ss,
                open_scope_get_file(osci), &osci->scope.body.children);
            if (re) break;
        }
        if (re) {
            while (true) {
                osci = aseglist_iterator_next(&tgti);
                if (!osci) break;
                // to prevent the uninitialized tables from being freed
                osci->scope.body.ss.table = NULL;
            }
        }
    }
    if (re) {
        for (mdg_node** j = i + 1; j != r->end; j++) {
            (**j).ss.table = NULL;
        }
        return re;
    }
    return re;
}
static inline void print_debug_info(resolver* r)
{
    printf("resolving {");
    mdg_node** i = r->start;
    i = r->start;
    while (i + 1 != r->end) {
        printf("%s, ", (**i).name);
        i++;
    }
    printf("%s}\n", (**i).name);
}
static inline resolve_error
resolve_var(resolver* r, stack* s, symbol* p, sym_var* sv)
{
    if (sv->type == NULL) {
        resolve_error re = resolve_ast_node(r, s, p, (ast_node*)sv->value);
        if (re != RE_OK) return re;
    }
    return RE_OK;
}
static inline resolve_error
resolve_ast_node(resolver* r, stack* s, symbol* p, ast_node* n)
{
    switch (*(ast_node_kind*)n) {
        case STMT_EXPRESSION:
            stack_set(s, ((stmt_expr*)n)->expr);
            return resolve_ast_node(r, s, p, (ast_node*)((stmt_expr*)n)->expr);
        case SYM_VAR: return resolve_var(r, s, p, (sym_var*)n);
        default:
            stack_pop(s);
            stack_pop(s);
            return RE_OK;
    }
}
static inline resolve_error resolve_stack(resolver* r, stack* s)
{
    while (true) {
        ast_node* next = stack_peek(s);
        if (!next) return RE_OK;
        symbol* parent = stack_peek_prev(s);
        resolve_error re = resolve_ast_node(r, s, parent, next);
        if (re != RE_OK) return re;
    }
}
static inline resolve_error handle_preprocessor(resolver* r)
{
    ureg stacks_remaining = 0;
    sbuffer sb;
    sbi sbi;
    sbuffer_init(&sb, sizeof(stack) * 8);
    sbi_begin(&sbi, &sb);
    for (mdg_node** i = r->start; i != r->end; i++) {
        aseglist_iterator mdgn_tgts_it;
        aseglist_iterator_begin(&mdgn_tgts_it, &(**i).targets);
        while (true) {
            open_scope* osci = aseglist_iterator_next(&mdgn_tgts_it);
            if (!osci) break;
            if (osci->scope.body.children) {
                stack* st = sbi_get(&sbi, sizeof(stack));
                if (!st || !stack_is_empty(st)) {
                    st = (stack*)sbuffer_append(&sb, sizeof(stack));
                    stack_init(st, &r->tc->tempmem);
                }
                stack_push(st, osci);
                stack_push(st, osci->scope.body.children);
                stacks_remaining++;
                resolve_error re = resolve_stack(r, st);
                if (re == RE_UNKNOWN_SYMBOL) {
                    stack* st2;
                    while (true) {
                        st2 = (stack*)sbi_next(&sbi, sizeof(stack));
                        if (st2 == NULL) {
                            sbi_begin(&sbi, &sb);
                            continue;
                        }
                        if (st2 == st) break;
                        re = resolve_stack(r, st2);
                        if (re == RE_UNKNOWN_SYMBOL) continue;
                        if (re == OK) {
                            stack tmp = *st2;
                            *st2 = *st;
                            *st = tmp;
                            // something was resolved -> try each one again
                            st = st2;
                        }
                        else {
                            return re;
                        }
                    }
                }
                else if (re == RE_OK) {
                    stacks_remaining--;
                }
                else {
                    return re;
                }
            }
        }
    }
    if (stacks_remaining > 0) {
        panic("unresolved preprocessor statements!");
    }
    while (true) {
        stack* s = sbi_next(&sbi, sizeof(stack));
        if (!s) break;
    }
    sbuffer_fin(&sb);
    return RE_OK;
}
static inline resolve_error mark_mdg_nodes_resolved(resolver* r)
{
    for (mdg_node** i = r->start; i != r->end; i++) {
        resolve_error re = mdg_node_resolved(*i, r->tc);
        if (re) return re;
    }
    return RE_OK;
}

resolve_error
resolver_resolve_multiple(resolver* r, mdg_node** start, mdg_node** end)
{
    r->start = start;
    r->end = end;
    resolve_error re;
    print_debug_info(r);
    re = add_resolve_group_declarations(r);
    if (re) return re;
    re = handle_preprocessor(r);
    if (re) return re;
    re = mark_mdg_nodes_resolved(r);
    return re;
}

int resolver_resolve_single(resolver* r, mdg_node* node)
{
    mdg_node* node_buff[2];
    node_buff[0] = node;
    node_buff[1] = NULL;
    return resolver_resolve_multiple(r, node_buff, &node_buff[1]);
}
