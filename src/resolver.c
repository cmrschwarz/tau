#include "resolver.h"
#include "thread_context.h"
#include "utils/error.h"
#include "utils/panic.h"

static resolve_error
add_simple_body_decls(resolver* r, symbol_table* parent_st, body* b);
static resolve_error
add_expr_decls(resolver* r, symbol_table* st, symbol_table* sst, expr* e)
{
    if (e == NULL) return RE_OK;
    switch (e->kind) {
        case EXPR_RETURN:
            return add_expr_decls(r, st, sst, ((expr_return*)e)->value);

        case EXPR_BREAK:
            return add_expr_decls(r, st, sst, ((expr_break*)e)->value);

        case EXPR_BLOCK:
            return add_simple_body_decls(r, st, &((expr_block*)e)->body);

        case EXPR_IF: {
            expr_if* ei = (expr_if*)e;
            resolve_error re = add_expr_decls(r, st, sst, ei->condition);
            if (re) return re;
            re = add_expr_decls(r, st, sst, ei->if_body);
            if (re) return re;
            return add_expr_decls(r, st, sst, ei->else_body);
        }

        case EXPR_LOOP:
            return add_simple_body_decls(r, st, &((expr_loop*)e)->body);

        case EXPR_DO:
            return add_expr_decls(r, st, sst, ((expr_do*)e)->expr_body);

        case EXPR_DO_WHILE: {
            expr_do_while* edw = (expr_do_while*)e;
            resolve_error re = add_expr_decls(r, st, sst, edw->condition);
            if (re) return re;
            re = add_simple_body_decls(r, st, &edw->do_body);
            if (re) return re;
            return add_simple_body_decls(r, st, &edw->finally_body);
        }

        case EXPR_WHILE: {
            expr_while* ew = (expr_while*)e;
            resolve_error re = add_expr_decls(r, st, sst, ew->condition);
            if (re) return re;
            re = add_simple_body_decls(r, st, &ew->while_body);
            if (re) return re;
            return add_simple_body_decls(r, st, &ew->finally_body);
        }

        case EXPR_MACRO: {
            expr_macro* em = (expr_macro*)e;
            resolve_error re = add_simple_body_decls(r, st, &em->body);
            if (re) return re;
            return add_expr_decls(r, st, sst, (expr*)em->next);
        }

        case EXPR_PP: return add_expr_decls(r, st, sst, ((expr_pp*)e)->child);

        case EXPR_MATCH: {
            expr_match* em = (expr_match*)e;
            resolve_error re = add_expr_decls(r, st, sst, em->match_expr);
            if (re) return re;
            for (match_arm** ma = em->match_arms; *ma != NULL; ma++) {
                re = add_expr_decls(r, st, sst, (**ma).condition);
                if (re) return re;
                re = add_expr_decls(r, st, sst, (**ma).value);
                if (re) return re;
            }
            return RE_OK;
        }
        default: return RE_OK;
    }
}
static resolve_error
add_stmt_list_decls(resolver* r, symbol_table* st, symbol_table* sst, stmt** sl)
{
    resolve_error re;

    while (*sl) {
        stmt* s = *sl;
        // TODO: handle actual decls, lol
        if (ast_node_is_scope((ast_node*)s)) {
            // these are parts of a module and therefore already handled
            if (!ast_node_is_open_scope((ast_node*)s)) {
                re = add_simple_body_decls(r, st, &((scope*)s)->body);
                if (re) return re;
            }
            sl = &(*sl)->next;
        }
        else if (s->node.kind == STMT_EXPRESSION) {
            re = add_expr_decls(r, st, sst, ((stmt_expr*)s)->expr);
            if (re) return re;
            sl = &(*sl)->next;
        }
        else {
            switch (s->node.kind) {
                case SYM_VAR_DECL:
                case SYM_VAR_DECL_UNINITIALIZED: {
                    symbol_table* tgtst = st;
                    symbol* conflict;
                    *sl = s->next;
                    if (sst && stmt_flags_get_access_mod(s->node.flags) !=
                                   AM_UNSPECIFIED) {
                        tgtst = sst;
                    }
                    conflict = symbol_table_insert(tgtst, (symbol*)s);
                    if (conflict) {
                        error_log_report_annotated_twice(
                            &r->tc->error_log, ES_RESOLVER, false,
                            "symbol redeclaration",
                            ast_node_get_file((ast_node*)s, tgtst),
                            src_range_get_start(s->node.srange),
                            src_range_get_end(s->node.srange),
                            "a symbol of this name is already defined in this "
                            "scope",
                            ast_node_get_file((ast_node*)conflict, tgtst),
                            src_range_get_start(conflict->stmt.node.srange),
                            src_range_get_end(conflict->stmt.node.srange),
                            "previous definition here");
                        return RE_SYMBOL_REDECLARATION;
                    }
                } break;
                case STMT_IMPORT:
                case STMT_USING:
                case SYM_NAMED_USING:
                case SYM_PARAM:
                case STMT_PP_STMT:
                case STMT_COMPOUND_ASSIGN: sl = &(*sl)->next; break; // TODO
                default: assert(false); // unknown node_kind
            }
        }
    }
    return RE_OK;
}
static resolve_error
add_simple_body_decls(resolver* r, symbol_table* parent_st, body* b)
{
    if (b->symtab == NULL) {
        b->symtab = parent_st;
    }
    else {
        b->symtab->parent = parent_st;
    }
    return add_stmt_list_decls(r, b->symtab, NULL, &b->children);
}
static inline resolve_error mark_mdg_nodes_resolved(resolver* r)
{
    for (mdg_node** i = r->start; i != r->end; i++) {
        resolve_error re = mdg_node_resolved(*i, r->tc);
        if (re) return re;
    }
    return RE_OK;
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

resolve_error
resolver_resolve_multiple(resolver* r, mdg_node** start, mdg_node** end)
{
    r->start = start;
    r->end = end;
    print_debug_info(r);
    for (mdg_node** i = start; i != end; i++) {
        (**i).symtab = symbol_table_new(
            atomic_ureg_load(&(**i).decl_count),
            atomic_ureg_load(&(**i).using_count), NULL);
        (**i).symtab->parent = NULL;
        if (!(**i).symtab) return RE_FATAL;
    }
    for (mdg_node** i = start; i != end; i++) {
        aseglist_iterator asi;
        aseglist_iterator_begin(&asi, &(**i).open_scopes);
        for (open_scope* osc = aseglist_iterator_next(&asi); osc != NULL;
             osc = aseglist_iterator_next(&asi)) {
            osc->scope.body.symtab->parent = (**i).symtab;
            add_stmt_list_decls(
                r, osc->scope.body.symtab, (**i).symtab,
                &osc->scope.body.children);
        }
    }
    return mark_mdg_nodes_resolved(r);
}

int resolver_resolve_single(resolver* r, mdg_node* node)
{
    mdg_node* node_buff[2];
    node_buff[0] = node;
    node_buff[1] = NULL;
    return resolver_resolve_multiple(r, node_buff, &node_buff[1]);
}
void resolver_fin(resolver* r)
{
}
int resolver_init(resolver* r, thread_context* tc)
{
    r->tc = tc;
    return OK;
}
