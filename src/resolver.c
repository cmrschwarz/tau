#include "resolver.h"
#include "thread_context.h"
#include "utils/error.h"
#include "utils/panic.h"

static resolve_error add_body_decls(src_file* f, symbol_table* sst, body* b);
static resolve_error
add_expr_decls(src_file* f, symbol_table* st, symbol_table* sst, expr* e)
{
    if (e == NULL) return;
    switch (e->kind) {
        case EXPR_RETURN:
            return add_expr_decls(f, st, sst, ((expr_return*)e)->value);

        case EXPR_BREAK:
            return add_expr_decls(f, st, sst, ((expr_break*)e)->value);

        case EXPR_BLOCK:
            return add_body_decls(f, NULL, &((expr_block*)e)->body);

        case EXPR_IF: {
            expr_if* ei = (expr_if*)e;
            resolve_error r = add_expr_decls(f, st, sst, ei->condition);
            if (r) return r;
            r = add_expr_decls(f, st, sst, ei->if_body);
            if (r) return r;
            return add_expr_decls(f, st, sst, ei->else_body);
        }

        case EXPR_LOOP: return add_body_decls(f, NULL, &((expr_loop*)e)->body);

        case EXPR_DO:
            return add_expr_decls(f, st, sst, ((expr_do*)e)->expr_body);

        case EXPR_DO_WHILE: {
            expr_do_while* edw = (expr_do_while*)e;
            resolve_error r = add_expr_decls(f, st, sst, edw->condition);
            if (r) return r;
            r = add_body_decls(f, NULL, &edw->do_body);
            if (r) return r;
            return add_body_decls(f, NULL, &edw->finally_body);
        }

        case EXPR_WHILE: {
            expr_while* ew = (expr_while*)e;
            resolve_error r = add_expr_decls(f, st, sst, ew->condition);
            if (r) return r;
            r = add_body_decls(f, NULL, &ew->while_body);
            if (r) return r;
            return add_body_decls(f, NULL, &ew->finally_body);
        }

        case EXPR_MACRO: {
            expr_macro* em = (expr_macro*)e;
            resolve_error r = add_body_decls(f, NULL, &em->body);
            if (r) return r;
            return add_expr_decls(f, st, sst, (expr*)em->next);
        }

        case EXPR_PP: return add_expr_decls(f, st, sst, ((expr_pp*)e)->child);

        case EXPR_MATCH: {
            expr_match* em = (expr_match*)e;
            resolve_error r = add_expr_decls(f, st, sst, em->match_expr);
            if (r) return r;
            for (match_arm** ma = em->match_arms; *ma != NULL; ma++) {
                r = add_expr_decls(f, st, sst, (**ma).condition);
                if (r) return r;
                r = add_expr_decls(f, st, sst, (**ma).value);
                if (r) return r;
            }
            return RE_OK;
        }
        default: return RE_OK;
    }
}
static resolve_error
add_stmt_decls(src_file* f, symbol_table* st, symbol_table* sst, stmt* s)
{
    // TODO: handle actual decls, lol
    if (ast_node_is_scope((ast_node*)s)) {
        // these are parts of a module and therefore already handled
        if (!ast_node_is_open_scope((ast_node*)s)) {
            return add_body_decls(f, NULL, &((scope*)s)->body);
        }
        else {
            return RE_OK;
        }
    }
    else if (s->node.kind == STMT_EXPRESSION) {
        return add_expr_decls(f, st, sst, ((stmt_expr*)s)->expr);
    }
}
static resolve_error add_body_decls(src_file* f, symbol_table* sst, body* b)
{
    resolve_error re;
    for (stmt* s = b->children; s != NULL; s = s->next) {
        re = add_stmt_decls(f, b->symtab, sst, s);
        if (re) return re;
    }
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
            atomic_ureg_load(&(**i).using_count));
        if (!(**i).symtab) return RE_FATAL;
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
