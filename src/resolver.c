#include "resolver.h"
#include "thread_context.h"
#include "utils/error.h"
#include "utils/panic.h"

static resolve_error
add_simple_body_decls(resolver* r, symbol_table* parent_st, body* b);

static resolve_error report_redeclaration_error(
    resolver* r, symbol* redecl, symbol* first, symbol_table* st)
{
    error_log_report_annotated_twice(
        &r->tc->error_log, ES_RESOLVER, false, "symbol redeclaration",
        ast_node_get_file((ast_node*)redecl, st),
        src_range_get_start(redecl->node.srange),
        src_range_get_end(redecl->node.srange),
        "a symbol of this name is already defined in this "
        "scope",
        ast_node_get_file((ast_node*)first, st),
        src_range_get_start(first->node.srange),
        src_range_get_end(first->node.srange), "previous definition here");
    return RE_SYMBOL_REDECLARATION;
}
static resolve_error add_ast_node_decls(
    resolver* r, symbol_table* st, symbol_table* sst, ast_node* n)
{
    if (n == NULL) return RE_OK;
    if (ast_node_is_scope(n)) {
        // these are parts of a module and therefore already handled
        if (ast_node_is_open_scope((ast_node*)n)) return RE_OK;
        return add_simple_body_decls(r, st, &((scope*)n)->body);
    }
    switch (n->kind) {
        case OSC_MODULE:
        case OSC_MODULE_GENERIC:
        case OSC_EXTEND:
        case OSC_EXTEND_GENERIC: {
            return RE_OK;
        }
        case SC_STRUCT:
        case SC_STRUCT_GENERIC:
        case SC_TRAIT:
        case SC_TRAIT_GENERIC: {
            return add_simple_body_decls(r, st, &((scope*)n)->body);
        }

        case SC_FUNC:
        case SC_FUNC_GENERIC: {
        }
        case STMT_IMPORT: {
        }
        case STMT_USING: {
        }
        case SYM_NAMED_USING:
        case SYM_PARAM:
        case STMT_COMPOUND_ASSIGN:
        case SYM_VAR_DECL: return RE_OK;
        case SYM_VAR_DECL_UNINITIALIZED: {
            symbol_table* tgtst =
                (ast_node_flags_get_access_mod(n->flags) == AM_UNSPECIFIED)
                    ? st
                    : sst;
            symbol* conflict;
            conflict = symbol_table_insert(tgtst, (symbol*)n);
            if (conflict) {
                return report_redeclaration_error(
                    r, (symbol*)n, conflict, tgtst);
            }
            return RE_OK;
        }

        case EXPR_RETURN:
            return add_ast_node_decls(r, st, sst, ((expr_return*)n)->value);

        case EXPR_BREAK:
            return add_ast_node_decls(r, st, sst, ((expr_break*)n)->value);

        case EXPR_BLOCK:
            return add_simple_body_decls(r, st, &((expr_block*)n)->body);

        case EXPR_IF: {
            expr_if* ei = (expr_if*)n;
            resolve_error re = add_ast_node_decls(r, st, sst, ei->condition);
            if (re) return re;
            re = add_ast_node_decls(r, st, sst, ei->if_body);
            if (re) return re;
            return add_ast_node_decls(r, st, sst, ei->else_body);
        }

        case EXPR_LOOP:
            return add_simple_body_decls(r, st, &((expr_loop*)n)->body);

        case EXPR_MACRO: {
            expr_macro* em = (expr_macro*)n;
            resolve_error re = add_simple_body_decls(r, st, &em->body);
            if (re) return re;
            return add_ast_node_decls(r, st, sst, (ast_node*)em->next);
        }

        case EXPR_PP:
            return add_ast_node_decls(r, st, sst, ((expr_pp*)n)->pp_expr);

        case EXPR_MATCH: {
            expr_match* em = (expr_match*)n;
            resolve_error re = add_ast_node_decls(r, st, sst, em->match_expr);
            if (re) return re;
            for (match_arm** ma = (match_arm**)em->body.elements; *ma != NULL;
                 ma++) {
                re = add_ast_node_decls(r, st, sst, (**ma).condition);
                if (re) return re;
                re = add_ast_node_decls(r, st, sst, (**ma).value);
                if (re) return re;
            }
            return RE_OK;
        }
        default:
            return RE_OK; // TODO
            assert(false); // unknown node_kind
    }
    assert(false);
}

static resolve_error add_body_decls(
    resolver* r, symbol_table* parent_st, symbol_table* shared_st, body* b)
{
    if (b->symtab == &EMPTY_ST) {
        b->symtab = parent_st;
    }
    else {
        b->symtab->parent = parent_st;
    }
    for (ast_node** n = b->elements; *n; n++) {
        resolve_error re = add_ast_node_decls(r, b->symtab, shared_st, *n);
        if (re) return re;
    }
    return RE_OK;
}
static resolve_error
add_simple_body_decls(resolver* r, symbol_table* parent_st, body* b)
{
    return add_body_decls(r, parent_st, NULL, b);
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
            atomic_ureg_load(&(**i).using_count), true, NULL);
        //(**i).symtab->parent = &EMPTY_ST;
        if (!(**i).symtab) return RE_FATAL;
    }
    for (mdg_node** i = start; i != end; i++) {
        aseglist_iterator asi;
        aseglist_iterator_begin(&asi, &(**i).open_scopes);
        for (open_scope* osc = aseglist_iterator_next(&asi); osc != NULL;
             osc = aseglist_iterator_next(&asi)) {
            osc->scope.body.symtab->parent = (**i).symtab;
            add_body_decls(r, NULL, (**i).symtab, &osc->scope.body);
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
