#include "resolver.h"
#include "thread_context.h"
#include "utils/error.h"
#include "utils/panic.h"

static resolve_error
add_simple_body_decls(resolver* r, symbol_table* parent_st, body* b);

static inline resolve_error
report_unknown_symbol(resolver* r, ast_node* n, symbol_table* st)
{
    error_log_report_annotated(
        &r->tc->error_log, ES_RESOLVER, false, "unknown symbol",
        ast_node_get_file(n, st), src_range_get_start(n->srange),
        src_range_get_end(n->srange), "use of an undefined symbol");
    return RE_UNKNOWN_SYMBOL;
}
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
static resolve_error
add_symbol(resolver* r, symbol_table* st, symbol_table* sst, ast_node* n)
{
    symbol_table* tgtst =
        (ast_node_flags_get_access_mod(n->flags) == AM_UNSPECIFIED) ? st : sst;
    symbol** conflict;
    conflict = symbol_table_insert(tgtst, (symbol*)n);
    if (conflict) {
        return report_redeclaration_error(r, (symbol*)n, *conflict, tgtst);
    }
    return RE_OK;
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
            // these guys are handled from their mdg node, not from
            // where they appear in source
            return RE_OK;
        }
        case SC_STRUCT:
        case SC_STRUCT_GENERIC:
        case SC_TRAIT:
        case SC_TRAIT_GENERIC: {
            resolve_error re = add_symbol(r, st, sst, n);
            if (re) return re;
            return add_simple_body_decls(r, st, &((scope*)n)->body);
        }

        case SC_FUNC:
        case SC_FUNC_GENERIC: {
            symbol_table* tgtst =
                (ast_node_flags_get_access_mod(n->flags) == AM_UNSPECIFIED)
                    ? st
                    : sst;
            symbol** conflict;
            conflict = symbol_table_insert(tgtst, (symbol*)n);
            if (conflict) {
                sym_func_overloaded* sfo;
                symbol* sn = (symbol*)n;
                if ((**conflict).node.kind == SC_FUNC) {
                    sfo = (sym_func_overloaded*)pool_alloc(
                        &r->tc->permmem, sizeof(sym_func_overloaded));
                    if (!sfo) return RE_FATAL;
                    sfo->symbol.node.kind = SYM_FUNC_OVERLOADED;
                    sfo->symbol.node.flags = STMT_FLAGS_DEFAULT;
                    sfo->symbol.node.srange = SRC_RANGE_INVALID;
                    sfo->symbol.next = (**conflict).next;
                    sfo->symbol.name = (**conflict).name;
                    sfo->funcs = *conflict;
                    (**conflict).next = n;
                    sn->next = NULL;
                    *conflict = sfo;
                }
                else if ((**conflict).node.kind == SYM_FUNC_OVERLOADED) {
                    sfo = (sym_func_overloaded*)conflict;
                    sn->next = sfo->funcs;
                    sfo->funcs = sn;
                }
            }
            return RE_OK;
            // not doing function bodys because they are stronly ordered
            // add_simple_body_decls(r, st, &((scope*)n)->body);
        }
        case STMT_IMPORT:
        case STMT_USING:
        case STMT_COMPOUND_ASSIGN:
            // TODO
            return RE_OK;
        case SYM_NAMED_USING:
        case SYM_VAR_DECL:
        case SYM_VAR_DECL_UNINITIALIZED: {
            return add_symbol(r, st, sst, n);
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

        case EXPR_PP: {
            symbol_table* sstpp = sst ? sst->pp_symtab : NULL;
            return add_ast_node_decls(
                r, st->pp_symtab, sstpp, ((expr_pp*)n)->pp_expr);
        }
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

resolve_error resolve_body(resolver* r, body* b);
resolve_error
resolve_type(resolver* r, ast_node* t_expr, ast_element** reduced_tgt)
{
    return RE_OK;
}

// the symbol table is not the one that contains the symbol, but the one
// where it was declared and where the type name loopup should start
resolve_error resolve_ast_node(resolver* r, ast_node* n, symbol_table* st)
{
    if (!n) return RE_OK;
    if (ast_node_flags_get_resolved(n->flags)) return RE_OK;
    if (ast_node_flags_get_resolving(n->flags)) {
        stack_push(&r->error_stack, n);
        return RE_TYPE_LOOP;
    }
    switch (n->kind) {
        case EXPR_IDENTIFIER: {
            expr_identifier* e = (expr_identifier*)n;
            symbol* s = symbol_table_lookup(st, e->value.str);
            if (!s) return report_unknown_symbol(r, n, st);
            e->value.node = (ast_node*)s;
            ast_node_flags_set_resolved(&n->flags);
            return RE_OK;
        }
        case SC_STRUCT:
        case SC_STRUCT_GENERIC:
        case SC_TRAIT:
        case SC_TRAIT_GENERIC: {
            return resolve_body(r, &((scope*)n)->body);
        }
        case SC_FUNC:
        case SC_FUNC_GENERIC: {
            // TODO: parameters
            return resolve_body(r, &((scope*)n)->body);
        }
        case STMT_USING:
        case STMT_COMPOUND_ASSIGN: {
            // TODO
            return RE_OK;
        }
        case SYM_NAMED_USING:
        case SYM_VAR_DECL: {
        }
        case SYM_VAR_DECL_UNINITIALIZED: {
            return RE_OK;
        }

        case EXPR_RETURN:
            return resolve_ast_node(r, ((expr_return*)n)->value, st);

        case EXPR_BREAK:
            return resolve_ast_node(r, ((expr_return*)n)->value, st);
        case EXPR_BLOCK: return resolve_body(r, &((expr_block*)n)->body);

        case EXPR_IF: {
            expr_if* ei = (expr_if*)n;
            resolve_error re = resolve_ast_node(r, ei->condition, st);
            if (re) return re;
            re = resolve_ast_node(r, ei->if_body, st);
            if (re) return re;
            return resolve_ast_node(r, ei->else_body, st);
        }

        case EXPR_LOOP:
            return add_simple_body_decls(r, st, &((expr_loop*)n)->body);

        case EXPR_MACRO: {
            expr_macro* em = (expr_macro*)n;
            resolve_error re = add_simple_body_decls(r, st, &em->body);
            if (re) return re;
            return resolve_ast_node(r, (ast_node*)em->next, st);
        }

        case EXPR_PP: {
            // BIG TODO
            return RE_OK;
        }
        case EXPR_MATCH: {
            expr_match* em = (expr_match*)n;
            resolve_error re = resolve_ast_node(r, em->match_expr, st);
            if (re) return re;
            for (match_arm** ma = (match_arm**)em->body.elements; *ma != NULL;
                 ma++) {
                re = resolve_ast_node(r, (**ma).condition, st);
                if (re) return re;
                re = resolve_ast_node(r, (**ma).value, st);
                if (re) return re;
            }
            return RE_OK;
        }
        default: return RE_OK;
    }
}
static inline resolve_error report_type_loop(resolver* r)
{
    // TODO
    return RE_OK;
}
resolve_error resolve_body(resolver* r, body* b)
{
    resolve_error re;
    for (ast_node** n = b->elements; *n != NULL; n++) {
        re = resolve_ast_node(r, *n, b->symtab);
        if (re) return re;
    }
    return RE_OK;
}
resolve_error resolve_body_reporting_loops(resolver* r, body* b)
{
    resolve_error re = resolve_body(r, b);
    if (re == RE_TYPE_LOOP) report_type_loop(r);
    return re;
}
resolve_error
resolver_resolve_multiple(resolver* r, mdg_node** start, mdg_node** end)
{
    r->start = start;
    r->end = end;
    print_debug_info(r);
    for (mdg_node** i = start; i != end; i++) {
        int r = symbol_table_init(
            &(**i).symtab, atomic_ureg_load(&(**i).decl_count),
            atomic_ureg_load(&(**i).using_count), true, NULL);
        if (r) return RE_FATAL;
        (**i).symtab->parent = GLOBAL_SYMTAB;
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
    for (mdg_node** i = start; i != end; i++) {
        aseglist_iterator asi;
        aseglist_iterator_begin(&asi, &(**i).open_scopes);
        for (open_scope* osc = aseglist_iterator_next(&asi); osc != NULL;
             osc = aseglist_iterator_next(&asi)) {
            osc->scope.body.symtab->parent = (**i).symtab;
            resolve_body_reporting_loops(r, &osc->scope.body);
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
    stack_fin(&r->error_stack);
}
int resolver_init(resolver* r, thread_context* tc)
{
    r->tc = tc;
    if (stack_init(&r->error_stack, &r->tc->tempmem)) return RE_FATAL;
    return OK;
}
