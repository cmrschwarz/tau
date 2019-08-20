#include "resolver.h"
#include "thread_context.h"
#include "utils/error.h"
#include "utils/panic.h"

resolve_error resolve_body(resolver* r, body* b);
static resolve_error
add_simple_body_decls(resolver* r, symbol_table* parent_st, body* b);
resolve_error
resolve_ast_node(resolver* r, ast_node* n, symbol_table* st, ast_elem** ctype);

static inline resolve_error ret_ctype(ast_elem* type, ast_elem** ctype)
{
    *ctype = type;
    return RE_OK;
}
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
add_symbol(resolver* r, symbol_table* st, symbol_table* sst, symbol* sym)
{
    sym->declaring_st = st;
    symbol_table* tgtst =
        (ast_node_flags_get_access_mod(sym->node.flags) == AM_UNSPECIFIED)
            ? st
            : sst;
    symbol** conflict;
    conflict = symbol_table_insert(tgtst, sym);
    if (conflict) {
        return report_redeclaration_error(r, sym, *conflict, tgtst);
    }
    return RE_OK;
}
static resolve_error add_ast_node_decls(
    resolver* r, symbol_table* st, symbol_table* sst, ast_node* n)
{
    if (n == NULL) return RE_OK;
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
            resolve_error re = add_symbol(r, st, sst, (symbol*)n);
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
            symbol* sym = (symbol*)n;
            sym->declaring_st = st;
            conflict = symbol_table_insert(tgtst, sym);
            if (conflict) {
                sym_func_overloaded* sfo;
                if ((**conflict).node.kind == SC_FUNC) {
                    sfo = (sym_func_overloaded*)pool_alloc(
                        &r->tc->permmem, sizeof(sym_func_overloaded));
                    if (!sfo) return RE_FATAL;
                    sfo->symbol.node.kind = SYM_FUNC_OVERLOADED;
                    sfo->symbol.node.flags = AST_NODE_FLAGS_DEFAULT;
                    sfo->symbol.node.srange = SRC_RANGE_INVALID;
                    sfo->symbol.next = (**conflict).next;
                    sfo->symbol.name = (**conflict).name;
                    sfo->funcs = (sc_func*)*conflict;
                    (**conflict).next = sym;
                    sym->next = NULL;
                    *conflict = (symbol*)sfo;
                }
                else if ((**conflict).node.kind == SYM_FUNC_OVERLOADED) {
                    sfo = (sym_func_overloaded*)conflict;
                    sym->next = (symbol*)sfo->funcs;
                    sfo->funcs = (sc_func*)n;
                }
                else {
                    return report_redeclaration_error(r, sym, *conflict, tgtst);
                }
            }

            // TODO
            // we should not be doing function bodys because they are stronly
            // ordered, this is for debugging
            // (therefore also not doing func parameters)
            return add_simple_body_decls(r, st, &((scope*)n)->body);

            return RE_OK;
        }
        case STMT_IMPORT:
        case STMT_USING:
        case STMT_COMPOUND_ASSIGN:
            // TODO
            return RE_OK;
        case SYM_NAMED_USING:
        case SYM_VAR:
        case SYM_VAR_INITIALIZED: {
            return add_symbol(r, st, sst, (symbol*)n);
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
    if (b->symtab == NULL) {
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

bool ctypes_unifiable(ast_elem* a, ast_elem* b)
{
    // immediately return true e.g. for primitives
    if (a == b) return true;
    return false; // TODO
    /*
    switch (a->kind) {
        case TYPE_MODIFIERS:
    }
     */
}

resolve_error
get_param_ctype(resolver* r, symbol_table* st, sym_param* p, ast_elem** ctype)
{
    if (!ast_node_flags_get_resolved(p->symbol.node.flags)) {
        resolve_error re =
            resolve_ast_node(r, (ast_node*)p->type, st, &p->ctype);
        *ctype = p->ctype;
        if (re) return re;
    }
    else {
        *ctype = p->ctype;
    }
    return RE_OK;
}
resolve_error operator_func_applicable(
    resolver* r, symbol_table* op_st, ast_elem* lhs, ast_elem* rhs, sc_func* f,
    bool* applicable, ast_elem** ctype)
{
    // ensure func has exactly 2 parameters
    // [varargs not allowed for operators]
    if (!f->params || !f->params->symbol.next || f->params->symbol.next->next) {
        *applicable = false;
        return RE_OK;
    }
    resolve_error re;
    ast_elem* fparam;
    re = get_param_ctype(r, op_st, f->params, &fparam);
    if (re) return re;
    if (!ctypes_unifiable(lhs, fparam)) {
        *applicable = false;
        return RE_OK;
    }
    re = get_param_ctype(r, op_st, (sym_param*)f->params->symbol.next, &fparam);
    if (re) return re;
    if (!ctypes_unifiable(rhs, fparam)) {
        *applicable = false;
        return RE_OK;
    }
    if (ast_node_flags_get_resolved(f->return_type->flags)) {
        re = resolve_ast_node(r, f->return_type, op_st, ctype);
        if (re) return re;
    }
    else {
        if (ctype) *ctype = f->return_ctype;
    }
    *applicable = true;
    return RE_OK;
}
bool func_applicable(
    resolver* r, symbol_table* fn_st, ast_elem** call_arg_types, ureg arg_count,
    sc_func* func, bool* applicable, ast_elem** ctype)
{
    // works cause varags are not in the lang yet
    if (func->param_count != arg_count) return false;
    sym_param* p = func->params;
    for (ureg i = 0; i < arg_count; i++) {
        ast_elem* param_type;
        get_param_ctype(r, fn_st, p, &param_type);
        if (!ctypes_unifiable(call_arg_types[i], param_type)) {
            *applicable = false;
            return RE_OK;
        }
        p = (sym_param*)p->symbol.next;
    }
    *applicable = true;
    return resolve_ast_node(r, func->return_type, fn_st, ctype);
}
resolve_error resolve_func_call(
    resolver* r, char* func_name, expr_call* c, symbol_table* st,
    ast_elem** ctype)
{
    ast_elem** call_arg_types =
        dbuffer_claim(&r->call_types, c->arg_count * sizeof(ast_elem*));
    for (ureg i = 0; i < c->arg_count; i++) {
        resolve_ast_node(r, c->args[i], st, &call_arg_types[i]);
    }
    symbol_table* lt = st;
    while (lt) {
        symbol_table* fn_st;
        symbol** s = symbol_table_lookup_with_decl(lt, func_name, &fn_st);
        if (!s) return report_unknown_symbol(r, c->lhs, st);
        bool applicable;
        resolve_error re;
        if ((**s).node.kind == SYM_FUNC_OVERLOADED) {
            sym_func_overloaded* sfo = (sym_func_overloaded*)s;
            sc_func* f = sfo->funcs;
            while (f) {
                re = func_applicable(
                    r, fn_st, call_arg_types, c->arg_count, f, &applicable,
                    ctype);
                if (re) return re;
                if (applicable) return RE_OK;
                f = (sc_func*)f->scope.symbol.next;
            }
        }
        else if ((**s).node.kind == SC_FUNC) {
            re = func_applicable(
                r, fn_st, call_arg_types, c->arg_count, (sc_func*)(*s),
                &applicable, ctype);
            if (re) return re;
            if (applicable) return RE_OK;
        }
        else {
            assert(false);
        }
        lt = lt->parent;
    }
    dbuffer_pop(&r->call_types, c->arg_count * sizeof(ast_elem*));
    // TODO
    return RE_OK;
}
resolve_error
resolve_call(resolver* r, expr_call* c, symbol_table* st, ast_elem** ctype)
{

    if (c->lhs->kind == EXPR_IDENTIFIER) {
        return resolve_func_call(
            r, ((expr_identifier*)c->lhs)->value.str, c, st, ctype);
    }
    else {
        assert(false); // TODO
    }
}
resolve_error choose_binary_operator_overload(
    resolver* r, expr_op_binary* ob, symbol_table* st, ast_elem** ctype)
{
    ast_elem *lhs_ctype, *rhs_ctype;
    resolve_error re = resolve_ast_node(r, ob->lhs, st, &lhs_ctype);
    if (re) return re;
    re = resolve_ast_node(r, ob->rhs, st, &rhs_ctype);
    if (re) return re;
    if (lhs_ctype->kind == PRIMITIVE && rhs_ctype->kind == PRIMITIVE) {
        // TODO: proper inbuild operator resolution
        // maybe create a cat_prim_kind and switch over cat'ed lhs and rhs
        ob->op = lhs_ctype;
        if (ctype) *ctype = lhs_ctype;
        return RE_OK;
    }
    symbol_table* lt = st;
    while (lt) {
        bool applicable;
        symbol_table* op_st;
        symbol** s = symbol_table_lookup_with_decl(
            lt, op_to_str(ob->node.operator_kind), &op_st);
        if (!s) return report_unknown_symbol(r, (ast_node*)ob, lt);
        if ((**s).node.kind == SYM_FUNC_OVERLOADED) {
            sym_func_overloaded* sfo = (sym_func_overloaded*)s;
            sc_func* f = sfo->funcs;
            while (f) {
                re = operator_func_applicable(
                    r, op_st, lhs_ctype, rhs_ctype, f, &applicable, ctype);
                if (re) return re;
                if (applicable) return RE_OK;
                f = (sc_func*)f->scope.symbol.next;
            }
        }
        else if ((**s).node.kind == SC_FUNC) {
            re = operator_func_applicable(
                r, op_st, lhs_ctype, rhs_ctype, (sc_func*)*s, &applicable,
                ctype);
            if (re) return re;
            if (applicable) return RE_OK;
        }
        lt = lt->parent;
    }
    return report_unknown_symbol(r, (ast_node*)ob, st);
}
ast_elem* get_resolved_symbol_ctype(symbol* s)
{
    switch (s->node.kind) {
        case SYM_VAR:
        case SYM_VAR_INITIALIZED: return ((sym_var*)s)->ctype; break;
        case SYM_NAMED_USING: return NULL; // TODO
        default: return (ast_elem*)s;
    }
}
// the symbol table is not the one that contains the symbol, but the one
// where it was declared and where the type name loopup should start
resolve_error
resolve_ast_node(resolver* r, ast_node* n, symbol_table* st, ast_elem** ctype)
{
    if (ast_elem_is_open_scope((ast_elem*)n)) {
        if (ctype) *ctype = (ast_elem*)n;
        return RE_OK;
    }
    if (!n) return RE_OK;
    // PERF: find a way to avoid checking in sub exprs
    bool resolved = ast_node_flags_get_resolved(n->flags);
    if (resolved) {
        if (ctype == NULL) return RE_OK;
    }
    else {
        if (ast_node_flags_get_resolving(n->flags)) {
            stack_push(&r->error_stack, n);
            return RE_TYPE_LOOP;
        }
        ast_node_flags_set_resolving(&n->flags);
    }
    resolve_error re;
    switch (n->kind) {
        case PRIMITIVE:
        case EXPR_LITERAL: {
            if (ctype) {
                *ctype = (ast_elem*)&PRIMITIVES[n->primitive_kind];
            }
            return RE_OK;
        };
        case EXPR_IDENTIFIER: {
            expr_identifier* e = (expr_identifier*)n;
            if (resolved) {
                return ret_ctype(
                    get_resolved_symbol_ctype(e->value.symbol), ctype);
            }
            symbol_table* sym_st;
            symbol** s =
                symbol_table_lookup_with_decl(st, e->value.str, &sym_st);
            if (!s) return report_unknown_symbol(r, n, st);
            e->value.symbol = *s;
            re = resolve_ast_node(r, (ast_node*)*s, sym_st, ctype);
            if (re) return re;
            ast_node_flags_set_resolved(&n->flags);
            return RE_OK;
        }
        case EXPR_OP_CALL: {
            expr_call* c = (expr_call*)n;
            if (resolved) return ret_ctype(c->target->return_ctype, ctype);
            return resolve_call(r, c, st, ctype);
        }
        case EXPR_CONTINUE:
        case EXPR_OP_UNARY: {
            // TODO
            return RE_OK;
        }
        case EXPR_OP_BINARY: {
            expr_op_binary* ob = (expr_op_binary*)n;
            if (resolved) {
                if (ob->op->kind == PRIMITIVE) {
                    *ctype = (ast_elem*)&PRIMITIVES[((ast_node*)ob->op)
                                                        ->primitive_kind];
                }
                else {
                    *ctype = ((sc_func*)ob->op)->return_ctype;
                }
                return RE_OK;
            }
            re = choose_binary_operator_overload(r, ob, st, ctype);
            if (re) return re;
            ast_node_flags_set_resolved(&n->flags);
            return RE_OK;
        }
        case SC_STRUCT:
        case SC_STRUCT_GENERIC:
        case SC_TRAIT:
        case SC_TRAIT_GENERIC: {
            if (ctype) *ctype = (ast_elem*)n;
            if (resolved) return RE_OK;
            re = resolve_body(r, &((scope*)n)->body);
            if (re) return re;
            ast_node_flags_set_resolved(&n->flags);
            return RE_OK;
        }

        case SC_FUNC:
        case SC_FUNC_GENERIC: {
            // parameters are done on the call site
            // if func is never called -> no no need to resolve the params
            if (ctype) *ctype = (ast_elem*)n;
            if (resolved) return RE_OK;
            re = resolve_body(r, &((scope*)n)->body);
            if (re) return re;
            ast_node_flags_set_resolved(&n->flags);
            return RE_OK;
        }
        case STMT_IMPORT:
        case STMT_USING:
        case SYM_NAMED_USING:
        case STMT_COMPOUND_ASSIGN: {
            // TODO
            return RE_OK;
        }
        case SYM_VAR: {
            sym_var* v = (sym_var*)n;
            if (resolved) return ret_ctype(v->ctype, ctype);
            ast_elem* type;
            re = resolve_ast_node(r, v->type, st, &type);
            if (re) return re;
            v->ctype = type;
            if (ctype) *ctype = type;
            ast_node_flags_set_resolved(&n->flags);
            return RE_OK;
        }
        case SYM_VAR_INITIALIZED: {
            sym_var_initialized* vi = (sym_var_initialized*)n;
            if (resolved) return ret_ctype(vi->var.ctype, ctype);
            if (vi->var.type) {
                re = resolve_ast_node(r, vi->var.type, st, &vi->var.ctype);
                if (re) return re;
                if (ctype) *ctype = vi->var.ctype;
                ast_elem* val_type;
                re = resolve_ast_node(r, vi->initial_value, st, &val_type);
                if (re) return re;
                // TODO: make assert(val_type == ctype)
            }
            else {
                re = resolve_ast_node(r, vi->initial_value, st, &vi->var.ctype);
                if (re) return re;
                if (ctype) *ctype = vi->var.ctype;
            }
            ast_node_flags_set_resolved(&n->flags);
            return RE_OK;
        }

        case EXPR_RETURN: {
            // TODO ctype on resolved
            re = resolve_ast_node(r, ((expr_return*)n)->value, st, NULL);
            if (re) return re;
            ast_node_flags_set_resolved(&n->flags);
            return RE_OK;
        }
        case EXPR_BREAK: {
            // TODO ctype
            if (ctype) *ctype = NULL;
            if (resolved) return RE_OK;
            re = resolve_ast_node(r, ((expr_return*)n)->value, st, NULL);
            if (re) return re;
            ast_node_flags_set_resolved(&n->flags);
            return RE_OK;
        }
        case EXPR_BLOCK: {
            // TODO ctype
            if (ctype) *ctype = NULL;
            if (resolved) return RE_OK;
            re = resolve_body(r, &((expr_block*)n)->body);
            if (re) return re;
            ast_node_flags_set_resolved(&n->flags);
            return RE_OK;
        }

        case EXPR_IF: {
            // TODO ctype
            if (ctype) *ctype = NULL;
            if (resolved) return RE_OK;
            expr_if* ei = (expr_if*)n;
            resolve_error re = resolve_ast_node(r, ei->condition, st, NULL);
            if (re) return re;
            re = resolve_ast_node(r, ei->if_body, st, NULL);
            if (re) return re;
            return resolve_ast_node(r, ei->else_body, st, NULL);
        }

        case EXPR_LOOP:
            // TODO ctype
            if (ctype) *ctype = NULL;
            if (resolved) return RE_OK;
            return add_simple_body_decls(r, st, &((expr_loop*)n)->body);

        case EXPR_MACRO: {
            // TODO ctype
            if (ctype) *ctype = NULL;
            if (resolved) return RE_OK;
            expr_macro* em = (expr_macro*)n;
            resolve_error re = add_simple_body_decls(r, st, &em->body);
            if (re) return re;
            return resolve_ast_node(r, (ast_node*)em->next, st, NULL);
        }

        case EXPR_PP: {
            // BIG TODO
            if (ctype) *ctype = NULL;
            return RE_OK;
        }
        case EXPR_MATCH: {
            // TODO ctype
            if (ctype) *ctype = NULL;
            if (resolved) return RE_OK;
            expr_match* em = (expr_match*)n;
            resolve_error re = resolve_ast_node(r, em->match_expr, st, NULL);
            if (re) return re;
            for (match_arm** ma = (match_arm**)em->body.elements; *ma != NULL;
                 ma++) {
                re = resolve_ast_node(r, (**ma).condition, st, NULL);
                if (re) return re;
                re = resolve_ast_node(r, (**ma).value, st, NULL);
                if (re) return re;
            }
            return RE_OK;
        }
        default: assert(false); return RE_UNKNOWN_SYMBOL;
    }
}
static inline void report_type_loop(resolver* r)
{
    // TODO
    assert(false);
}
resolve_error resolve_body(resolver* r, body* b)
{
    resolve_error re;
    for (ast_node** n = b->elements; *n != NULL; n++) {
        re = resolve_ast_node(r, *n, b->symtab, NULL);
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
    resolve_error re;
    print_debug_info(r);
    for (mdg_node** i = start; i != end; i++) {
        int r = symbol_table_init(
            &(**i).symtab, atomic_ureg_load(&(**i).decl_count),
            atomic_ureg_load(&(**i).using_count), true, NULL);
        if (r) return RE_FATAL;
        if (!(**i).symtab) return RE_FATAL;
        (**i).symtab->parent = GLOBAL_SYMTAB;
    }
    for (mdg_node** i = start; i != end; i++) {
        aseglist_iterator asi;
        aseglist_iterator_begin(&asi, &(**i).open_scopes);
        for (open_scope* osc = aseglist_iterator_next(&asi); osc != NULL;
             osc = aseglist_iterator_next(&asi)) {
            re =
                add_body_decls(r, (**i).symtab, (**i).symtab, &osc->scope.body);
            if (re) return re;
        }
    }
    for (mdg_node** i = start; i != end; i++) {
        aseglist_iterator asi;
        aseglist_iterator_begin(&asi, &(**i).open_scopes);
        for (open_scope* osc = aseglist_iterator_next(&asi); osc != NULL;
             osc = aseglist_iterator_next(&asi)) {
            osc->scope.body.symtab->parent = (**i).symtab;
            re = resolve_body_reporting_loops(r, &osc->scope.body);
            if (re) return re;
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
    dbuffer_fin(&r->call_types);
    stack_fin(&r->error_stack);
}
int resolver_init(resolver* r, thread_context* tc)
{
    r->tc = tc;
    if (stack_init(&r->error_stack, &r->tc->tempmem)) return ERR;
    if (dbuffer_init(&r->call_types)) {
        stack_fin(&r->error_stack);
        return ERR;
    }
    return OK;
}
