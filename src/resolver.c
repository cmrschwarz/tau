#include "resolver.h"
#include "thread_context.h"
#include "utils/error.h"
#include "utils/panic.h"
#include "utils/zero.h"

resolve_error resolve_body(resolver* r, body* b);
static resolve_error
add_simple_body_decls(resolver* r, symbol_table* parent_st, body* b);
resolve_error
resolve_ast_node(resolver* r, ast_node* n, symbol_table* st, ast_elem** ctype);
resolve_error resolve_func(resolver* r, sc_func* fn, symbol_table* parent_st);
resolve_error resolve_expr_body(
    resolver* r, ast_node* expr, body* b, symbol_table* parent_st);
static inline resolve_error ret_ctype(ast_elem* type, ast_elem** ctype)
{
    *ctype = type;
    return RE_OK;
}
static inline resolve_error
report_unknown_symbol_raw(resolver* r, src_file* f, src_range range)
{
    error_log_report_annotated(
        &r->tc->error_log, ES_RESOLVER, false, "unknown symbol", f,
        src_range_get_start(range), src_range_get_end(range),
        "use of an undefined symbol");
    return RE_UNKNOWN_SYMBOL;
}
static inline resolve_error
report_unknown_symbol(resolver* r, ast_node* n, symbol_table* st)
{
    return report_unknown_symbol_raw(r, ast_node_get_file(n, st), n->srange);
}
static resolve_error report_redeclaration_error_raw(
    thread_context* tc, symbol* redecl, src_file* redecl_file, symbol* prev,
    src_file* prev_file)
{
    error_log_report_annotated_twice(
        &tc->error_log, ES_RESOLVER, false, "symbol redeclaration", redecl_file,
        src_range_get_start(redecl->node.srange),
        src_range_get_end(redecl->node.srange),
        "a symbol of this name is already defined in this "
        "scope",
        prev_file, src_range_get_start(prev->node.srange),
        src_range_get_end(prev->node.srange), "previous definition here");
    return RE_SYMBOL_REDECLARATION;
}
static resolve_error report_redeclaration_error(
    resolver* r, symbol* redecl, symbol* prev, symbol_table* st)
{
    return report_redeclaration_error_raw(
        r->tc, redecl, ast_node_get_file((ast_node*)redecl, st), prev,
        ast_node_get_file((ast_node*)prev, st));
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
    resolve_error re;
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
            re = add_symbol(r, st, sst, (symbol*)n);
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
            // we don't do function bodys here because they are strongly ordered
            // (vars can't be used before declaration)
            return RE_OK;
        }
        case SYM_IMPORT_GROUP: {
            sym_import_group* ig = (sym_import_group*)n;
            if (ig->symbol.name) {
                return add_symbol(r, st, sst, (symbol*)ig);
            }
            else {
                return add_import_group_decls(
                    r->tc, ast_node_get_file((ast_node*)ig, st), ig, st);
            }
        }
        case SYM_IMPORT_MODULE: {
            return add_symbol(r, st, sst, (symbol*)n);
        }
        case STMT_USING:
        case STMT_COMPOUND_ASSIGN:
            // TODO
            return RE_OK;

        case SYM_VAR: return add_symbol(r, st, sst, (symbol*)n);
        case SYM_NAMED_USING: {
            re = add_symbol(r, st, sst, (symbol*)n);
            if (re) return re;
            return add_ast_node_decls(
                r, st, sst, ((sym_named_using*)n)->target);
        }
        case SYM_VAR_INITIALIZED: {
            re = add_symbol(r, st, sst, (symbol*)n);
            if (re) return re;
            return add_ast_node_decls(
                r, st, sst, ((sym_var_initialized*)n)->initial_value);
        }

        case EXPR_RETURN:
        case EXPR_BREAK:
            return add_ast_node_decls(r, st, sst, ((expr_break*)n)->value);

        case EXPR_BLOCK:
            return add_simple_body_decls(r, st, &((expr_block*)n)->body);

        case EXPR_IF: {
            expr_if* ei = (expr_if*)n;
            re = add_ast_node_decls(r, st, sst, ei->condition);
            if (re) return re;
            re = add_ast_node_decls(r, st, sst, ei->if_body);
            if (re) return re;
            return add_ast_node_decls(r, st, sst, ei->else_body);
        }

        case EXPR_LOOP:
            return add_simple_body_decls(r, st, &((expr_loop*)n)->body);

        case EXPR_MACRO: {
            expr_macro* em = (expr_macro*)n;
            re = add_simple_body_decls(r, st, &em->body);
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
            re = add_ast_node_decls(r, st, sst, em->match_expr);
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
        case EXPR_OP_BINARY: {
            re = add_ast_node_decls(r, st, sst, ((expr_op_binary*)n)->lhs);
            if (re) return re;
            return add_ast_node_decls(r, st, sst, ((expr_op_binary*)n)->rhs);
        }
        case EXPR_ACCESS: {
            expr_access* a = (expr_access*)n;
            re = add_ast_node_decls(r, st, sst, a->lhs);
            if (re) return re;
            for (ureg i = 0; i < a->arg_count; i++) {
                re = add_ast_node_decls(r, st, sst, a->args[i]);
                if (re) return re;
            }
            // we could make this tail recursive by moving lhs down here
            // but this way we traverse as declared ->better cache usage
            return RE_OK;
        }
        case EXPR_CALL: {
            expr_call* c = (expr_call*)n;
            re = add_ast_node_decls(r, st, sst, c->lhs);
            if (re) return re;
            for (ureg i = 0; i < c->arg_count; i++) {
                re = add_ast_node_decls(r, st, sst, c->args[i]);
                if (re) return re;
            }
            return RE_OK;
        }
        case EXPR_SCOPE_ACCESS:
        case EXPR_MEMBER_ACCESS: {
            return add_ast_node_decls(r, st, sst, ((expr_scope_access*)n)->lhs);
        }
        case EXPR_PARENTHESES: {
            return add_ast_node_decls(
                r, st, sst, ((expr_parentheses*)n)->child);
        }
        case EXPR_OP_UNARY: {
            return add_ast_node_decls(r, st, sst, ((expr_op_unary*)n)->child);
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
resolve_error add_import_group_decls(
    thread_context* tc, src_file* f, sym_import_group* ig, symbol_table* st)
{
    symbol* next = ig->children.symbols;
    symbol* s;
    while (next != NULL) {
        s = next;
        next = s->next;
        if (s->node.kind == SYM_IMPORT_GROUP) {
            sym_import_group* nig = (sym_import_group*)s;
            if (!nig->symbol.name) {
                resolve_error re = add_import_group_decls(tc, f, nig, st);
                if (re) return re;
                continue;
            }
        }
        symbol** cf = symbol_table_insert(st, s);
        if (cf) return report_redeclaration_error_raw(tc, s, f, *cf, f);
    }
    // we use the resolved state to detect if the symbol was used,
    // so we don't set it here
    // ast_node_flags_set_resolved(&ig->symbol.node.flags);
    ig->children.symtab = st;
    return RE_OK;
}
resolve_error operator_func_applicable(
    resolver* r, symbol_table* op_st, ast_elem* lhs, ast_elem* rhs, sc_func* f,
    bool* applicable, ast_elem** ctype)
{
    // ensure func has exactly 2 parameters
    // [varargs not allowed for operators]
    if (f->param_count != 2) {
        *applicable = false;
        return RE_OK;
    }
    if (!ast_node_flags_get_resolved(f->scope.symbol.node.flags)) {
        resolve_error re = resolve_ast_node(r, (ast_node*)f, op_st, NULL);
        if (re) return re;
    }
    if (!ctypes_unifiable(lhs, f->params[0].ctype)) {
        *applicable = false;
        return RE_OK;
    }
    if (!ctypes_unifiable(rhs, f->params[1].ctype)) {
        *applicable = false;
        return RE_OK;
    }
    if (ctype) *ctype = f->return_ctype;
    *applicable = true;
    return RE_OK;
}
resolve_error func_applicable(
    resolver* r, symbol_table* fn_st, ast_elem** call_arg_types, ureg arg_count,
    sc_func* func, bool* applicable, ast_elem** ctype)
{
    // works cause varags are not in the lang yet
    if (func->param_count != arg_count) return false;
    if (!ast_node_flags_get_resolved(func->scope.symbol.node.flags)) {
        resolve_error re = resolve_ast_node(r, (ast_node*)func, fn_st, NULL);
        if (re) return re;
    }
    for (ureg i = 0; i < arg_count; i++) {
        if (!ctypes_unifiable(func->params[i].ctype, call_arg_types[i])) {
            *applicable = false;
            return RE_OK;
        }
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
    resolve_error re = RE_OK;
    while (lt) {
        symbol_table* fn_st;
        symbol** s = symbol_table_lookup_with_decl(lt, func_name, &fn_st);
        if (!s) {
            re = report_unknown_symbol(r, c->lhs, st);
            break;
        }
        bool applicable;
        if ((**s).node.kind == SYM_FUNC_OVERLOADED) {
            sym_func_overloaded* sfo = (sym_func_overloaded*)s;
            sc_func* f = sfo->funcs;
            while (f) {
                re = func_applicable(
                    r, fn_st, call_arg_types, c->arg_count, f, &applicable,
                    ctype);
                if (re || applicable) break;
                f = (sc_func*)f->scope.symbol.next;
            }
        }
        else if ((**s).node.kind == SC_FUNC) {
            re = func_applicable(
                r, fn_st, call_arg_types, c->arg_count, (sc_func*)(*s),
                &applicable, ctype);
            if (re || applicable) break;
        }
        else {
            assert(false);
        }
        lt = lt->parent;
    }
    dbuffer_pop(&r->call_types, c->arg_count * sizeof(ast_elem*));
    return re;
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
        case SYM_NAMED_USING:
            return NULL; // TODO
        default: return (ast_elem*)s;
    }
}
void set_break_target_ctype(ast_node* n, ast_elem* ctype)
{
    switch (n->kind) {
        case EXPR_BLOCK: ((expr_block*)n)->ctype = ctype; break;
        case EXPR_IF: ((expr_if*)n)->ctype = ctype; break;
        case EXPR_LOOP: ((expr_loop*)n)->ctype = ctype; break;
        default: assert(false);
    }
    ast_node_flags_set_resolved(&n->flags);
}
resolve_error
get_resolved_symbol_symtab(resolver* r, symbol* s, symbol_table** tgt_st)
{
    if (ast_elem_is_scope((ast_elem*)s)) {
        *tgt_st = ((scope*)s)->body.symtab;
    }
    else if (s->node.kind == SYM_IMPORT_GROUP) {
        *tgt_st = ((sym_import_group*)s)->children.symtab;
    }
    else if (s->node.kind == SYM_IMPORT_MODULE) {
        *tgt_st = ((sym_import_module*)s)->target.mdg_node->symtab;
    }
    else if (s->node.kind == SYM_IMPORT_SYMBOL) {
        return get_resolved_symbol_symtab(
            r, ((sym_import_symbol*)s)->target.symbol, tgt_st);
    }
    else {
        assert(false); // TODO: error
    }
    return RE_OK;
}
// the symbol table is not the one that contains the symbol, but the one
// where it was declared and where the type name loopup should start
resolve_error resolve_ast_node_raw(
    resolver* r, ast_node* n, symbol_table* st, ast_elem** ctype)
{
    if (!n) {
        if (ctype) *ctype = (ast_elem*)&PRIMITIVES[PT_VOID];
        return RE_OK;
    }
    if (ast_elem_is_open_scope((ast_elem*)n)) {
        if (ctype) *ctype = (ast_elem*)n;
        return RE_OK;
    }
    // PERF: find a way to avoid checking in sub exprs
    bool resolved = ast_node_flags_get_resolved(n->flags);
    if (resolved) {
        if (ctype == NULL) return RE_OK;
    }
    else {
        if (ast_node_flags_get_resolving(n->flags)) {
            bool labelable;
            ast_elem_get_label((ast_elem*)n, &labelable);
            // AST_NODE_KIND_ERROR means we explicitly intend to
            // trigger the type loop error here
            if (labelable && n->kind != AST_NODE_KIND_ERROR)
                return RE_REQUIRES_BODY_TYPE;
            assert(false);
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
        case EXPR_CALL: {
            expr_call* c = (expr_call*)n;
            if (resolved) return ret_ctype(c->target->return_ctype, ctype);
            return resolve_call(r, c, st, ctype);
        }
        case EXPR_CONTINUE:
        case EXPR_OP_UNARY: {
            // TODO
            return RE_OK;
        }
        case EXPR_PARENTHESES: {
            return resolve_ast_node(
                r, ((expr_parentheses*)n)->child, st, ctype);
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
        case EXPR_SCOPE_ACCESS: {
            expr_scope_access* esa = (expr_scope_access*)n;
            if (resolved) {
                return ret_ctype(
                    get_resolved_symbol_ctype(esa->target.symbol), ctype);
            }
            re = resolve_ast_node(r, esa->lhs, st, NULL);
            if (re) return re;
            if (esa->lhs->kind != EXPR_IDENTIFIER) {
                assert(false); // TODO: error
            }
            symbol* lhs_sym = ((expr_identifier*)esa->lhs)->value.symbol;
            symbol_table* lhs_st;
            re = get_resolved_symbol_symtab(r, lhs_sym, &lhs_st);
            if (re) return re;
            symbol** s = symbol_table_lookup(lhs_st, esa->target.name);
            if (!s) {
                return report_unknown_symbol_raw(
                    r, ast_node_get_file((ast_node*)esa, st),
                    esa->target_srange);
            }
            esa->target.symbol = *s;
            ast_node_flags_set_resolved(&n->flags);
            if (ctype) *ctype = get_resolved_symbol_ctype(*s);
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
            // TODO: ctype should actually be some kind of func ptr
            if (ctype) *ctype = (ast_elem*)n;
            if (resolved) return RE_OK;
            re = resolve_func(r, (sc_func*)n, st);
            if (re) return re;
            ast_node_flags_set_resolved(&n->flags);
            return RE_OK;
        }
        case SYM_IMPORT_GROUP: {
            if (ctype) *ctype = (ast_elem*)n;
            if (!resolved) ast_node_flags_set_resolved(&n->flags);
            return RE_OK;
        }
        case SYM_IMPORT_MODULE: {
            // TODO: switch from ast_node to symtab here
            // TODO: fix ctype
            // if (ctype) *ctype = ((sym_import_module*)n)->target.mdg_node;
            if (!resolved) ast_node_flags_set_resolved(&n->flags);
            return RE_OK;
        }
        case SYM_IMPORT_SYMBOL: {
            sym_import_symbol* is = (sym_import_symbol*)n;
            if (resolved) {
                return ret_ctype(
                    get_resolved_symbol_ctype(is->target.symbol), ctype);
            }
            symbol_table* sym_st;
            symbol** s =
                symbol_table_lookup_with_decl(st, is->target.name, &sym_st);
            if (!s) return report_unknown_symbol(r, n, st);
            is->target.symbol = *s;
            re = resolve_ast_node(r, (ast_node*)*s, sym_st, ctype);
            if (re) return re;
            ast_node_flags_set_resolved(&n->flags);
            return RE_OK;
        }
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

        case EXPR_RETURN:
        case EXPR_BREAK: {
            if (ctype) *ctype = (ast_elem*)&PRIMITIVES[PT_VOID];
            if (resolved) return RE_OK;
            expr_break* b = (expr_break*)n;
            re = resolve_ast_node(r, b->value, st, &b->value_ctype);
            if (re) {
                if (re != RE_REQUIRES_BODY_TYPE) return re;
                b->value_ctype = NULL;
            }
            else {
                ast_node_flags_set_resolved(&n->flags);
            }

            ast_elem* tgt_type;
            if (n->kind == EXPR_BREAK) {
                re = resolve_ast_node(r, b->target, st, &tgt_type);
                if (re) {
                    if (re != RE_REQUIRES_BODY_TYPE) return re;
                    if (b->value_ctype == NULL) return RE_REQUIRES_BODY_TYPE;
                    set_break_target_ctype(b->target, b->value_ctype);
                    return RE_OK;
                }
            }
            else if (b->target->kind == SC_FUNC) {
                // must already be resolved since parenting function
                tgt_type = ((sc_func*)b->target)->return_ctype;
            }
            else {
                tgt_type = ((sc_func_generic*)b->target)->return_ctype;
            }
            if (!ctypes_unifiable(b->value_ctype, tgt_type)) {
                ureg vstart, vend;
                ast_node_get_bounds(b->value, &vstart, &vend);
                error_log_report_annotated_twice(
                    &r->tc->error_log, ES_RESOLVER, false, "type missmatch",
                    ast_node_get_file((ast_node*)b, st), vstart, vend,
                    "the type returned from here doesn't match the target "
                    "scope's",
                    // TODO: st is kinda wrong here
                    ast_node_get_file((ast_node*)b->target, st),
                    src_range_get_start(b->target->srange),
                    src_range_get_end(b->target->srange), "target scope here");
                return RE_TYPE_MISSMATCH;
            }
            return RE_OK;
        }
        case EXPR_BLOCK: {
            expr_block* b = (expr_block*)n;
            if (!resolved) {
                re = resolve_expr_body(r, (ast_node*)b, &b->body, st);
                if (re) return re;
            }
            if (ctype) *ctype = b->ctype;
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
resolve_error
resolve_ast_node(resolver* r, ast_node* n, symbol_table* st, ast_elem** ctype)
{
    resolve_error re = resolve_ast_node_raw(r, n, st, ctype);
    if (re == RE_OK) return RE_OK;
    if (re == RE_TYPE_LOOP) {
        stack_push(&r->error_stack, n);
    }
    else if (re == RE_REQUIRES_BODY_TYPE) {
        ast_node_flags_clear_resolving(&n->flags);
    }
    return re;
}
static inline void report_type_loop(resolver* r)
{
    // TODO
    assert(false);
}
resolve_error
resolve_expr_body(resolver* r, ast_node* expr, body* b, symbol_table* parent_st)
{
    resolve_error re;
    bool second_pass = false;
    for (ast_node** n = b->elements; *n != NULL; n++) {
        re = resolve_ast_node(r, *n, b->symtab, NULL);
        if (re == RE_REQUIRES_BODY_TYPE) {
            second_pass = true;
            continue;
        }
        if (re) return re;
    }
    if (!ast_node_flags_get_resolved(expr->flags)) {
        // TODO: error msg
        if (!second_pass) assert(false);
        // will cause resolve to trigger type loop error on first loop
        expr->kind = AST_NODE_KIND_ERROR;
    }
    if (second_pass) {
        for (ast_node** n = b->elements; *n != NULL; n++) {
            re = resolve_ast_node(r, *n, b->symtab, NULL);
            if (re) return re;
        }
    }
    return RE_OK;
}
resolve_error resolve_func(resolver* r, sc_func* fn, symbol_table* parent_st)
{
    body* b = &fn->scope.body;
    if (b->symtab == NULL) {
        b->symtab = parent_st;
    }
    else {
        b->symtab->parent = parent_st;
    }
    symbol_table* st = b->symtab;

    resolve_error re;
    for (ureg i = 0; i < fn->param_count; i++) {
        // TODO: default args etc.
        re = resolve_ast_node(
            r, fn->params[i].type, parent_st, &fn->params[i].ctype);
        if (re) return re;
        re = add_symbol(r, st, NULL, (symbol*)&fn->params[i]);
        if (re) return re;
    }
    re = resolve_ast_node(r, fn->return_type, parent_st, &fn->return_ctype);
    if (re) return re;
    ast_node_flags_set_resolved(&fn->scope.symbol.node.flags);
    for (ast_node** n = b->elements; *n != NULL; n++) {
        re = add_ast_node_decls(r, st, NULL, *n);
        if (re) return re;
        re = resolve_ast_node(r, *n, b->symtab, NULL);
        if (re) return re;
    }
    return RE_OK;
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
            atomic_ureg_load(&(**i).using_count), true, (ast_node*)*i);
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
