#include "generic_instance_resolution.h"
#include "resolver.h"
#include "thread_context.h"
#include "assert.h"

resolve_error instantiate_ast_elem(
    resolver* r, ast_elem* src, ast_elem** tgt, symbol_table* gst,
    ast_elem** gvals);

#define COPY_INST(r, TVAR, TGT, NODE_TYPE, n)                                  \
    do {                                                                       \
        (TVAR) = pool_alloc(&r->tc->permmem, sizeof(NODE_TYPE));               \
        if ((TVAR) == NULL) return RE_FATAL;                                   \
        *(NODE_TYPE*)(TVAR) = *(NODE_TYPE*)n;                                  \
        if (TGT) *(TGT) = (ast_elem*)(TVAR);                                   \
    } while (false)

resolve_error instantiate_body(
    resolver* r, ast_node* generic_owner, ast_body* src, ast_node* inst_owner,
    ast_body* tgt, symbol_table* gst, ast_elem** gvals)
{
    symbol_table* stc = src->symtab;
    if (stc->owning_node == (ast_elem*)generic_owner) {
        ureg using_count =
            src->symtab->usings ? src->symtab->usings->usings_count : 0;
        int r = symbol_table_init(
            &tgt->symtab, src->symtab->decl_count, using_count, false,
            (ast_elem*)inst_owner, src->symtab->ppl);
        if (r) return RE_FATAL;
    }
    // TODO: switch to count instead of zero termination to get rid of this
    // mess
    void** body_elems = list_builder_start(&r->tc->listb);
    for (ast_node** n = src->elements; *n; n++) {
        ast_elem* inst;
        instantiate_ast_elem(r, (ast_elem*)*n, &inst, tgt->symtab, gvals);
        if (list_builder_add(&r->tc->listb, inst)) {
            list_builder_drop_list(&r->tc->listb, body_elems);
            return RE_FATAL;
        }
    }
    tgt->elements = (ast_node**)list_builder_pop_list_zt(
        &r->tc->listb, body_elems, &r->tc->permmem);
    tgt->srange = src->srange;
    return RE_OK;
}

resolve_error instantiate_ast_elem(
    resolver* r, ast_elem* n, ast_elem** tgt, symbol_table* ist,
    ast_elem** gvals)
{
    resolve_error re;
    switch (n->kind) {
        case SYM_VAR: {
            sym_var* v;
            COPY_INST(r, v, tgt, sym_var, n);
            symbol** res = symbol_table_insert(ist, (symbol*)v);
            assert(res == NULL);
            v->sym.declaring_st = ist;
            v->var_id = claim_symbol_id(r, (symbol*)v, false);
            return instantiate_ast_elem(r, v->ctype, &v->ctype, ist, gvals);
        }
        case SYM_VAR_INITIALIZED: {
            sym_var_initialized* v;
            COPY_INST(r, v, tgt, sym_var_initialized, n);
            symbol** res = symbol_table_insert(ist, (symbol*)v);
            assert(res == NULL);
            v->var.sym.declaring_st = ist;
            v->var.var_id = claim_symbol_id(r, (symbol*)v, false);
            re = instantiate_ast_elem(
                r, v->var.ctype, &v->var.ctype, ist, gvals);
            if (re) return re;
            return instantiate_ast_elem(
                r, (ast_elem*)v->initial_value, (ast_elem**)&v->initial_value,
                ist, gvals);
        }
        case SYM_GENERIC_PARAM: {
            sym_param* sp = (sym_param*)n;
            ast_elem* st_owner = sp->sym.declaring_st->owning_node;
            if (st_owner->kind == SC_STRUCT_GENERIC) {
                // TODO: maybe some kind of pseudo node?
                sc_struct_generic* sg = (sc_struct_generic*)st_owner;
                *tgt = *(gvals + (sp - sg->generic_params));
            }
            else {
                assert(false); // TODO
            }
            return RE_OK;
        }
        default:
            assert(false);
            *tgt = n; // for the common case we can share it... i think :)
            break;
    }
    return RE_OK;
}
resolve_error instantiate_generic_struct(
    resolver* r, expr_access* ea, ast_elem** args, sc_struct_generic* sg,
    symbol_table* st, ureg ppl, ast_elem** value, ast_elem** ctype)
{
    resolve_error re;
    sc_struct_generic_inst* sgi =
        pool_alloc(&r->tc->permmem, sizeof(sc_struct_generic_inst));
    if (!sgi) return RE_FATAL;
    sgi->st.sb = sg->sb;
    sgi->st.sb.sc.sym.node.kind = SC_STRUCT_GENERIC_INST;
    sgi->generic_vals =
        pool_alloc(&r->tc->permmem, sizeof(ast_node*) * ea->arg_count);
    if (!sgi->generic_vals) return RE_FATAL;
    sgi->generic_val_count = ea->arg_count;
    memcpy(sgi->generic_vals, args, sizeof(ast_elem*) * ea->arg_count);
    sbuffer_remove_back(&r->call_types, sizeof(ast_elem*) * ea->arg_count);
    re = instantiate_body(
        r, (ast_node*)sg, &sg->sb.sc.body, (ast_node*)sgi, &sgi->st.sb.sc.body,
        st, sgi->generic_vals);
    sgi->st.id = claim_symbol_id(
        r, (symbol*)sgi, symbol_table_is_public(sg->sb.sc.sym.declaring_st));
    if (re) return re;
    sgi->st.sb.sc.sym.next = (symbol*)sg->instances;
    sg->instances = sgi;
    if (value) *value = (ast_elem*)sgi;
    if (ctype) *ctype = TYPE_ELEM;
    return RE_OK;
}

// PERF: create a hashmap for this
resolve_error resolve_generic_struct(
    resolver* r, expr_access* ea, sc_struct_generic* sg, symbol_table* st,
    ureg ppl, ast_elem** value, ast_elem** ctype)
{
    if (ea->arg_count != sg->generic_param_count) {
        assert(false); // TODO: error / varargs
    }
    ast_elem** args =
        sbuffer_append(&r->call_types, sizeof(ast_elem*) * ea->arg_count);
    if (!args) return RE_FATAL;
    resolve_error re;
    for (ureg i = 0; i < ea->arg_count; i++) {
        re = resolve_ast_node(r, ea->args[i], st, ppl, &args[i], NULL);
        if (re) return re;
    }
    for (sc_struct_generic_inst* sgi = sg->instances; sgi;
         sgi = (sc_struct_generic_inst*)sgi->st.sb.sc.sym.next) {
        bool success = true;
        for (ureg i = 0; i < ea->arg_count; i++) {
            if (!ctypes_unifiable(args[i], sgi->generic_vals[i])) {
                success = false;
                break;
            }
        }
        if (success) {
            if (value) *value = (ast_elem*)sgi;
            if (ctype) *ctype = TYPE_ELEM;
            return RE_OK;
        }
    }
    return instantiate_generic_struct(r, ea, args, sg, st, ppl, value, ctype);
}