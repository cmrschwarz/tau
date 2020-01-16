#include "generic_instance_resolution.h"
#include "resolver.h"
#include "thread_context.h"
#include "assert.h"

resolve_error instantiate_ast_elem(
    resolver* r, ast_elem* n, ast_elem** tgt, symbol_table* st, bool inst);

static inline void* alloc_perm(resolver* r, ureg size)
{
    void* res = pool_alloc(&r->tc->permmem, size);
    if (!res) error_log_report_allocation_failiure(r->tc->err_log);
    return res;
}

#define COPY_TO_TGT(r, NODE, NODE_TYPE, TGT_PTR_PTR)                           \
    do {                                                                       \
        *(TGT_PTR_PTR) = alloc_perm((r), sizeof(NODE_TYPE));                   \
        if (*(TGT_PTR_PTR) == NULL) return RE_FATAL;                           \
    } while (false)

#define COPY_INST(r, NODE, NODE_TYPE, COPY_PTR, TGT_PTR_PTR)                   \
    do {                                                                       \
        (COPY_PTR) = alloc_perm((r), sizeof(NODE_TYPE));                       \
        if ((COPY_PTR) == NULL) return RE_FATAL;                               \
        *(NODE_TYPE*)(COPY_PTR) = *(NODE_TYPE*)(NODE);                         \
        *(TGT_PTR_PTR) = (ast_elem*)(COPY_PTR);                                \
    } while (false)
// inst indicates whether we are in a generic instance, or if we are currently
// copying a nested generic. in that case we don't want to claim var ids
resolve_error instantiate_body(
    resolver* r, ast_body* src, ast_body* tgt, ast_node* generic_owner,
    ast_node* inst_owner, symbol_table* tgt_parent_st, bool inst)
{
    symbol_table* stc = src->symtab;
    if (stc->owning_node == (ast_elem*)generic_owner) {
        ureg using_count =
            src->symtab->usings ? src->symtab->usings->usings_count : 0;
        int r = symbol_table_init(
            &tgt->symtab, src->symtab->decl_count, using_count, false,
            (ast_elem*)inst_owner, src->symtab->ppl);
        if (r) return RE_FATAL;
        tgt->symtab->parent = tgt_parent_st;
    }
    // TODO: switch to count instead of zero termination to get rid of this
    // mess
    resolve_error re = RE_OK;
    void** body_elems = list_builder_start(&r->tc->listb);
    for (ast_node** n = src->elements; *n; n++) {
        ast_elem* copy;
        re = instantiate_ast_elem(r, (ast_elem*)*n, &copy, tgt->symtab, inst);
        if (re) break;
        if (list_builder_add(&r->tc->listb, copy)) {
            list_builder_drop_list(&r->tc->listb, body_elems);
            return RE_FATAL;
        }
    }
    tgt->elements = (ast_node**)list_builder_pop_list_zt(
        &r->tc->listb, body_elems, &r->tc->permmem);
    tgt->srange = src->srange;
    return re;
}
// instance is the generic instance of the struct or function, inst_of is the
// generic that is being instantiated
resolve_error instantiate_ast_elem(
    resolver* r, ast_elem* n, ast_elem** tgt, symbol_table* st, bool inst)
{
    resolve_error re;
    switch (n->kind) {
        case EXPR_IDENTIFIER: {
            COPY_TO_TGT(r, n, expr_identifier, tgt);
            return RE_OK;
        }
        case SYM_VAR: {
            sym_var* vc;
            COPY_INST(r, n, sym_var, vc, tgt);
            symbol** res = symbol_table_insert(st, (symbol*)vc);
            assert(res == NULL);
            vc->sym.declaring_st = st;
            if (inst) vc->var_id = claim_symbol_id(r, (symbol*)vc, false);
            return instantiate_ast_elem(
                r, &vc->type->elem, &vc->ctype, st, inst);
        }
        case SYM_VAR_INITIALIZED: {
            sym_var_initialized* vc;
            COPY_INST(r, n, sym_var_initialized, vc, tgt);
            symbol** res = symbol_table_insert(st, (symbol*)vc);
            assert(res == NULL);
            vc->var.sym.declaring_st = st;
            vc->var.var_id = claim_symbol_id(r, (symbol*)vc, false);
            re = instantiate_ast_elem(
                r, (ast_elem*)vc->var.type, &vc->var.ctype, st, inst);
            if (re) return re;
            return instantiate_ast_elem(
                r, (ast_elem*)vc->initial_value, (ast_elem**)&vc->initial_value,
                st, inst);
        }
        case SC_FUNC: {
            // TODO: think about NOT doing this were possible :)
            sc_func* f = (sc_func*)n;
            sc_func* fc;
            COPY_INST(r, n, sc_func, fc, tgt);
            ureg param_size = fc->fnb.param_count * sizeof(sym_param);
            fc->fnb.params = alloc_perm(r, param_size);
            if (!fc->fnb.params) return RE_FATAL;
            memcpy(fc->fnb.params, f->fnb.params, param_size);
            for (ureg i = 0; i < fc->fnb.param_count; i++) {
                re = instantiate_ast_elem(
                    r, (ast_elem*)f->fnb.params[i].type,
                    (ast_elem**)&fc->fnb.params[i].type, st, inst);
                if (re) return re;
            }
            return instantiate_body(
                r, &f->fnb.sc.body, &fc->fnb.sc.body, (ast_node*)f,
                (ast_node*)fc, st, inst);
        }
        case EXPR_OP_UNARY: {
            expr_op_unary* ou;
            COPY_INST(r, n, expr_identifier, ou, tgt);
            return instantiate_ast_elem(
                r, (ast_elem*)ou->child, (ast_elem**)&ou->child, st, inst);
        };
        default:
            assert(false);
            *tgt = n; // for the common case we can share it... i think :)
            break;
    }
    return RE_OK;
}
resolve_error instantiate_generic_struct(
    resolver* r, expr_access* ea, ast_elem** args, ast_elem** ctypes,
    sc_struct_generic* sg, symbol_table* st, ureg ppl,
    sc_struct_generic_inst** tgt)
{
    resolve_error re;
    sc_struct_generic_inst* sgi =
        pool_alloc(&r->tc->permmem, sizeof(sc_struct_generic_inst));
    if (!sgi) return RE_FATAL;
    sgi->st.sb = sg->sb;
    sgi->st.sb.sc.sym.node.kind = SC_STRUCT_GENERIC_INST;
    sgi->generic_args = pool_alloc(
        &r->tc->permmem, sizeof(sym_param_generic_inst) * ea->arg_count);
    if (!sgi->generic_args) return RE_FATAL;
    sgi->generic_arg_count = ea->arg_count;
    re = instantiate_body(
        r, &sg->sb.sc.body, &sgi->st.sb.sc.body, (ast_node*)sg, (ast_node*)sgi,
        st, true);
    sgi->st.id = claim_symbol_id(
        r, (symbol*)sgi, symbol_table_is_public(sg->sb.sc.sym.declaring_st));
    for (ureg i = 0; i < sg->generic_param_count; i++) {
        sym_param* gp = &sg->generic_params[i];
        sym_param_generic_inst* gpi = &sgi->generic_args[i];
        gpi->value = args[i];
        gpi->ctype = ctypes[i];
        gpi->sym.declaring_st = sgi->st.sb.sc.body.symtab;
        gpi->sym.name = gp->sym.name;
        gpi->sym.node.flags = gp->sym.node.flags;
        ast_flags_set_resolved(&gpi->sym.node.flags);
        gpi->sym.node.kind = SYM_PARAM_GENERIC_INST;
        gpi->sym.node.srange = gp->sym.node.srange;
        symbol** c =
            symbol_table_insert(sgi->st.sb.sc.body.symtab, (symbol*)gpi);
        if (c) {
            return report_redeclaration_error(
                r, *c, &sgi->generic_args[i], sgi->st.sb.sc.body.symtab);
        }
    }
    if (re) return re;
    sgi->st.sb.sc.sym.next = (symbol*)sg->instances;
    sg->instances = sgi;
    ast_flags_clear_resolving(&sgi->st.sb.sc.sym.node.flags);
    ast_flags_clear_resolved(&sgi->st.sb.sc.sym.node.flags);
    *tgt = sgi;
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
    ast_elem** ctypes =
        sbuffer_append(&r->call_types, sizeof(ast_elem*) * ea->arg_count);
    if (!ctypes) return RE_FATAL;
    resolve_error re;
    for (ureg i = 0; i < ea->arg_count; i++) {
        re = resolve_ast_node(r, ea->args[i], st, ppl, &args[i], &ctypes[i]);
        if (re) return re;
    }
    for (sc_struct_generic_inst* sgi = sg->instances; sgi;
         sgi = (sc_struct_generic_inst*)sgi->st.sb.sc.sym.next) {
        bool success = true;
        for (ureg i = 0; i < ea->arg_count; i++) {
            if (!ctypes_unifiable(args[i], sgi->generic_args[i].value)) {
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
    sc_struct_generic_inst* sgi;
    re = instantiate_generic_struct(r, ea, args, ctypes, sg, st, ppl, &sgi);
    sbuffer_remove_back(&r->call_types, sizeof(ast_elem*) * ea->arg_count);
    sbuffer_remove_back(&r->call_types, sizeof(ast_elem*) * ea->arg_count);
    if (re) return re;
    return resolve_ast_node(
        r, (ast_node*)sgi, sgi->st.sb.sc.sym.declaring_st,
        sgi->st.sb.sc.sym.declaring_st->ppl, value, ctype);
}
