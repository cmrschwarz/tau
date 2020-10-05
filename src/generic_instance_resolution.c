#include "generic_instance_resolution.h"
#include "resolver.h"
#include "thread_context.h"
#include "assert.h"

resolve_error instantiate_ast_node(
    resolver* r, ast_node* n, ast_node** tgt, symbol_table* st);

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
        ast_node_set_status((ast_node*)*(TGT_PTR_PTR), NODE_STATUS_PARSED);    \
        **(NODE_TYPE**)(TGT_PTR_PTR) = *(NODE_TYPE*)(NODE);                    \
    } while (false)

#define COPY_INST(r, NODE, NODE_TYPE, COPY_PTR, TGT_PTR_PTR)                   \
    do {                                                                       \
        (COPY_PTR) = alloc_perm((r), sizeof(NODE_TYPE));                       \
        if ((COPY_PTR) == NULL) return RE_FATAL;                               \
        *(NODE_TYPE*)(COPY_PTR) = *(NODE_TYPE*)(NODE);                         \
        ast_node_set_status((ast_node*)(COPY_PTR), NODE_STATUS_PARSED);        \
        *(TGT_PTR_PTR) = (ast_node*)(COPY_PTR);                                \
    } while (false)
// inst indicates whether we are in a generic instance, or if we are currently
// copying a nested generic. in that case we don't want to claim var ids
resolve_error instantiate_body(
    resolver* r, ast_body* src, ast_body* tgt, ast_node* inst_owner)
{
    if (src->symtab) {
        ureg using_count = symbol_table_get_using_count(src->symtab);
        ureg sym_count = symbol_table_get_symbol_count(src->symtab);
        ureg impl_count = 0;
        ureg generic_impl_count = 0;
        if (src->symtab->tt) {
            impl_count = (1 << src->symtab->tt->impl_lists_bitcount) - 1;
            generic_impl_count =
                dbuffer_is_invalid(&src->symtab->tt->generic_impls)
                    ? 0
                    : dbuffer_get_size(&src->symtab->tt->generic_impls) /
                          sizeof(trait_impl_generic*);
        }
        tgt->symtab = symbol_table_create(
            sym_count, using_count, impl_count, generic_impl_count);
        if (!tgt->symtab) return RE_FATAL;
    }
    else {
        tgt->symtab = NULL;
    }
    // TODO: switch to count instead of zero termination to get rid of this
    // mess
    resolve_error re = RE_OK;
    void** body_elems = list_builder_start(&r->tc->listb);
    for (ast_node** n = src->elements; *n; n++) {
        ast_node* copy;
        re = instantiate_ast_node(r, *n, &copy, tgt->symtab);
        if (re) break;
        if (list_builder_add(&r->tc->listb, copy)) {
            list_builder_drop_list(&r->tc->listb, body_elems);
            return RE_FATAL;
        }
    }
    tgt->elements = (ast_node**)list_builder_pop_list_zt(
        &r->tc->listb, body_elems, &r->tc->permmem);
    tgt->srange = src->srange;
    tgt->owning_node = inst_owner;
    tgt->parent = src->parent;
    return re;
}
// instance is the generic instance of the struct or function, inst_of is the
// generic that is being instantiated
resolve_error
instantiate_ast_node(resolver* r, ast_node* n, ast_node** tgt, symbol_table* st)
{
    if (!n) {
        *tgt = NULL;
        return RE_OK;
    }
    resolve_error re;
    switch (n->kind) {
        case EXPR_LITERAL: {
            *tgt = n;
            return RE_OK;
        }
        case EXPR_IDENTIFIER: {
            COPY_TO_TGT(r, n, expr_identifier, tgt);
            return RE_OK;
        }
        case SYM_VAR: {
            sym_var* vc;
            COPY_INST(r, n, sym_var, vc, tgt);
            return instantiate_ast_node(r, vc->type, &vc->type, st);
        }
        case SYM_VAR_INITIALIZED: {
            sym_var_initialized* vc;
            COPY_INST(r, n, sym_var_initialized, vc, tgt);
            re = instantiate_ast_node(r, vc->var.type, &vc->var.type, st);
            if (re) return re;
            return instantiate_ast_node(
                r, vc->initial_value, (ast_node**)&vc->initial_value, st);
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
                sym_param* p = &fc->fnb.params[i];
                ast_node_set_status((ast_node*)p, NODE_STATUS_PARSED);
                p->sym.declaring_body = &fc->fnb.sc.body;
                re = instantiate_ast_node(
                    r, f->fnb.params[i].type, &p->type, st);
                if (re) return re;
            }
            return instantiate_body(
                r, &f->fnb.sc.body, &fc->fnb.sc.body, (ast_node*)fc);
        }
        case SC_FUNC_GENERIC: {
        }
        case EXPR_OP_UNARY: {
            expr_op_unary* ou;
            COPY_INST(r, n, expr_op_unary, ou, tgt);
            return instantiate_ast_node(r, ou->child, &ou->child, st);
        }
        case EXPR_OP_BINARY: {
            expr_op_binary* ob;
            COPY_INST(r, n, expr_op_binary, ob, tgt);
            re = instantiate_ast_node(r, ob->lhs, &ob->lhs, st);
            if (re) return re;
            return instantiate_ast_node(r, ob->rhs, &ob->rhs, st);
        }
        case EXPR_CAST: {
            expr_cast* c;
            COPY_INST(r, n, expr_cast, c, tgt);
            re = instantiate_ast_node(r, c->value, &c->value, st);
            if (re) return re;
            return instantiate_ast_node(r, c->target_type, &c->target_type, st);
        }
        default:
            assert(false);
            *tgt = n; // for the common case we can share it... i think :)
            break;
    }
    return RE_OK;
}
resolve_error instantiate_generic_struct(
    resolver* r, expr_access* ea, ast_elem** args, ast_elem** ctypes,
    sc_struct_generic* sg, ast_body* parent_body, sc_struct_generic_inst** tgt)
{
    resolve_error re;
    sc_struct_generic_inst* sgi =
        pool_alloc(&r->tc->permmem, sizeof(sc_struct_generic_inst));
    if (!sgi) return RE_FATAL;
    sgi->st.sb = sg->sb;
    sgi->st.sb.sc.osym.sym.node.kind = SC_STRUCT_GENERIC_INST;
    sgi->generic_args = pool_alloc(
        &r->tc->permmem, sizeof(sym_param_generic_inst) * ea->arg_count);
    if (!sgi->generic_args) return RE_FATAL;
    sgi->generic_arg_count = ea->arg_count;
    re = instantiate_body(
        r, &sg->sb.sc.body, &sgi->st.sb.sc.body, (ast_node*)sgi);
    for (ureg i = 0; i < sg->generic_param_count; i++) {
        sym_param* gp = &sg->generic_params[i];
        sym_param_generic_inst* gpi = &sgi->generic_args[i];
        gpi->value = args[i];
        gpi->ctype = ctypes[i];
        // gpi->sym.declaring_st = sgi->st.sb.sc.body.symtab;
        gpi->sym.name = gp->sym.name;
        gpi->sym.node.flags = gp->sym.node.flags;
        ast_node_set_resolved(&gpi->sym.node);
        gpi->sym.node.kind = SYM_PARAM_GENERIC_INST;
        gpi->sym.node.srange = gp->sym.node.srange;
        symbol* c =
            symbol_table_insert(sgi->st.sb.sc.body.symtab, (symbol*)gpi);
        if (c) {
            report_redeclaration_error(r, c, (symbol*)&sgi->generic_args[i]);
        }
    }
    if (re) return re;
    sgi->st.sb.sc.osym.sym.next = (symbol*)sg->instances;
    sg->instances = sgi;
    ast_node_set_status((ast_node*)sgi, NODE_STATUS_PARSED);
    *tgt = sgi;
    return RE_OK;
}

// PERF: create a hashmap for this
resolve_error resolve_generic_struct(
    resolver* r, expr_access* ea, sc_struct_generic* sg, ast_body* parent_body,
    ast_elem** value, ast_elem** ctype)
{
    resolve_error re;
    if (ea->arg_count != sg->generic_param_count) {
        assert(false); // TODO: error / varargs
    }
    ast_elem** args =
        sbuffer_append(&r->temp_buffer, sizeof(ast_elem*) * ea->arg_count);
    if (!args) return RE_FATAL;
    ast_elem** ctypes =
        sbuffer_append(&r->temp_buffer, sizeof(ast_elem*) * ea->arg_count);
    if (!ctypes) return RE_FATAL;

    for (ureg i = 0; i < ea->arg_count; i++) {
        re =
            resolve_ast_node(r, ea->args[i], parent_body, &args[i], &ctypes[i]);
        if (re) return re;
    }
    for (sc_struct_generic_inst* sgi = sg->instances; sgi;
         sgi = (sc_struct_generic_inst*)sgi->st.sb.sc.osym.sym.next) {
        bool success = true;
        for (ureg i = 0; i < ea->arg_count; i++) {
            if (args[i] == sgi->generic_args[i].value) {
                success = false;
                break;
            }
        }
        if (success) {
            if (value) *value = (ast_elem*)sgi;
            if (ctype) *ctype = TYPE_ELEM;
            sbuffer_remove_back(
                &r->temp_buffer, sizeof(ast_elem*) * ea->arg_count);
            sbuffer_remove_back(
                &r->temp_buffer, sizeof(ast_elem*) * ea->arg_count);
            return RE_OK;
        }
    }
    sc_struct_generic_inst* sgi;
    re = instantiate_generic_struct(r, ea, args, ctypes, sg, parent_body, &sgi);
    sbuffer_remove_back(&r->temp_buffer, sizeof(ast_elem*) * ea->arg_count);
    sbuffer_remove_back(&r->temp_buffer, sizeof(ast_elem*) * ea->arg_count);
    if (re) return re;
    re = add_body_decls(r, &sgi->st.sb.sc.body, NULL, false);
    if (re) return re;
    sgi->st.backend_id =
        claim_symbol_id(r, (symbol*)sgi, ast_body_is_public(&sg->sb.sc.body));
    return resolve_ast_node(r, (ast_node*)sgi, parent_body, value, ctype);
}
