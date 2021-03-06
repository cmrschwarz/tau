#include "generic_instance_resolution.h"
#include "resolver.h"
#include "thread_context.h"
#include "assert.h"
#include "tauc.h"

resolve_error instantiate_ast_node(
    resolver* r, ast_node* n, ast_node** tgt, ast_body* body,
    ast_node* instance);

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

resolve_error
setup_type_ids(resolver* r, type_derivatives* td, ureg* backend_id)
{
    int res = type_map_init(&td->tm);
    if (res) return RE_FATAL;
    *backend_id = ptr_map_claim_backend_id(&r->pm);
    td->ptr_id = ptr_map_claim_id(&r->pm);
    td->slice_id = ptr_map_claim_id(&r->pm);
    return RE_OK;
}
// inst indicates whether we are in a generic instance, or if we are currently
// copying a nested generic. in that case we don't want to claim var ids
resolve_error instantiate_body(
    resolver* r, ast_body* src, ast_body* tgt, ast_body* inst_parent_body,
    ast_node* inst_owning_node, ast_node* instance)
{
    tgt->parent = inst_parent_body;
    tgt->owning_node = inst_owning_node;
    tgt->srange = src->srange;
    if (src->symtab) {
        ureg using_count = symbol_table_has_usings(src->symtab)
                               ? symbol_table_get_using_capacity(src->symtab)
                               : 0;
        ureg sym_count = symbol_table_get_symbol_capacity(src->symtab);
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
        re = instantiate_ast_node(r, *n, &copy, tgt, instance);
        if (re) break;
        if (list_builder_add(&r->tc->listb, copy)) {
            list_builder_drop_list(&r->tc->listb, body_elems);
            return RE_FATAL;
        }
    }
    tgt->elements = (ast_node**)list_builder_pop_list_zt(
        &r->tc->listb, body_elems, &r->tc->permmem);
    return re;
}
// instance is the generic instance of the struct or function, inst_of is the
// generic that is being instantiated
resolve_error instantiate_ast_node(
    resolver* r, ast_node* n, ast_node** tgt, ast_body* body,
    ast_node* instance)
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
            return instantiate_ast_node(r, vc->type, &vc->type, body, instance);
        }
        case SYM_VAR_INITIALIZED: {
            sym_var_initialized* vc;
            COPY_INST(r, n, sym_var_initialized, vc, tgt);
            re = instantiate_ast_node(
                r, vc->var.type, &vc->var.type, body, instance);
            if (re) return re;
            return instantiate_ast_node(
                r, vc->initial_value, (ast_node**)&vc->initial_value, body,
                instance);
        }
        case SC_FUNC: {
            // TODO: think about NOT doing this were possible :)
            sc_func* f = (sc_func*)n;
            sc_func* fc;
            COPY_INST(r, n, sc_func, fc, tgt);
            ureg param_size = fc->fnb.param_count * sizeof(sym_param);
            fc->fnb.params = alloc_perm(r, param_size);
            fc->id = ast_node_claim_id(r, n, ast_body_is_public(body));
            if (!fc->fnb.params) return RE_FATAL;
            memcpy(fc->fnb.params, f->fnb.params, param_size);
            for (ureg i = 0; i < fc->fnb.param_count; i++) {
                sym_param* p = &fc->fnb.params[i];
                ast_node_set_status((ast_node*)p, NODE_STATUS_PARSED);
                p->sym.declaring_body = &fc->fnb.sc.body;
                re = instantiate_ast_node(
                    r, f->fnb.params[i].type, &p->type, body, instance);
                if (re) return re;
            }
            re = instantiate_ast_node(
                r, f->fnb.return_type, &fc->fnb.return_type, body, instance);
            return instantiate_body(
                r, &f->fnb.sc.body, &fc->fnb.sc.body, body, (ast_node*)fc,
                instance);
        }
        case EXPR_RETURN: {
            expr_return* er;
            COPY_INST(r, n, expr_return, er, tgt);
            return instantiate_ast_node(
                r, er->value, &er->value, body, instance);
        }
        case EXPR_OP_UNARY: {
            expr_op_unary* ou;
            COPY_INST(r, n, expr_op_unary, ou, tgt);
            return instantiate_ast_node(
                r, ou->child, &ou->child, body, instance);
        }
        case EXPR_OP_BINARY: {
            expr_op_binary* ob;
            COPY_INST(r, n, expr_op_binary, ob, tgt);
            re = instantiate_ast_node(r, ob->lhs, &ob->lhs, body, instance);
            if (re) return re;
            return instantiate_ast_node(r, ob->rhs, &ob->rhs, body, instance);
        }
        case EXPR_CAST: {
            expr_cast* c;
            COPY_INST(r, n, expr_cast, c, tgt);
            re = instantiate_ast_node(r, c->value, &c->value, body, instance);
            if (re) return re;
            return instantiate_ast_node(
                r, c->target_type, &c->target_type, body, instance);
        }
        case EXPR_ACCESS: {
            expr_access* a = (expr_access*)n;
            expr_access* ac;
            COPY_INST(r, n, expr_access, ac, tgt);
            ac->args = alloc_perm(r, a->arg_count * sizeof(ast_elem*));
            if (!ac->args) return RE_FATAL;
            re = instantiate_ast_node(r, ac->lhs, &ac->lhs, body, instance);
            if (re) return re;
            for (ureg i = 0; i < ac->arg_count; i++) {
                re = instantiate_ast_node(
                    r, a->args[i], &ac->args[i], body, instance);
                if (re) return re;
            }
            return RE_OK;
        }
        case EXPR_PP: {
            expr_pp* eppc;
            COPY_INST(r, n, expr_pp, eppc, tgt);
            return instantiate_ast_node(
                r, eppc->pp_expr, &eppc->pp_expr, body, instance);
        }
        case EXPR_PASTE_STR: {
            expr_paste_str* epsc;
            COPY_INST(r, n, expr_paste_str, epsc, tgt);
            return instantiate_ast_node(
                r, epsc->value, &epsc->value, body, instance);
        }
        case SC_STRUCT: {
            sc_struct* st = (sc_struct*)n;
            sc_struct* stc;
            COPY_INST(r, n, sc_struct, stc, tgt);
            re = setup_type_ids(r, &stc->type_derivs, &stc->backend_id);
            if (re) return re;
            return instantiate_body(
                r, &st->sb.sc.body, &stc->sb.sc.body, body, (ast_node*)stc,
                instance);
        } break;
        default: assert(false); return RE_FATAL;
    }
    return RE_OK;
}

// sufficiently setup struct instance so we can leave the gim lock
resolve_error create_generic_struct_inst(
    resolver* r, expr_access* ea, ast_elem** args, ast_elem** ctypes,
    sc_struct_generic* sg, sc_struct_generic_inst** tgt)
{
    sc_struct_generic_inst* sgi =
        pool_alloc(&r->tc->permmem, sizeof(sc_struct_generic_inst));
    if (!sgi) return RE_FATAL;
    sgi->st.sb = sg->sb;
    sgi->base = sg;
    sgi->st.sb.sc.osym.sym.node.kind = SC_STRUCT_GENERIC_INST;
    ast_node_set_status((ast_node*)sgi, NODE_STATUS_PARSED);
    ((ast_node*)sgi)->emitted_for_pp = false;
    sgi->generic_args = pool_alloc(
        &r->tc->permmem, sizeof(sym_param_generic_inst) * ea->arg_count);
    if (!sgi->generic_args) return RE_FATAL;
    sgi->generic_arg_count = ea->arg_count;
    for (ureg i = 0; i < sg->generic_param_count; i++) {
        sym_param_generic_inst* gpi = &sgi->generic_args[i];
        gpi->value = args[i];
        gpi->ctype = ctypes[i];
    }
    resolve_error re =
        setup_type_ids(r, &sgi->st.type_derivs, &sgi->st.backend_id);
    if (re) return re;
    *tgt = sgi;
    // TODO: alloc these somewhere else, maybe a freelist ore smth.
    sgi->res_ctx = tmalloc(sizeof(generic_resolution_ctx));
    assert(sgi->res_ctx);
    sgi->res_ctx->responsible_tc = r->tc;
    sgi->res_ctx->instantiated = false;
    int res = list_init(&sgi->res_ctx->generic_inst_waiters);
    assert(!res);
    UNUSED(res);
    return RE_OK;
}
resolve_error
instantiate_generic_struct(resolver* r, sc_struct_generic_inst* sgi)
{
    resolve_error re;
    sc_struct_generic* sg = sgi->base;
    // if this is from a foreign module (reached through an import)
    // we need a final id immediately since others could need it
    // PERF: check if it's declared in the current mdg res group
    // to save us the atomic?
    bool public_st = is_body_public_st(&sg->sb.sc.body);
    bool extern_st = ((ast_node*)sg)->emitted_for_pp;
    assert(!extern_st || public_st);
    ureg public_sym_count = r->public_sym_count;
    sgi->st.backend_id = claim_symbol_id(r, (symbol*)sgi, public_st);
    re = instantiate_body(
        r, &sg->sb.sc.body, &sgi->st.sb.sc.body, sg->sb.sc.body.parent,
        (ast_node*)sgi, (ast_node*)sgi);
    if (re) return re;
    for (ureg i = 0; i < sg->generic_param_count; i++) {
        sym_param* gp = &sg->generic_params[i];
        sym_param_generic_inst* gpi = &sgi->generic_args[i];
        gpi->sym.declaring_body = &sgi->st.sb.sc.body;
        gpi->sym.name = gp->sym.name;
        gpi->sym.node.flags = gp->sym.node.flags;
        ast_node_set_resolved(&gpi->sym.node);
        gpi->sym.node.kind = SYM_PARAM_GENERIC_INST;
        gpi->sym.node.srange = gp->sym.node.srange;
        symbol* c =
            symbol_table_insert(sgi->st.sb.sc.body.symtab, (symbol*)gpi);
        if (c) {
            report_redeclaration_error(r, c, (symbol*)&sgi->generic_args[i]);
            if (curr_context_raise_error(
                    r, &sgi->st.sb.sc.body, (ast_node*)sgi)) {
                return RE_FATAL;
            }
        }
    }
    if (!re) {
        re = add_body_decls(r, &sgi->st.sb.sc.body, NULL, public_st);
    }
    ast_node_set_status((ast_node*)sgi, NODE_STATUS_DECLARED);
    if (extern_st) {
        ureg id_count = r->public_sym_count - public_sym_count;
        ureg ids = atomic_ureg_add(&r->tc->t->node_ids, id_count);
        llvm_backend_reserve_symbols(
            r->backend, r->id_space - PRIV_SYMBOL_OFFSET, ids + id_count);
        ureg id_space = ids;
        adjust_node_ids(r, &id_space, (ast_node*)sgi);
        assert(id_space == ids + id_count);
        r->public_sym_count = public_sym_count;
    }
    return RE_OK;
}
static inline generic_inst_waiter*
create_waiter(resolver* r, sc_struct_generic_inst* sgi)
{
    generic_inst_waiter* giw = tmalloc(sizeof(generic_inst_waiter));
    if (!giw) return NULL;
    if (list_init(&giw->requiring_pprns)) {
        tfree(giw);
        return NULL;
    }
    atomic_boolean_init(&giw->cmw.done, false);
    // any of those does it reallly.
    giw->cmw.requiring_module = *r->mdgs_begin;
    atomic_ureg_inc(&giw->cmw.requiring_module->ungenerated_pp_deps);
    if (false) { // TODO: append this to a waiter list
        list_fin(&giw->requiring_pprns, true);
        tfree(giw);
        return NULL;
    }
    return giw;
}
static inline resolve_error resolve_generic_struct_instance_raw(
    resolver* r, sc_struct_generic_inst* sgi, ast_body* body, bool instantiate,
    generic_inst_waiter* waiter)
{
    resolve_error re;
    if (instantiate) {
        re = instantiate_generic_struct(r, sgi);
        if (re) return re;
    }
    // we need a body that will not cause pprns to get added
    // to thread dependant nodes
    ast_body* dummy_body = &(**r->mdgs_begin).body;
    re = resolve_ast_node(r, (ast_node*)sgi, dummy_body, NULL, NULL);
    assert(!sgi->st.sb.sc.body.pprn);
    gim_lock(&sgi->base->inst_map);
    if (instantiate) sgi->res_ctx->instantiated = true;
    list_it it;
    list_it_begin(&it, &sgi->res_ctx->generic_inst_waiters);
    generic_inst_waiter* giw;
    while ((giw = list_it_next(&it, &sgi->res_ctx->generic_inst_waiters))) {
        atomic_boolean_store(&giw->cmw.done, true);
        ureg prev =
            atomic_ureg_dec(&giw->cmw.requiring_module->ungenerated_pp_deps);
        UNUSED(prev);
        assert(prev > 0);
    }
    if (!re) {
        list_fin(&sgi->res_ctx->generic_inst_waiters, true);
        tfree(sgi->res_ctx);
        sgi->res_ctx = NULL;
        if (waiter) tfree(waiter);
    }
    else {
        // we have to wait on somebody else. we place a waiter
        assert(sgi->res_ctx->responsible_tc == r->tc);
        sgi->res_ctx->responsible_tc = NULL;
        list_clear(&sgi->res_ctx->generic_inst_waiters);
        if (re != RE_FATAL) {
            if (!waiter) {
                waiter = create_waiter(r, sgi);
                if (!waiter) re = RE_FATAL;
            }
            if (waiter) {
                if (list_append(
                        &sgi->res_ctx->generic_inst_waiters, NULL, waiter)) {
                    re = RE_FATAL;
                }
            }
        }
    }
    gim_unlock(&sgi->base->inst_map);
    if (re && re != RE_FATAL) {
        assert(waiter);
    }
    return re;
}
static inline resolve_error handle_existing_struct_instance(
    resolver* r, sc_struct_generic_inst* sgi, ast_body* body,
    generic_inst_waiter* waiter)
{
    sc_struct_generic* sg = sgi->base;
    generic_resolution_ctx* c = sgi->res_ctx;
    if (!c) {
        gim_unlock(&sg->inst_map);
        return RE_OK;
    }
    if (c->responsible_tc == r->tc) {
        // TODO: do we need to prevent loops?
        gim_unlock(&sg->inst_map);
        return RE_OK;
    }
    if (!c->responsible_tc) {
        c->responsible_tc = r->tc;
        bool inst = c->instantiated;
        gim_unlock(&sg->inst_map);
        return resolve_generic_struct_instance_raw(r, sgi, body, !inst, waiter);
    }
    // TODO: cycle prevention strat: lock all generics from this
    // thread in order and only place the waiter
    // if we got all of them and state still indicates wait
    if (!waiter) {
        waiter = create_waiter(r, sgi);
        if (!waiter) {
            gim_unlock(&sg->inst_map);
            return RE_FATAL;
        }
    }
    else {
        atomic_boolean_store_flat(&waiter->cmw.done, false);
    }
    gim_unlock(&sg->inst_map);
    pp_resolve_node* pprn;
    if (get_curr_pprn(r, body, &pprn)) return RE_FATAL;
    assert(pprn); // we don't just resolve generic instances for fun.
    if (list_append(&waiter->requiring_pprns, NULL, pprn)) return RE_FATAL;
    pprn->resolve_dep_count++;
    return RE_UNREALIZED_COMPTIME;
}

resolve_error resolve_generic_struct_access(
    resolver* r, expr_access* ea, ast_body* body, sc_struct_generic* sg,
    ast_elem** value, ast_elem** ctype)
{
    resolve_error re;
    if (ea->arg_count != sg->generic_param_count) {
        assert(false); // once we have generic overloading we shouldn' get here
    }
    ast_elem** args =
        sbuffer_append(&r->temp_buffer, sizeof(ast_elem*) * ea->arg_count);
    if (!args) return RE_FATAL;
    ast_elem** ctypes =
        sbuffer_append(&r->temp_buffer, sizeof(ast_elem*) * ea->arg_count);
    if (!ctypes) return RE_FATAL;
    bool err = false;
    for (ureg i = 0; i < ea->arg_count; i++) {
        re = resolve_ast_node(r, ea->args[i], body, &args[i], &ctypes[i]);
        if (re) return re;
        if (args[i] == ERROR_ELEM) err = true;
    }
    if (err) {
        if (curr_context_raise_error(r, body, (ast_node*)ea)) {
            return RE_FATAL;
        }
        if (value) *value = ERROR_ELEM;
        if (ctype) *ctype = ERROR_ELEM;
        return RE_OK;
    }
    gim_lock(&sg->inst_map);
    sc_struct_generic_inst** st =
        gim_get_struct(&sg->inst_map, args, ea->arg_count);
    if (!st) {
        gim_unlock(&sg->inst_map);
        return RE_FATAL;
    }
    sc_struct_generic_inst* sgi;
    if (!*st) {
        re = create_generic_struct_inst(r, ea, args, ctypes, sg, &sgi);
        if (!re) *st = sgi;
        gim_unlock(&sg->inst_map);
        if (!re) {
            re = resolve_generic_struct_instance_raw(r, sgi, body, true, NULL);
        }
    }
    else {
        sgi = *st;
        // we might duplicate waiters for this thread this way
        // Too Bad! (will be fixed by the cache thingy)
        re = handle_existing_struct_instance(r, sgi, body, NULL);
    }
    if (value) *value = (ast_elem*)sgi;
    if (ctype) *ctype = TYPE_ELEM;
    sbuffer_remove_back(&r->temp_buffer, sizeof(ast_elem*) * ea->arg_count);
    sbuffer_remove_back(&r->temp_buffer, sizeof(ast_elem*) * ea->arg_count);
    return re;
}

resolve_error resolve_generic_struct_instance(
    resolver* r, sc_struct_generic_inst* sgi, ast_body* body,
    generic_inst_waiter* waiter)
{
    // TODO: some kind of per thread cache to avoid
    // locks on already resolved structs my be in order
    gim_lock(&sgi->base->inst_map);
    return handle_existing_struct_instance(r, sgi, body, waiter);
}
