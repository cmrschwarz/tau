#include "post_resolution_pass.h"
#include "mdg.h"
prp_error prp_ast_body_insert_dtors(
    post_resolution_pass* prp, prp_block_node* parent, ast_node* node,
    ast_body* body);

int prp_partial_fin(post_resolution_pass* prp, int i, int r)
{
    switch (i) {
        case -1: freelist_fin(&prp->block_node_mem); // fallthrough
        case 4: stack_fin(&prp->nested_funcs); // fallthrough
        case 3: freelist_fin(&prp->var_data_mem); // fallthrough
        case 2: freelist_fin(&prp->var_node_mem); // fallthrough
        case 1: pool_fin(&prp->mem); // fallthrough
        case 0: break;
    }
    return r;
}
int prp_init(post_resolution_pass* prp)
{
    int r = pool_init(&prp->mem);
    if (r) return prp_partial_fin(prp, 0, r);
    r = freelist_init(&prp->var_node_mem, &prp->mem, sizeof(prp_var_node));
    if (r) return prp_partial_fin(prp, 1, r);
    r = freelist_init(&prp->var_data_mem, &prp->mem, sizeof(prp_var_data));
    if (r) return prp_partial_fin(prp, 2, r);
    r = freelist_init(&prp->block_node_mem, &prp->mem, sizeof(prp_block_node));
    if (r) return prp_partial_fin(prp, 3, r);
    r = stack_init(&prp->nested_funcs, &prp->mem);
    if (r) return prp_partial_fin(prp, 4, r);
    return OK;
}
void prp_fin(post_resolution_pass* prp)
{
    prp_partial_fin(prp, -1, 0);
}

prp_error prp_var_data_push(post_resolution_pass* prp, sym_var* v)
{
    prp_var_data* vd = (prp_var_data*)freelist_alloc(&prp->var_data_mem);
    if (!vd) return PRPE_FATAL;
    vd->breaking_expr = NULL;
    if (prp->curr_block->is_else) {
        vd->exit_states = v->prpvn->curr_data->curr_state_in_block;
    }
    else {
        vd->exit_states = VAR_STATE_UNKNOWN;
    }
    vd->curr_state_in_block = VAR_STATE_UNKNOWN;
    vd->curr_state_inducing_expr = NULL;
    vd->parent = v->prpvn->curr_data;

    vd->block = prp->curr_block;
    vd->prev = prp->curr_block->used_vars;
    prp->curr_block->used_vars = vd;
    vd->var_node = v->prpvn;

    v->prpvn->curr_data = vd;
    return PRPE_OK;
}
prp_error prp_var_node_create(post_resolution_pass* prp, sym_var* v)
{
    prp_var_node* vn = (prp_var_node*)freelist_alloc(&prp->var_node_mem);
    if (!vn) return PRPE_FATAL;
    assert(v->prpvn == NULL);
    v->prpvn = vn;
    prp_var_state initial_state = VAR_STATE_UNDEFINED;
    if (v->osym.sym.node.kind == SYM_VAR_INITIALIZED &&
        ((sym_var_initialized*)v)->initial_value !=
            (ast_node*)&PRIMITIVES[PT_UNDEFINED]) {
        initial_state = VAR_STATE_DEFINED;
    }
    vn->var = v;
    vn->next_in_break_chain = NULL;
    vn->top_data_on_break_path = NULL;
    vn->curr_data = &vn->owner_var_data;
    vn->owner_var_data.var_node = vn;
    vn->owner_var_data.exit_states = VAR_STATE_UNKNOWN;
    vn->owner_var_data.breaking_expr = NULL;
    vn->owner_var_data.curr_state_in_block = initial_state;
    vn->owner_var_data.curr_state_inducing_expr = (ast_node*)v;
    vn->owner_var_data.parent = NULL;
    vn->owner_var_data.block = prp->curr_block;
    vn->owner_var_data.prev = prp->curr_block->owned_vars;
    prp->curr_block->owned_vars = &vn->owner_var_data;
    return PRPE_OK;
}
void prp_block_node_activate(post_resolution_pass* prp, prp_block_node* b)
{
    prp->curr_block = b;
    for (prp_var_data* vd = b->owned_vars; vd; vd = vd->prev) {
        vd->var_node->curr_data = vd;
    }
    for (prp_var_data* vd = b->used_vars; vd; vd = vd->prev) {
        vd->var_node->curr_data = vd;
    }
}
prp_error prp_block_node_push(
    post_resolution_pass* prp, ast_node* node, ast_body* body,
    prp_block_node** tgt)
{
    prp_block_node* bn = (prp_block_node*)freelist_alloc(&prp->block_node_mem);
    if (!bn) return PRPE_FATAL;
    bn->node = node;
    bn->parent = prp->curr_block;
    bn->depth = prp->curr_block ? prp->curr_block->depth + 1 : 0;
    bn->final_attempt = (bn->parent == NULL || bn->parent->final_attempt);
    bn->owned_vars = NULL;
    bn->used_vars = NULL;
    bn->second_pass = false;
    prp->curr_block = bn;
    if (body) {
        bn->next = body->elements;
    }
    else {
        assert(node->kind == EXPR_IF);
        bn->next = (ast_node**)NULL_PTR_PTR;
    }
    if (tgt) *tgt = bn;
    return PRPE_OK;
}
prp_error prp_handle_assign(post_resolution_pass* prp, expr_op_binary* assign)
{
    prp_error err;
    if (assign->lhs->kind != EXPR_IDENTIFIER) return PRPE_OK;
    expr_identifier* id = (expr_identifier*)assign->lhs;
    assert(ast_elem_is_var((ast_elem*)id->value.sym));
    sym_var* v = (sym_var*)id->value.sym;
    if (v->prpvn == NULL) return PRPE_OK; // external variable
    prp_var_state prev_state = v->prpvn->curr_data->curr_state_in_block;
    prp_var_state new_state;
    prp_var_data* vd = v->prpvn->curr_data;
    bool defined;
    if (assignment_is_meta_assignment(assign, &defined)) {
        new_state = defined ? VAR_STATE_DEFINED : VAR_STATE_UNDEFINED;
    }
    else {
        // TODO: make this valid for POD's because we are nice citizens
        // TODO: error
        assert(
            vd->var_node->var->ctype->kind == SYM_PRIMITIVE ||
            vd->curr_state_in_block == VAR_STATE_DEFINED);
        new_state = VAR_STATE_DEFINED;
    }
    if (new_state != prev_state) {
        if (v->prpvn->curr_data->block != prp->curr_block) {
            err = prp_var_data_push(prp, v);
            if (err) return err;
        }
        v->prpvn->curr_data->curr_state_in_block = new_state;
        v->prpvn->curr_data->curr_state_inducing_expr = (ast_node*)assign;
    }
    return PRPE_OK;
}
prp_error prp_handle_break(post_resolution_pass* prp, expr_break* b)
{
    prp_block_node* bn = prp->curr_block;
    prp_block_node* break_bn = b->target.ebb->prpbn;
    // the exit state of the vars in this link chain will be updated for the
    // block that we break from
    prp_var_node* break_chain = NULL;
    assert(break_bn); // we don't do expr prp mode for now
    while (true) {
        for (prp_var_data* vd = bn->owned_vars; vd; vd = vd->prev) {
            vd->exit_states |= vd->var_node->curr_data->curr_state_in_block;
        }
        for (prp_var_data* vd = bn->used_vars; vd; vd = vd->prev) {
            // we only want to add this to the break chain
            // if the var has not been visited yet (top_data == NULL)
            // and it is  owned by a block outside the block we
            // break from (depth < break_bn->depth)
            if (!vd->var_node->top_data_on_break_path &&
                vd->var_node->owner_var_data.block->depth < break_bn->depth) {
                vd->var_node->next_in_break_chain = break_chain;
                break_chain = vd->var_node;
            }
            vd->var_node->top_data_on_break_path = vd;
        }
        if (bn == break_bn) break;
        bn = bn->parent;
    }
    prp_var_node* vn = break_chain;
    while (true) {
        if (!vn) break;
        if (vn->top_data_on_break_path->block == break_bn) {
            // the break block already has a var data for this, so we update
            // that
            vn->top_data_on_break_path->exit_states |=
                vn->curr_data->curr_state_in_block;
        }
        else {
            // no var data in the block, so we need to insert one
            prp_var_data* vd =
                (prp_var_data*)freelist_alloc(&prp->var_data_mem);
            if (!vd) return PRPE_FATAL;
            vd->breaking_expr = (ast_node*)b;
            vd->exit_states = vn->curr_data->curr_state_in_block;
            // otherwise it would own it -> have a var data
            assert(vn->top_data_on_break_path->parent);
            vd->curr_state_in_block =
                vn->top_data_on_break_path->parent->curr_state_in_block;
            vd->parent = vn->top_data_on_break_path->parent;
            vn->top_data_on_break_path->parent = vd;
            vd->curr_state_inducing_expr = NULL;
            vd->block = break_bn;
            vd->prev = break_bn->used_vars;
            break_bn->used_vars = vd;
            vd->var_node = vn;
        }
        prp_var_node* vn_next = vn->next_in_break_chain;
        vn->next_in_break_chain = NULL;
        vn->top_data_on_break_path = NULL;
        vn = vn_next;
    }
    return PRPE_OK;
}
prp_error prp_handle_node(post_resolution_pass* prp, ast_node* n)
{
    prp_error err;
    switch (n->kind) {
        case SYM_VAR:
        case SYM_VAR_INITIALIZED: {
            return prp_var_node_create(prp, (sym_var*)n);
        }
        case EXPR_OP_BINARY: {
            expr_op_binary* ob = (expr_op_binary*)n;
            if (ob->node.op_kind == OP_ASSIGN) {
                return prp_handle_assign(prp, ob);
            }
        } break;
        case EXPR_CALL: {
            // TODO
        } break;
        case EXPR_BREAK: {
            return prp_handle_break(prp, (expr_break*)n);
        }
        case EXPR_CONTINUE: {
            assert(false); // TODO
        }
        case EXPR_BLOCK: {
            expr_block* b = (expr_block*)n;
            if (b->ebb.prpbn) {
                prp_block_node_activate(prp, b->ebb.prpbn);
                return PRPE_OK;
            }
            return prp_block_node_push(
                prp, n, &((expr_block*)n)->body, &b->ebb.prpbn);
        }
        case EXPR_LOOP: {
            expr_loop* l = (expr_loop*)n;
            if (l->ebb.prpbn) {
                prp_block_node_activate(prp, l->ebb.prpbn);
                return PRPE_OK;
            }
            return prp_block_node_push(
                prp, n, &((expr_loop*)n)->body, &l->ebb.prpbn);
        }
        case EXPR_IF: {
            expr_if* i = (expr_if*)n;
            if (i->prpbn) {
                prp_block_node_activate(prp, i->prpbn);
                return PRPE_OK;
            }
            else {
                err = prp_block_node_push(prp, n, NULL, &i->prpbn);
                if (err) return err;
            }
            return prp_handle_node(prp, ((expr_if*)n)->if_body);
        }
        default: break;
    }
    return PRPE_OK;
}

static inline bool prp_redo_loop(prp_block_node* bn)
{
    if (bn->second_pass) return false;
    for (prp_var_data* v = bn->used_vars; v; v = v->prev) {
        if (v->parent->curr_state_in_block != v->curr_state_in_block) {
            return true;
        }
    }
    return false;
}
// should be called AFER prp->curr_block has been set to block->parent
prp_error prp_leave_var_data(
    post_resolution_pass* prp, prp_var_data* vd, prp_block_node* block,
    bool force_keep)
{
    if (vd->parent->curr_state_in_block != vd->exit_states) {
        prp_var_data* true_parent;
        if (vd->parent->block != block->parent) {
            vd->var_node->curr_data = vd->var_node->curr_data->parent;
            prp_error err = prp_var_data_push(prp, vd->var_node->var);
            if (err) return err;
            true_parent = vd->var_node->curr_data;
        }
        else {
            true_parent = vd->parent;
        }
        true_parent->curr_state_in_block = vd->exit_states;
    }
    if (block->final_attempt && !force_keep) {
        freelist_free(&prp->var_data_mem, vd);
    }
    return PRPE_OK;
}
prp_error prp_handle_regular_block_after_exit(
    post_resolution_pass* prp, prp_block_node* bn)
{
    for (prp_var_data* vd = bn->used_vars; vd; vd = vd->prev) {
        vd->exit_states |= vd->curr_state_in_block;
        prp_error err = prp_leave_var_data(prp, vd, bn, false);
        if (err) return err; // TODO: fin even in case of error
    }
    if (bn->final_attempt) {
        freelist_free(&prp->block_node_mem, bn);
    }
    return PRPE_OK;
}

void prp_block_release_owned(post_resolution_pass* prp, bool force_keep)
{
    prp_block_node* bn = prp->curr_block;
    for (prp_var_data* vd = bn->owned_vars; vd; vd = vd->prev) {
        if (bn->final_attempt && !force_keep) {
            // TODO: insert destructors for the initialize ones
            // on block enter restore var nodes from var data
            char* dtor_msg;
            if (vd->curr_state_in_block == VAR_STATE_DEFINED) {
                dtor_msg = "destructor";
            }
            else if (vd->curr_state_in_block == VAR_STATE_MAYBE_DEFINED) {
                dtor_msg = "dynamic destructor";
            }
            else if (vd->curr_state_in_block == VAR_STATE_UNDEFINED) {
                dtor_msg = "no destructor";
            }
            else {
                dtor_msg = "error in destructor decision";
            }
            printf("%s for %s\n", dtor_msg, vd->var_node->var->osym.sym.name);
            freelist_free(&prp->var_node_mem, vd);
        }
    }
}
prp_error prp_handle_if_exit(post_resolution_pass* prp, expr_if* ei)
{
    // TODO: fin everyting even in case of error
    prp_block_node* bn = prp->curr_block;
    assert(!bn->owned_vars);
    prp_error err;
    if (!bn->is_else) {
        if (ei->else_body) {
            for (prp_var_data* vd = bn->used_vars; vd; vd = vd->prev) {
                vd->exit_states |= vd->curr_state_in_block;
                vd->curr_state_in_block = vd->parent->curr_state_in_block;
            }
            bn->is_else = true;
            return prp_handle_node(prp, ei->else_body);
        }
        // no else block
        prp->curr_block = bn->parent;
        for (prp_var_data* vd = bn->used_vars; vd; vd = vd->prev) {
            vd->exit_states |=
                vd->curr_state_in_block | vd->parent->curr_state_in_block;
            err = prp_leave_var_data(prp, vd, bn, false);
            if (err) return err;
        }
        if (bn->final_attempt) {
            freelist_free(&prp->block_node_mem, bn);
        }
        return PRPE_OK;
    }
    // we just did the else block
    bn->is_else = false;
    prp->curr_block = bn->parent;
    for (prp_var_data* vd = bn->used_vars; vd; vd = vd->prev) {
        vd->exit_states |= vd->curr_state_in_block;
        err = prp_leave_var_data(prp, vd, bn, false);
        if (err) return err;
    }
    if (bn->final_attempt) {
        freelist_free(&prp->block_node_mem, bn);
    }
    return PRPE_OK;
}
prp_error prp_step(post_resolution_pass* prp)
{
    prp_error err;
    prp_block_node* bn = prp->curr_block;
    ast_node* curr = *bn->next;
    if (!curr) {
        if (bn->node->kind == EXPR_LOOP) {
            if (prp_redo_loop(bn)) {
                bn->next = ((expr_loop*)bn->node)->body.elements;
                bn->second_pass = true;
                return PRPE_OK;
            }
        }
        if (bn->node->kind == EXPR_IF) {
            return prp_handle_if_exit(prp, (expr_if*)bn->node);
        }
        prp_block_release_owned(prp, false);
        prp->curr_block = bn->parent;
        err = prp_handle_regular_block_after_exit(prp, bn);
        if (err) return err;
        return PRPE_OK;
    }
    prp->curr_block->next++;
    return prp_handle_node(prp, curr);
}

prp_error prp_run_func(post_resolution_pass* prp, sc_func_base* fn)
{
    prp->curr_fn = fn;
    prp_error e;
    e = prp_block_node_push(prp, (ast_node*)fn, &fn->sc.body, NULL);
    if (e) return e;
    while (prp->curr_block) {
        e = prp_step(prp);
        if (e) {
            pool_clear(&prp->mem);
            return e;
        }
    }
    return PRPE_OK;
}
prp_error prp_run_nested_funcs(post_resolution_pass* prp)
{
    while (!stack_is_empty(&prp->nested_funcs)) {
        sc_func_base* f = (sc_func_base*)stack_pop(&prp->nested_funcs);
        prp_error err = prp_run_func(prp, f);
        if (err) return err;
    }
    return PRPE_OK;
}
prp_error prp_run_symtab(post_resolution_pass* prp, symbol_table* st)
{
    prp_error err;
    symtab_it stit = symtab_it_make(st);
    for (symbol* s = symtab_it_next(&stit); s; s = symtab_it_next(&stit)) {
        if (s->node.kind == SC_FUNC) {
            err = prp_run_func(prp, (sc_func_base*)s);
            if (err) return err;
        }
        err = prp_run_nested_funcs(prp);
        if (err) return err;
    }
    return PRPE_OK;
}
prp_error
prp_run_modules(post_resolution_pass* prp, mdg_node** start, mdg_node** end)
{
    stack_fin(&prp->nested_funcs); // TODO: maybe don't realloc this everytime
    freelist_clear(&prp->var_node_mem);
    freelist_clear(&prp->var_data_mem);
    freelist_clear(&prp->block_node_mem);
    pool_clear(&prp->mem);
    stack_init(&prp->nested_funcs, &prp->mem);
    prp_error err;
    prp->module_mode = true;
    prp->curr_block = NULL;
    for (mdg_node** n = start; n != end; n++) {
        aseglist_iterator it;
        aseglist_iterator_begin(&it, &(**n).module_frames);
        for (module_frame* mf = aseglist_iterator_next(&it); mf;
             mf = aseglist_iterator_next(&it)) {
            if (mf->body.symtab->owning_node != (ast_elem*)mf) continue;
            err = prp_run_symtab(prp, mf->body.symtab);
            if (err) return err;
        }
        err = prp_run_symtab(prp, (**n).symtab);
        if (err) return err;
    }
    return PRPE_OK;
}