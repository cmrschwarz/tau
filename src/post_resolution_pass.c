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
    vd->curr_state_in_block = VAR_STATE_UNKNOWN;
    vd->curr_state_inducing_expr = NULL;
    vd->exit_state = VAR_STATE_UNKNOWN;
    // var_node->curr_block can't be NULL so no false positives
    if (v->prpvn->curr_block == prp->curr_block->twin_branch) {
        v->prpvn->curr_data->branch_twin = vd;
        vd->branch_twin = v->prpvn->curr_data;
        vd->parent = v->prpvn->curr_data->parent;
    }
    else {
        vd->branch_twin = NULL;
        vd->parent = v->prpvn->curr_data;
    }
    vd->state_before_block = v->prpvn->curr_data->curr_state_in_block;
    v->prpvn->curr_data = vd;
    v->prpvn->curr_block = prp->curr_block;
    vd->prev = prp->curr_block->used_vars;
    prp->curr_block->used_vars = vd;
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
    vn->curr_data = &vn->owner_var_data;
    vn->curr_block = prp->curr_block;
    vn->owner_var_data.branch_twin = NULL;
    vn->owner_var_data.breaking_expr = NULL;
    vn->owner_var_data.curr_state_in_block = initial_state;
    vn->owner_var_data.curr_state_inducing_expr = (ast_node*)v;
    vn->owner_var_data.exit_state = VAR_STATE_UNKNOWN;
    vn->owner_var_data.parent = NULL;
    vn->owner_var_data.state_before_block = VAR_STATE_UNDEFINED;
    vn->owner_var_data.prev = prp->curr_block->owned_vars;
    prp->curr_block->owned_vars = vn;
    return PRPE_OK;
}

prp_error
prp_block_node_push(post_resolution_pass* prp, ast_node* node, ast_body* body)
{
    prp_block_node* bn = (prp_block_node*)freelist_alloc(&prp->block_node_mem);
    if (!bn) return PRPE_FATAL;
    bn->node = node;
    bn->parent = prp->curr_block;
    bn->inside_loop =
        (bn->parent->inside_loop || bn->parent->node->kind == EXPR_LOOP);
    bn->owned_vars = NULL;
    bn->used_vars = NULL;
    bn->second_pass = false;
    bn->twin_branch = NULL;
    prp->curr_block = bn;
    if (body) {
        bn->next = body->elements;
    }
    else {
        assert(node->kind == EXPR_IF);
        bn->next = NULL_PTR_PTR;
    }
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
    if (v->prpvn->curr_block != prp->curr_block) {
        err = prp_var_data_push(prp, v);
        if (err) return err;
    }
    prp_var_data* vd = v->prpvn->curr_data;
    if (assign->rhs->kind == EXPR_IDENTIFIER) {
        expr_identifier* rid = (expr_identifier*)assign->rhs;
        if (rid->value.sym == (symbol*)&PRIMITIVES[PT_UNDEFINED]) {
            vd->curr_state_in_block = VAR_STATE_UNDEFINED;
            return PRPE_OK;
        }
        else if (rid->value.sym == (symbol*)&PRIMITIVES[PT_DEFINED]) {
            vd->curr_state_in_block = VAR_STATE_DEFINED;
            return PRPE_OK;
        }
    }
    // TODO: make this valid for POD's because we are nice citizens
    // TODO: error
    assert(vd->curr_state_in_block == VAR_STATE_DEFINED);
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
        case OP_ASSIGN: {
            return prp_handle_assign(prp, (expr_op_binary*)n);
        }
        case EXPR_CALL: {
            // TODO
        } break;
        case EXPR_BLOCK: {
            return prp_block_node_push(prp, n, &((expr_block*)n)->body);
        }
        case EXPR_LOOP: {
            return prp_block_node_push(prp, n, &((expr_loop*)n)->body);
        }
        case EXPR_IF: {
            err = prp_block_node_push(prp, n, NULL);
            if (err) return err;
            return prp_handle_node(prp, ((expr_if*)n)->if_body);
        }
        default: break;
    }
}

static inline bool prp_redo_loop(prp_block_node* bn)
{
    if (bn->second_pass) return false;
    for (prp_var_data* v = bn->used_vars; v; v = v->prev) {
        if (v->state_before_block != v->curr_state_in_block) {
            return true;
        }
    }
    return false;
}

prp_error prp_step(post_resolution_pass* prp)
{
    prp_block_node* bn = prp->curr_block;
    ast_node* curr = *bn->next;
    if (!curr) {
        if (bn->node->kind == EXPR_LOOP) {
            if (prp_redo_loop(bn)) {
                bn->next = ((expr_loop*)bn->node)->body.elements;
                bn->second_pass = true;
                return prp_step(prp);
            }
        }
        if (bn->node->kind == EXPR_IF) {
            expr_if* ei = (expr_if*)bn->node;
            if (!bn->is_else && ei->else_body != NULL) {
            }
            else if (bn->is_else) {
            }
        }
    }
    prp->curr_block->next++;
    return prp_handle_node(prp, curr);
}

prp_error prp_run_func(post_resolution_pass* prp, sc_func_base* fn)
{
    prp->curr_fn = fn;
    prp_error e;
    e = prp_block_node_push(prp, (ast_node*)fn, &fn->sc.body);
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
    prp_error err;
    prp->module_mode = true;
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