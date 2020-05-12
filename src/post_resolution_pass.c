#include "post_resolution_pass.h"
#include "mdg.h"
#include "error_log.h"
#include "thread_context.h"
#include "print_ast.h"
prp_error prp_ast_body_insert_dtors(
    post_resolution_pass* prp, prp_block_node* parent, ast_node* node,
    ast_body* body);

int prp_partial_fin(post_resolution_pass* prp, int i, int r)
{
    switch (i) {
        case -1: stack_fin(&prp->nested_funcs); // fallthrough
        case 1: sbuffer_fin(&prp->mem); // fallthrough
        case 0: break;
    }
    return r;
}
int prp_init(post_resolution_pass* prp, thread_context* tc)
{
    int r = sbuffer_init(&prp->mem, plattform_get_page_size());
    if (r) return prp_partial_fin(prp, 0, r);
    r = stack_init(&prp->nested_funcs, &tc->permmem);
    if (r) return prp_partial_fin(prp, 1, r);
    prp->tc = tc;
    return OK;
}
void prp_fin(post_resolution_pass* prp)
{
    prp_partial_fin(prp, -1, 0);
}
prp_error
prp_var_data_push(post_resolution_pass* prp, sym_var* v, prp_var_state state)
{
    prp_var_data* vd =
        (prp_var_data*)sbuffer_append(&prp->mem, sizeof(prp_var_data));
    if (!vd) return PRPE_FATAL;
    if (prp->curr_block->is_else) {
        vd->exit_states = v->prpvn->curr_data->curr_state;
    }
    else {
        vd->exit_states = VAR_STATE_UNKNOWN;
    }
    vd->applied_entry_states = v->prpvn->curr_data->curr_state;
    vd->curr_state = state;
    vd->parent = v->prpvn->curr_data;
    vd->block = prp->curr_block;
    vd->prev = prp->curr_block->used_vars;
    prp->curr_block->used_vars = vd;
    vd->var_node = v->prpvn;
    v->prpvn->curr_data = vd;
    // if we modified a var that was previously untouched inside the
    // block we broke out of, we didn't add the necessary exit_state
    // therefore we need to rerun
    if (prp->curr_block->outermost_break_target &&
        vd->parent->block->depth <
            prp->curr_block->outermost_break_target->depth) {
        prp->curr_block->outermost_break_target->force_rerun = true;
    }
    return PRPE_OK;
}
prp_error prp_var_node_create(post_resolution_pass* prp, sym_var* v)
{
    prp_var_node* vn =
        (prp_var_node*)sbuffer_append(&prp->mem, sizeof(prp_var_node));
    if (!vn) return PRPE_FATAL;
    assert(v->prpvn == NULL);
    v->prpvn = vn;
    prp_var_state initial_state = VAR_STATE_VALID;
    if (v->osym.sym.node.kind == SYM_VAR_INITIALIZED &&
        ((sym_var_initialized*)v)->initial_value !=
            (ast_node*)&PRIMITIVES[PT_UNDEFINED]) {
        initial_state = VAR_STATE_INVALID;
    }
    vn->var = v;
    vn->next_in_break_chain = NULL;
    vn->top_data_on_break_path = NULL;
    vn->curr_data = &vn->owner_var_data;
    vn->owner_var_data.var_node = vn;
    vn->owner_var_data.applied_entry_states = VAR_STATE_UNKNOWN;
    vn->owner_var_data.exit_states = VAR_STATE_UNKNOWN;
    vn->owner_var_data.curr_state = initial_state;
    vn->owner_var_data.parent = NULL;
    vn->owner_var_data.block = prp->curr_block;
    vn->owner_var_data.prev = prp->curr_block->owned_vars;
    prp->curr_block->owned_vars = &vn->owner_var_data;
    return PRPE_OK;
}
bool prp_block_node_activate_on_demand(
    post_resolution_pass* prp, prp_block_node* b)
{
    bool activate = b->force_rerun || b->outermost_break_target;
    for (prp_var_data* vd = b->used_vars; vd; vd = vd->prev) {
        vd->var_node->curr_data = vd;
        if (vd->parent->curr_state & ~vd->applied_entry_states) {
            activate = true;
            vd->applied_entry_states = vd->curr_state;
        }
        vd->curr_state = vd->parent->curr_state;
    }
    if (activate) {
        for (prp_var_data* vd = b->owned_vars; vd; vd = vd->prev) {
            vd->var_node->curr_data = vd;
            vd->curr_state = VAR_STATE_UNKNOWN;
        }
        prp->curr_block = b;
        if (b->node->kind != EXPR_PASTE_EVALUATION) {
            prp->curr_non_paste_block = b;
        }
        if (b->node->kind == EXPR_IF) {
            b->next_expr = (ast_node**)NULL_PTR_PTR;
        }
        else {
            b->next_expr = b->body->elements;
        }
        b->outermost_break_target = NULL;
        b->barrier_node = NULL;
        b->is_rerun = true;
        b->check_rerun = false;
        b->force_rerun = false;
        return true;
    }
    return false;
}
prp_error prp_block_node_push(
    post_resolution_pass* prp, ast_node* node, ast_body* body,
    prp_block_node** tgt)
{
    prp_block_node* bn =
        (prp_block_node*)sbuffer_append(&prp->mem, sizeof(prp_block_node));
    if (!bn) return PRPE_FATAL;
    bn->node = node;
    bn->body = body;
    bn->parent = prp->curr_block;
    bn->depth = prp->curr_block ? prp->curr_block->depth + 1 : 0;
    bn->owned_vars = NULL;
    bn->used_vars = NULL;
    bn->force_rerun = false;
    bn->barrier_node = NULL;
    bn->outermost_break_target = NULL;
    bn->check_rerun = false;
    bn->is_else = false;
    bn->is_rerun = false;
    prp->curr_block = bn;
    if (node->kind != EXPR_PASTE_EVALUATION) {
        prp->curr_non_paste_block = bn;
    }
    if (node->kind == EXPR_IF) {
        bn->next_expr = (ast_node**)NULL_PTR_PTR;
    }
    else {
        bn->next_expr = body->elements;
    }
    if (tgt) *tgt = bn;
    return PRPE_OK;
}
prp_error
prp_var_set_state(post_resolution_pass* prp, sym_var* v, prp_var_state state)
{
    prp_var_data* vd = v->prpvn->curr_data;
    if (vd->curr_state == state) return PRPE_OK;
    if (vd->block != prp->curr_block) {
        return prp_var_data_push(prp, v, state);
    }
    else {
        vd->curr_state = state;
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
    prp_var_state new_state;
    prp_var_data* vd = v->prpvn->curr_data;
    bool defined;
    if (assignment_is_meta_assignment(assign, &defined)) {
        new_state = defined ? VAR_STATE_INVALID : VAR_STATE_VALID;
    }
    else {
        // TODO: make this valid for POD's because we are nice citizens
        // TODO: error
        // HACK
        assert(
            vd->var_node->var->ctype->kind == SYM_PRIMITIVE ||
            vd->curr_state == VAR_STATE_INVALID);
        new_state = VAR_STATE_INVALID;
    }
    err = prp_var_set_state(prp, v, new_state);
    if (err) return err;
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
            vd->exit_states |= vd->var_node->curr_data->curr_state;
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
        if (!bn->outermost_break_target ||
            bn->outermost_break_target->depth > break_bn->depth) {
            bn->outermost_break_target = break_bn;
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
                vn->curr_data->curr_state;
        }
        else {
            // no var data in the block, so we need to insert one
            prp_var_data* vd =
                (prp_var_data*)sbuffer_append(&prp->mem, sizeof(prp_var_data));
            if (!vd) return PRPE_FATAL;
            vd->exit_states = vn->curr_data->curr_state;
            // otherwise it would own it -> have a var data
            assert(vn->top_data_on_break_path->parent);
            vd->curr_state = vn->top_data_on_break_path->parent->curr_state;
            vd->parent = vn->top_data_on_break_path->parent;
            vn->top_data_on_break_path->parent = vd;
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
    prp->curr_block->barrier_node = (ast_node*)b;
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
                prp_block_node_activate_on_demand(prp, b->ebb.prpbn);
                return PRPE_OK;
            }
            return prp_block_node_push(
                prp, n, &((expr_block*)n)->body, &b->ebb.prpbn);
        }
        case EXPR_LOOP: {
            expr_loop* l = (expr_loop*)n;
            if (l->ebb.prpbn) {
                prp_block_node_activate_on_demand(prp, l->ebb.prpbn);
                return PRPE_OK;
            }
            return prp_block_node_push(
                prp, n, &((expr_loop*)n)->body, &l->ebb.prpbn);
        }
        case EXPR_IF: {
            expr_if* i = (expr_if*)n;
            if (i->prpbn) {
                if (!prp_block_node_activate_on_demand(prp, i->prpbn)) {
                    return PRPE_OK;
                }
            }
            else {
                err = prp_block_node_push(
                    prp, n, prp->curr_block->body, &i->prpbn);
                if (err) return err;
            }
            return prp_handle_node(prp, ((expr_if*)n)->if_body);
        }
        case STMT_PASTE_EVALUATION: {
            stmt_paste_evaluation* spe = (stmt_paste_evaluation*)n;
            if (spe->pe.prpbn) {
                prp_block_node_activate_on_demand(prp, spe->pe.prpbn);
                return PRPE_OK;
            }
            return prp_block_node_push(
                prp, (ast_node*)spe, &spe->body, &spe->pe.prpbn);
        }
        case EXPR_PASTE_EVALUATION: {
            expr_paste_evaluation* epe = (expr_paste_evaluation*)n;
            return prp_handle_node(prp, epe->expr);
        }
        default: break;
    }
    return PRPE_OK;
}
// should be called AFER prp->curr_block has been set to block->parent
prp_error prp_leave_var_data(
    post_resolution_pass* prp, prp_var_data* vd, prp_block_node* leaving_block)
{
    assert(prp->curr_block == leaving_block->parent);
    vd->var_node->curr_data = vd->parent;
    if (leaving_block->node->kind != EXPR_LOOP) {
        return prp_var_set_state(prp, vd->var_node->var, vd->exit_states);
    }
    return PRPE_OK;
}

void prp_block_leave_owned(post_resolution_pass* prp, prp_block_node* bn)
{
    if (bn->node->kind == EXPR_LOOP) return;
    for (prp_var_data* vd = bn->owned_vars; vd; vd = vd->prev) {
        vd->exit_states |= vd->curr_state;
    }
}
prp_error prp_handle_if_exit(post_resolution_pass* prp, expr_if* ei)
{
    // TODO: fin everyting even in case of error
    prp_block_node* bn = prp->curr_block;
    assert(!bn->owned_vars);
    prp_error err;
    if (!bn->is_else) {
        if (bn->barrier_node) {
            bn->if_end_unreachable = true;
            bn->barrier_node = NULL;
        }
        if (ei->else_body) {
            for (prp_var_data* vd = bn->used_vars; vd; vd = vd->prev) {
                vd->exit_states |= vd->curr_state;
                vd->curr_state = vd->parent->curr_state;
            }
            bn->is_else = true;
            return prp_handle_node(prp, ei->else_body);
        }
        // no else block
        prp->curr_block = bn->parent;
        for (prp_var_data* vd = bn->used_vars; vd; vd = vd->prev) {
            vd->exit_states |= vd->curr_state | vd->parent->curr_state;
            err = prp_leave_var_data(prp, vd, bn);
            if (err) return err;
        }
        return PRPE_OK;
    }
    // we just did the else block
    bn->is_else = false;
    prp->curr_block = bn->parent;
    for (prp_var_data* vd = bn->used_vars; vd; vd = vd->prev) {
        vd->exit_states |= vd->curr_state;
        err = prp_leave_var_data(prp, vd, bn);
        if (err) return err;
    }
    if (bn->if_end_unreachable && bn->barrier_node) {
        bn->parent->barrier_node = (ast_node*)ei;
    }
    return PRPE_OK;
}
prp_error prp_handle_loop_exit(post_resolution_pass* prp, expr_loop* l)
{
    // TOOD
    assert(false);
    return PRPE_OK;
}
void prp_warn_dead_code(post_resolution_pass* prp)
{
    if (prp->curr_block->is_rerun) return;
    src_range_large barrier_srl;
    src_range_large following_srl;
    symbol_table* st = prp->curr_block->body->symtab;
    ast_node_get_src_range(prp->curr_block->barrier_node, st, &barrier_srl);
    ast_node_get_full_src_range(
        *prp->curr_block->next_expr, st, &following_srl);
    error_log_report_annotated_twice(
        prp->tc->err_log, ES_LIFETIMES, true, "dead code", following_srl.smap,
        following_srl.start, following_srl.end,
        "this code will never be executed", barrier_srl.smap, barrier_srl.start,
        barrier_srl.end, "control flow doesn't get past this statement");
    prp->curr_block->next_expr = (ast_node**)NULL_PTR_PTR;
}
prp_error prp_step(post_resolution_pass* prp)
{
    prp_error err;
    prp_block_node* bn = prp->curr_block;
    ast_node* curr = *bn->next_expr;
    if (curr && prp->curr_block->barrier_node) {
        prp_warn_dead_code(prp);
        curr = NULL;
    }
    if (!curr) {
        if (bn->node->kind == EXPR_IF) {
            return prp_handle_if_exit(prp, (expr_if*)bn->node);
        }
        bool loop_with_reachable_end =
            (bn->node->kind == EXPR_LOOP &&
             bn->next_expr != (ast_node**)NULL_PTR_PTR);
        prp->curr_block = bn->parent;
        prp_var_data* v = bn->used_vars;
        if (!bn->force_rerun) {
            bool check_redo = bn->check_rerun;
            while (true) {
                if (!v) break;
                v->exit_states |= v->curr_state;
                if (check_redo) {
                    prp_var_state entry_states = v->parent->curr_state;
                    if (loop_with_reachable_end) {
                        entry_states |= v->exit_states;
                    }
                    if (~v->applied_entry_states & entry_states) break;
                }
                err = prp_leave_var_data(prp, v, bn);
                if (err) return err;
                v = v->prev;
            }
        }
        if (v) { // early break -> redo
            // undo the leaving and update entry states
            for (prp_var_data* v2 = bn->used_vars; v2 != v; v2 = v2->prev) {
                v2->var_node->curr_data = v2;
                v2->curr_state = v2->parent->curr_state;
                if (loop_with_reachable_end) v2->curr_state |= v2->exit_states;
                v2->applied_entry_states = v2->curr_state;
            }
            for (prp_var_data* v2 = v; v2; v2 = v2->prev) {
                v2->curr_state = v2->parent->curr_state;
                v2->applied_entry_states = v2->curr_state;
            }
            bn->check_rerun = false;
            bn->force_rerun = false;
            bn->barrier_node = NULL;
            bn->is_rerun = true;
            bn->outermost_break_target = NULL;
            prp->curr_block = bn;
            bn->next_expr = bn->body->elements;
            return PRPE_OK;
        }
        prp_block_leave_owned(prp, bn);
        return PRPE_OK;
    }
    prp->curr_block->next_expr++;
    return prp_handle_node(prp, curr);
}
void prp_free_owned_vars(post_resolution_pass* prp)
{
    for (prp_var_data* vd = prp->curr_block->owned_vars; vd; vd = vd->prev) {
        dtor_kind dk;
        // TODO: compiler switch for debug output
        print_ast_node(
            (ast_node*)vd->var_node->var,
            (mdg_node*)symbol_table_get_module_table(
                vd->var_node->var->osym.sym.declaring_st)
                ->owning_node,
            0);
        printf(": ");
        switch (vd->exit_states) {
            case VAR_STATE_INVALID:
                dk = DTOR_KIND_STATIC;
                puts("static dtor");
                break;
            case VAR_STATE_MAYBE_VALID:
                dk = DTOR_KIND_DYNAMIC;
                puts("dynamic dtor");
                break;
            case VAR_STATE_VALID:
                dk = DTOR_KIND_KNOWN_DEAD;
                puts("no dtor");
                break;
            case VAR_STATE_UNKNOWN:
                assert(false); // well we failed then :/
                break;
        }
        ast_flags_set_dtor_kind(&vd->var_node->var->osym.sym.node.flags, dk);
        vd->var_node->var->prpvn = NULL;
    }
}
void prp_assign_func_dtors(post_resolution_pass* prp)
{
    assert(prp->curr_block->node == (ast_node*)prp->curr_fn);
    prp_free_owned_vars(prp);
    prp->curr_block->next_expr = prp->curr_block->body->elements;
    ast_node* curr = *prp->curr_block->next_expr;
    do {
        while (curr) {
            switch (curr->kind) {
                case EXPR_IF: {
                    expr_if* i = (expr_if*)curr;
                    assert(i->prpbn);
                    prp->curr_block = i->prpbn;
                    prp->curr_block->next_expr = (ast_node**)NULL_PTR_PTR;
                    i->prpbn->is_else = false;
                    curr = i->if_body;
                    continue;
                }
                case EXPR_LOOP:
                case EXPR_BLOCK: {
                    expr_block_base* ebb = (expr_block_base*)curr;
                    assert(ebb->prpbn);
                    prp->curr_block = ebb->prpbn;
                    prp->curr_block->next_expr =
                        prp->curr_block->body->elements;
                    ebb->prpbn = NULL;
                    prp_free_owned_vars(prp);
                } break;
                default: break;
            }
            prp->curr_block->next_expr++;
            curr = *prp->curr_block->next_expr;
        }
        if (prp->curr_block->node->kind == EXPR_IF) {
            expr_if* i = (expr_if*)prp->curr_block->node;
            if (!i->prpbn->is_else) {
                i->prpbn->is_else = true;
                if (i->else_body) {
                    curr = i->else_body;
                    i->prpbn->next_expr = (ast_node**)NULL_PTR_PTR;
                    continue;
                }
            }
            i->prpbn = NULL;
        }
        prp->curr_block = prp->curr_block->parent;
    } while (prp->curr_block);
}
prp_error prp_run_func(post_resolution_pass* prp, sc_func_base* fn)
{
    prp->curr_fn = fn;
    prp_error e;
    e = prp_block_node_push(prp, (ast_node*)fn, &fn->sc.body, NULL);
    prp_block_node* fnbn = prp->curr_block;
    if (e) return e;
    while (prp->curr_block) {
        e = prp_step(prp);
        if (e) return e;
    }
    prp->curr_block = fnbn;
    prp_assign_func_dtors(prp);
    sbuffer_clear(&prp->mem);
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
    stack_clear(&prp->nested_funcs);
    sbuffer_clear(&prp->mem);
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
