#include "name_mangle.h"
#include "resolver.h"
#include "utils/int_string_conversions.h"
#include "utils/alphabase32.h"
#include "tauc.h"
static const char* NAME_BEGIN = "_T";
typedef enum name_mangle_node_kind_e {
    NMNK_IDENT,
    NMNK_IDENT_ID,
    NMNK_IDENT_STR,
    NMNK_IDENT_LEN_STR,
    NMNK_LEN_STR,
    NMNK_STR,
} name_mangle_node_kind;
typedef struct name_mangle_node_s {
    name_mangle_node_kind kind;
    char ident[3];
    union content_u {
        const char* str;
        ureg id;
    } content;
    ureg str_len;
} name_mangle_node;

static inline int push_name_mangle_node_raw(
    name_mangle_node_kind k, const char* ident, const char* str, ureg id,
    sbuffer* buff, ureg* size)
{
    name_mangle_node* n = sbuffer_append(buff, sizeof(name_mangle_node));
    if (!n) return ERR;
    n->kind = k;
    if (k == NMNK_IDENT || k == NMNK_IDENT_ID || k == NMNK_IDENT_STR ||
        k == NMNK_IDENT_LEN_STR) {
        ureg ident_len = strlen(ident);
        assert(ident_len < sizeof(n->ident));
        memcpy(n->ident, ident, ident_len);
        n->ident[ident_len] = '\0';
        *size += ident_len;
    }
    if (k == NMNK_IDENT_STR || k == NMNK_IDENT_LEN_STR || k == NMNK_LEN_STR ||
        k == NMNK_STR) {
        n->content.str = str;
        n->str_len = strlen(str);
        *size += n->str_len;
    }
    if (k == NMNK_IDENT_LEN_STR || k == NMNK_LEN_STR) {
        *size += ureg_get_decimal_digits_count(n->str_len);
    }
    if (k == NMNK_IDENT_ID) {
        n->content.id = id;
        n->str_len = ureg_get_decimal_digits_count(id);
        *size += n->str_len;
    }
    return OK;
}

int push_name_mangle_id_node(
    const char* ident, ureg id, sbuffer* buff, ureg* size)
{
    return push_name_mangle_node_raw(
        NMNK_IDENT_ID, ident, NULL, id, buff, size);
}
int push_name_mangle_str_node(const char* str, sbuffer* buff, ureg* size)
{
    return push_name_mangle_node_raw(NMNK_STR, NULL, str, 0, buff, size);
}
int push_name_mangle_len_str_node(const char* str, sbuffer* buff, ureg* size)
{
    return push_name_mangle_node_raw(NMNK_LEN_STR, NULL, str, 0, buff, size);
}
int push_name_mangle_ident_node(const char* ident, sbuffer* buff, ureg* size)
{
    return push_name_mangle_node_raw(NMNK_IDENT, ident, NULL, 0, buff, size);
}
int push_scope_name(
    ast_node* node, ast_body* b, bool scoped, sbuffer* buff, ureg* size)
{
    ast_node* n = b->owning_node;
    switch (n->kind) {
        case MF_MODULE:
        case MF_MODULE_GENERIC:
        case MF_EXTEND:
        case MF_EXTEND_GENERIC: {
            module_frame* mf = (module_frame*)n;
            if (ast_node_get_access_mod(node) == AM_LOCAL) {
                if (!scoped) {
                    return push_name_mangle_str_node(
                        mf->name_mangled_unscoped, buff, size);
                }
                return push_name_mangle_str_node(mf->name_mangled, buff, size);
            }
            if (!scoped) return OK;
            return push_name_mangle_str_node(
                module_frame_get_module(mf)->name_mangled, buff, size);
        } break;

        case SC_TRAIT:
        case SC_TRAIT_GENERIC:
        case SC_TRAIT_GENERIC_INST:
        case SC_STRUCT:
        case SC_STRUCT_GENERIC:
        case SC_STRUCT_GENERIC_INST: {
            if (ast_elem_is_var((ast_elem*)node) &&
                ast_node_get_instance_member(node)) {
                return OK;
            }
            sc_struct_base* sb = (sc_struct_base*)n;
            return push_name_mangle_str_node(
                scoped ? sb->name_mangled : sb->name_mangled_unscoped, buff,
                size);
        }

        case SC_FUNC:
        case SC_FUNC_GENERIC:
        case SC_FUNC_GENERIC_INST: {
            if (ast_elem_is_var((ast_elem*)node)) return OK;
            sc_func_base* fb = (sc_func_base*)n;
            return push_name_mangle_str_node(
                scoped ? fb->name_mangled : fb->name_mangled_unscoped, buff,
                size);
        }
        case ELEM_MDG_NODE: {
            mdg_node* m = (mdg_node*)n;
            if (!scoped) {
                return push_name_mangle_len_str_node(m->name, buff, size);
            }
            return push_name_mangle_str_node(m->name_mangled, buff, size);
        }
        case EXPR_BLOCK:
        case EXPR_LOOP: {
            if (ast_elem_is_var((ast_elem*)node)) return OK;
            expr_block_base* ebb = (expr_block_base*)n;
            int res = push_name_mangle_str_node(
                ebb->name_mangled_unscoped, buff, size);
            if (scoped || res) return res;
            return push_scope_name(node, ebb->body.parent, scoped, buff, size);
        }

        case EXPR_PASTE_EVALUATION:
        case STMT_PASTE_EVALUATION: {
            return push_scope_name(
                node, ((paste_evaluation*)n)->body.parent, scoped, buff, size);
        }
        default: assert(false); return ERR;
    }
}

int push_ctype(
    tauc* t, ast_elem* ctype, ast_body* ctx, sbuffer* buff, ureg* size,
    bool put_sep_after, bool* sep_required_before)
{
    *sep_required_before = false;
    int r;
    bool no_sep = false;
    while (true) {
        switch (ctype->kind) {
            case SYM_PRIMITIVE: {
                primitive* p = (primitive*)ctype;
                char c;
                switch (p->sym.node.pt_kind) {
                    case PT_UINT: c = 'u'; break;
                    case PT_INT: c = 'i'; break;
                    case PT_VOID: c = 'v'; break;
                    case PT_FLOAT: c = 'f'; break;
                    default: c = '\0';
                }
                if (c != '\0') {
                    char id[2];
                    id[0] = c;
                    id[1] = '\0';
                    return push_name_mangle_node_raw(
                        NMNK_IDENT, (char*)&id, NULL, 0, buff, size);
                }
                break;
            }
            case TYPE_POINTER: {
                no_sep = true;
                type_pointer* tp = (type_pointer*)ctype;
                r = push_name_mangle_ident_node(
                    tp->tb.is_const ? "P" : "p", buff, size);
                if (r) return r;
                ctype = tp->base_type;
                continue;
            }
            // TODO: arrays, tuples, ...
            default: break;
        }
        break;
    }
    assert(ast_elem_is_symbol(ctype));
    if (put_sep_after && !no_sep) {
        if (push_name_mangle_ident_node("N", buff, size)) return ERR;
    }
    symbol* s = (symbol*)ctype;
    if (push_name_mangle_len_str_node(s->name, buff, size)) return ERR;
    ureg cb_level = 0;
    for (ast_body* cb = s->declaring_body; cb; cb = cb->parent) cb_level++;
    ureg ctx_level = 0;
    for (ast_body* ctxb = ctx; ctxb; ctxb = ctxb->parent) ctx_level++;
    ast_body* cb = s->declaring_body;
    while (cb_level > ctx_level) {
        if (push_scope_name((ast_node*)s, cb, false, buff, size)) return ERR;
        cb = cb->parent;
        cb_level--;
    }
    if (cb == ctx) {
        *sep_required_before = true;
        return OK;
    }
    ast_body* ctxb = ctx;
    ureg escape_count = 0;
    while (ctx_level > cb_level) {
        escape_count++;
        ctxb = ctxb->parent;
        ctx_level--;
    }
    while (cb != ctxb) {
        escape_count++;
        if (push_scope_name((ast_node*)s, cb, false, buff, size)) return ERR;
        cb = cb->parent;
        ctxb = ctxb->parent;
    }
    if (cb != &t->mdg.root_node->body) {
        if (escape_count < 3) {
            if (push_name_mangle_ident_node(
                    (escape_count == 1) ? "e" : "ee", buff, size)) {
                return ERR;
            }
        }
        else {
            if (push_name_mangle_ident_node("E", buff, size)) return ERR;
            if (push_name_mangle_id_node("E", escape_count, buff, size)) {
                return ERR;
            }
        }
    }
    else {
        *sep_required_before = true;
    }
    return OK;
}
int push_generic_args(
    tauc* t, sym_param_generic_inst* generic_args, ureg generic_args_count,
    ast_body* ctx, sbuffer* buff, ureg* size)
{
    if (generic_args_count == 0) return OK;
    if (push_name_mangle_ident_node("q", buff, size)) return ERR;
    bool sep_after = false;
    for (ureg i = generic_args_count; i != 0; i--) {
        assert(
            generic_args[i - 1].value->kind != EXPR_PASTE_EVALUATION); // TODO
        if (push_ctype(
                t, generic_args[i - 1].value, ctx, buff, size, sep_after,
                &sep_after))
            return ERR;
    }
    if (push_name_mangle_ident_node("Q", buff, size)) return ERR;
    return OK; // TODO
}
int push_params(
    tauc* t, sym_param* params, ureg param_count, bool generic, ast_body* ctx,
    sbuffer* buff, ureg* size)
{
    if (param_count == 0) return OK;
    if (push_name_mangle_ident_node("q", buff, size)) return ERR;
    bool sep_after = false;
    for (ureg i = param_count; i != 0; i--) {
        if (push_ctype(
                t, params[i - 1].ctype, ctx, buff, size, sep_after,
                &sep_after)) {
            return ERR;
        }
    }
    if (push_name_mangle_ident_node("Q", buff, size)) return ERR;
    return OK; // TODO
}

int name_mangle_raw(
    tauc* t, ast_node* n, ureg id, sbuffer* buff, pool* output_mem, ureg* size,
    char*** storage, ureg* unscoped_size, char*** unscoped_storage)
{
    switch (n->kind) {
        case SYM_VAR:
        case SYM_VAR_INITIALIZED: {
            symbol* s = (symbol*)n;
            assert(s->name);
            if (push_name_mangle_len_str_node(s->name, buff, size)) {
                return ERR;
            }
            *storage = &((sym_var_initialized*)n)->var.name_mangled;
            return push_scope_name(n, s->declaring_body, true, buff, size);
        }

        case SC_STRUCT: {
            sc_struct* s = (sc_struct*)n;
            if (push_name_mangle_len_str_node(
                    s->sb.sc.osym.sym.name, buff, size)) {
                return ERR;
            }
            *storage = &s->sb.name_mangled;
            *unscoped_storage = &s->sb.name_mangled_unscoped;
            return push_scope_name(n, s->sb.sc.body.parent, true, buff, size);
        }
        case SC_STRUCT_GENERIC: {
            sc_struct_generic* s = (sc_struct_generic*)n;
            ast_body* ctx = s->sb.sc.osym.sym.declaring_body;
            if (push_params(
                    t, s->generic_params, s->generic_param_count, true, ctx,
                    buff, size)) {
                return ERR;
            }
            if (push_name_mangle_len_str_node(
                    s->sb.sc.osym.sym.name, buff, size))
                return ERR;
            *storage = &s->sb.name_mangled;
            *unscoped_storage = &s->sb.name_mangled_unscoped;
            return push_scope_name(n, s->sb.sc.body.parent, true, buff, size);
        }
        case SC_STRUCT_GENERIC_INST: {
            sc_struct_generic_inst* s = (sc_struct_generic_inst*)n;
            ast_body* ctx = s->st.sb.sc.osym.sym.declaring_body;
            push_generic_args(
                t, s->generic_args, s->generic_arg_count, ctx, buff, size);
            if (push_name_mangle_len_str_node(
                    s->st.sb.sc.osym.sym.name, buff, size))
                return ERR;
            *unscoped_size = *size;
            *unscoped_storage = &s->st.sb.name_mangled_unscoped;
            *storage = &s->st.sb.name_mangled;
            return push_scope_name(
                n, s->st.sb.sc.body.parent, true, buff, size);
        }
        case SC_TRAIT: {
            sc_struct* tr = (sc_struct*)n;
            if (push_name_mangle_len_str_node(
                    tr->sb.sc.osym.sym.name, buff, size))
                return ERR;
            *unscoped_size = *size;
            *unscoped_storage = &tr->sb.name_mangled_unscoped;
            *storage = &tr->sb.name_mangled;
            return push_scope_name(n, tr->sb.sc.body.parent, true, buff, size);
        }
        case SC_TRAIT_GENERIC: {
            sc_trait_generic* tr = (sc_trait_generic*)n;
            ast_body* ctx = tr->sb.sc.osym.sym.declaring_body;
            if (push_params(
                    t, tr->generic_params, tr->generic_param_count, true, ctx,
                    buff, size)) {
                return ERR;
            }
            if (push_name_mangle_len_str_node(
                    tr->sb.sc.osym.sym.name, buff, size))
                return ERR;
            *unscoped_size = *size;
            *unscoped_storage = &tr->sb.name_mangled_unscoped;
            *storage = &tr->sb.name_mangled;
            return push_scope_name(n, tr->sb.sc.body.parent, true, buff, size);
        }
        case SC_TRAIT_GENERIC_INST: {
            sc_trait_generic_inst* tr = (sc_trait_generic_inst*)n;
            ast_body* ctx = tr->tr.sb.sc.osym.sym.declaring_body;
            if (push_generic_args(
                    t, tr->generic_args, tr->generic_arg_count, ctx, buff,
                    size)) {
                return ERR;
            }
            if (push_name_mangle_len_str_node(
                    tr->tr.sb.sc.osym.sym.name, buff, size)) {
                return ERR;
            }
            *unscoped_size = *size;
            *unscoped_storage = &tr->name_mangled_unscoped;
            *storage = &tr->tr.sb.name_mangled;
            return push_scope_name(
                n, tr->tr.sb.sc.body.parent, true, buff, size);
        }

        case SC_FUNC: {
            sc_func* f = (sc_func*)n;
            ast_body* ctx = f->fnb.sc.osym.sym.declaring_body;
            if (push_params(
                    t, f->fnb.params, f->fnb.param_count, false, ctx, buff,
                    size)) {
                return ERR;
            }
            if (push_name_mangle_len_str_node(
                    f->fnb.sc.osym.sym.name, buff, size))
                return ERR;
            *unscoped_size = *size;
            *unscoped_storage = &f->fnb.name_mangled_unscoped;
            *storage = &f->fnb.name_mangled;
            return push_scope_name(n, f->fnb.sc.body.parent, true, buff, size);
        }
        case SC_FUNC_GENERIC: {
            sc_func_generic* f = (sc_func_generic*)n;
            ast_body* ctx = f->fnb.sc.osym.sym.declaring_body;
            if (push_params(
                    t, f->fnb.params, f->fnb.param_count, false, ctx, buff,
                    size)) {
                return ERR;
            }
            if (push_params(
                    t, f->generic_params, f->generic_param_count, true, ctx,
                    buff, size)) {
                return ERR;
            }
            if (push_name_mangle_len_str_node(
                    f->fnb.sc.osym.sym.name, buff, size))
                return ERR;
            *unscoped_size = *size;
            *unscoped_storage = &f->fnb.name_mangled_unscoped;
            *storage = &f->fnb.name_mangled;
            return push_scope_name(n, f->fnb.sc.body.parent, true, buff, size);
        }
        case SC_FUNC_GENERIC_INST: {
            sc_func_generic_inst* f = (sc_func_generic_inst*)n;
            ast_body* ctx = f->fnb.fnb.sc.osym.sym.declaring_body;
            if (push_params(
                    t, f->fnb.fnb.params, f->fnb.fnb.param_count, false, ctx,
                    buff, size)) {
                return ERR;
            }
            if (push_generic_args(
                    t, f->generic_args, f->generic_arg_count, ctx, buff,
                    size)) {
                return ERR;
            }
            if (push_name_mangle_len_str_node(
                    f->fnb.fnb.sc.osym.sym.name, buff, size))
                return ERR;
            *unscoped_size = *size;
            *unscoped_storage = &f->fnb.fnb.name_mangled_unscoped;
            *storage = &f->fnb.fnb.name_mangled;
            return push_scope_name(
                n, f->fnb.fnb.sc.body.parent, true, buff, size);
        } break;

        case MF_MODULE:
        case MF_EXTEND: {
            module_frame* mf = (module_frame*)n;
            if (push_name_mangle_ident_node("a", buff, size)) return ERR;
            if (push_name_mangle_id_node("A", id, buff, size)) return ERR;
            *unscoped_size = *size;
            *unscoped_storage = &mf->name_mangled_unscoped;
            *storage = &mf->name_mangled;
            return push_scope_name(n, mf->body.parent, true, buff, size);
        }

        case EXPR_BLOCK:
        case EXPR_LOOP: {
            expr_block_base* ebb = (expr_block_base*)n;
            if (push_name_mangle_ident_node("a", buff, size)) return ERR;
            if (push_name_mangle_id_node("A", id, buff, size)) return ERR;
            *storage = &ebb->name_mangled_unscoped;
            return OK; // since we only store unscoped we cheat a little
        }

        case ELEM_MDG_NODE: {
            mdg_node* m = (mdg_node*)n;
            *storage = &m->name_mangled;
            while (m != t->mdg.root_node) {
                if (push_name_mangle_len_str_node(m->name, buff, size)) {
                    return ERR;
                }
                m = m->parent;
            }
            return push_name_mangle_str_node(NAME_BEGIN, buff, size);
        }
        default: break;
    }
    assert(false);
    return ERR;
}
int name_mangle(
    tauc* t, ast_node* node, ureg id, sbuffer* buff, pool* output_mem)
{
    sbuffer_iterator begin = sbuffer_iterator_begin_at_end((ptrlist*)buff);
    ureg size = 0;
    ureg unscoped_size = 0;
    char** storage = NULL;
    char** unscoped_storage = NULL;
    int res = name_mangle_raw(
        t, node, id, (ptrlist*)buff, output_mem, &size, &storage,
        &unscoped_size, &unscoped_storage);
    if (res) {
        sbuffer_set_end(buff, &begin);
        return ERR;
    }
    char* name = pool_alloc(output_mem, size + 1);
    char* tgt = name;
    sbuffer_iterator it = sbuffer_iterator_begin_at_end((ptrlist*)buff);
    while (true) {
        name_mangle_node* n =
            (name_mangle_node*)sbuffer_iterator_previous_until(
                &it, sizeof(name_mangle_node), &begin);
        if (!n) break;
        if (n->kind != NMNK_LEN_STR && n->kind != NMNK_STR) {
            for (char* c = n->ident; *c; c++) *tgt++ = *c;
        }
        switch (n->kind) {
            case NMNK_IDENT: break;
            case NMNK_IDENT_ID: {
                tgt += ureg_to_decimal_string(n->content.id, tgt);
            } break;
            case NMNK_IDENT_LEN_STR:
            case NMNK_LEN_STR: {
                tgt += ureg_to_decimal_string(n->str_len, tgt);
            } // fallthrough
            case NMNK_STR:
            case NMNK_IDENT_STR: {
                memcpy(tgt, n->content.str, n->str_len);
                tgt += n->str_len;
            } break;
            default: assert(false); break;
        }
    }
    *tgt = '\0';
    sbuffer_set_end(buff, &begin);
    assert(storage);
    *storage = name;
    if (unscoped_storage) {
        *unscoped_storage = name + (size - unscoped_size);
    }
    return OK;
}
