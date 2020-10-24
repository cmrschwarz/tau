#include "ast.h"
#include "utils/panic.h"
#include "utils/c_extensions.h"
#include <assert.h>
#include "src_map.h"

#define mk_prim(prim_kind, prim_name, ctype_kind)                              \
    [prim_kind] = {                                                            \
        .sym = {.node = {.kind = SYM_PRIMITIVE,                                \
                         .pt_kind = prim_kind,                                 \
                         .flags =                                              \
                             (NODE_STATUS_RESOLVED << ASTF_STATUS_OFFSET |     \
                              AM_PUBLIC << ASTF_ACCESS_MODIFIER_OFFSET),       \
                         .srange = SRC_RANGE_INVALID},                         \
                .name = prim_name,                                             \
                .next = NULL},                                                 \
        .ctype = (ast_elem*)&PRIMITIVES[ctype_kind]}
primitive PRIMITIVES[] = {
    mk_prim(PT_ERROR, "__error__", PT_ERROR),
    mk_prim(PT_FLUID_INT, "__fluid_int__", PT_ERROR),
    mk_prim(PT_VOID, "void", PT_TYPE),
    mk_prim(PT_GENERIC_TYPE, "Type[]", PT_ERROR),
    mk_prim(PT_GENERIC_TRAIT, "Trait[]", PT_ERROR),
    mk_prim(PT_UNREACHABLE, "unreachable", PT_ERROR),
    mk_prim(PT_UNDEFINED, "undefined", PT_ERROR),
    mk_prim(PT_DEFINED, "defined", PT_ERROR),
    mk_prim(PT_INT, "int", PT_TYPE),
    mk_prim(PT_UINT, "uint", PT_TYPE),
    mk_prim(PT_U8, "u8", PT_TYPE),
    mk_prim(PT_U16, "u16", PT_TYPE),
    mk_prim(PT_U32, "u32", PT_TYPE),
    mk_prim(PT_U64, "u64", PT_TYPE),
    mk_prim(PT_S8, "s8", PT_TYPE),
    mk_prim(PT_S16, "s16", PT_TYPE),
    mk_prim(PT_S32, "s32", PT_TYPE),
    mk_prim(PT_S64, "s64", PT_TYPE),
    mk_prim(PT_FLOAT, "float", PT_TYPE),
    mk_prim(PT_STRING, "string", PT_TYPE),
    mk_prim(PT_BINARY_STRING, "bstring", PT_TYPE),
    mk_prim(PT_VOID_PTR, "*void", PT_TYPE),
    mk_prim(PT_PASTED_EXPR, "Expr", PT_ERROR),
    mk_prim(PT_TYPE, "Type", PT_ERROR),
    mk_prim(PT_TRAIT, "Trait", PT_ERROR),
};

src_map* scope_get_smap(scope* s)
{
    return src_range_get_smap(s->osym.sym.node.srange);
}
src_map* module_frame_get_smap(module_frame* mf)
{
    return src_range_get_smap(mf->node.srange);
}
mdg_node* module_frame_get_module(module_frame* mf)
{
    ast_body* b = mf->body.parent;
    while (b->owning_node->kind != ELEM_MDG_NODE) b = b->parent;
    return (mdg_node*)b->owning_node;
}
void ast_node_get_full_src_range(
    ast_node* n, ast_body* body, src_range_large* srl)
{
    srl->smap = ast_body_get_smap(body);
    ast_node_get_bounds(n, &srl->start, &srl->end);
}
void ast_node_get_src_range(ast_node* n, ast_body* body, src_range_large* srl)
{
    src_range_unpack(n->srange, srl);
    if (!srl->smap) srl->smap = ast_body_get_smap(body);
}
char* ast_elem_get_label(ast_elem* n, bool* lbl)
{
    char* name;
    switch (n->kind) {
        case EXPR_BLOCK: {
            name = ((expr_block*)n)->ebb.name;
            if (lbl) *lbl = true;
        } break;
        case EXPR_MATCH: {
            name = ((expr_match*)n)->name;
            if (lbl) *lbl = true;
        } break;
        case EXPR_LOOP: {
            name = ((expr_loop*)n)->ebb.name;
            if (lbl) *lbl = true;
        } break;
        default: {
            name = NULL;
            if (lbl) *lbl = false;
        } break;
    }
    return name;
}
bool ast_elem_is_module_frame(ast_elem* s)
{
    return s->kind >= MF_FIRST_ID && s->kind <= MF_LAST_ID;
}
bool ast_elem_is_from_module(ast_elem* s)
{
    return ast_elem_is_module_frame(s) || s->kind == ELEM_MDG_NODE;
}
bool symbol_is_open_symbol(symbol* s)
{
    // since we know it's a symbol an upper bounds check is uneccessary
    return s->node.kind >= SYM_FIRST_OPEN_ID;
}
bool ast_elem_is_scope(ast_elem* s)
{
    return s->kind >= SC_FIRST_ID && s->kind <= SC_LAST_ID;
}
bool ast_elem_is_func_base(ast_elem* s)
{
    return s->kind == SC_FUNC || s->kind == SC_FUNC_GENERIC;
}
bool ast_elem_is_struct_base(ast_elem* s)
{
    return s->kind == SC_STRUCT || s->kind == SC_STRUCT_GENERIC ||
           s->kind == SC_STRUCT_GENERIC_INST;
}
bool ast_elem_is_trait_base(ast_elem* s)
{
    return s->kind == SC_TRAIT || s->kind == SC_TRAIT_GENERIC ||
           s->kind == SC_TRAIT_GENERIC_INST;
}
bool ast_elem_has_unordered_body(ast_elem* e)
{
    return ast_elem_is_trait_base(e) || ast_elem_is_struct_base(e) ||
           ast_elem_is_module_frame(e) || e->kind == ELEM_MDG_NODE;
}
bool ast_elem_is_any_import(ast_elem* s)
{
    switch (s->kind) {
        case SYM_IMPORT_MODULE:
        case SYM_IMPORT_PARENT:
        case SYM_NAMED_MOD_IMPORT_GROUP:
        case SYM_NAMED_SYM_IMPORT_GROUP:
        case ASTN_ANONYMOUS_SYM_IMPORT_GROUP:
        case ASTN_ANONYMOUS_MOD_IMPORT_GROUP: return true;
        default: return false;
    }
}
bool ast_elem_is_symbol(ast_elem* s)
{
    return (s->kind >= SYM_FIRST_ID && s->kind <= SYM_LAST_ID);
}
bool ast_elem_is_stmt(ast_elem* s)
{
    return (s->kind >= STMT_FIRST_ID && s->kind <= STMT_LAST_ID);
}
bool ast_elem_is_expr(ast_elem* s)
{
    return (s->kind >= EXPR_FIRST_ID && s->kind <= EXPR_LAST_ID);
}
bool ast_elem_is_type_slice(ast_elem* s)
{
    return (s->kind == TYPE_ARRAY || s->kind == TYPE_SLICE);
}
bool ast_elem_is_paste_evaluation(ast_elem* s)
{
    return s->kind == EXPR_PASTE_EVALUATION || s->kind == STMT_PASTE_EVALUATION;
}
bool assignment_is_meta_assignment(expr_op_binary* ob, bool* defined)
{
    assert(ast_node_get_resolved(&ob->node));
    if (ob->rhs->kind == EXPR_IDENTIFIER) {
        expr_identifier* id = (expr_identifier*)ob->rhs;
        if (id->value.sym == (symbol*)&PRIMITIVES[PT_UNDEFINED]) {
            if (defined) *defined = false;
            return true;
        }
        if (id->value.sym == (symbol*)&PRIMITIVES[PT_DEFINED]) {
            if (defined) *defined = true;
            return true;
        }
    }
    return false;
}
bool ast_body_is_braced(ast_body* b)
{
    if (b->elements[0] && !b->elements[1]) {
        return (b->srange != b->elements[0]->srange);
    }
    return true;
}
bool ast_elem_is_expr_block_base(ast_elem* n)
{
    return n->kind == EXPR_BLOCK || n->kind == EXPR_LOOP;
}
bool is_unary_op_postfix(operator_kind t)
{
    switch (t) {
        case OP_POST_INCREMENT:
        case OP_POST_DECREMENT: return true;
        default: return false;
    }
}
void ast_node_get_bounds(ast_node* n, ureg* start, ureg* end)
{
    switch (n->kind) {
        case EXPR_LOOP: {
            if (start) *start = src_range_get_start(n->srange);
            if (end) ast_node_get_bounds(n, NULL, end);
            return;
        }
        case EXPR_OP_BINARY: {
            if (start)
                ast_node_get_bounds(((expr_op_binary*)n)->lhs, start, NULL);
            if (end) ast_node_get_bounds(((expr_op_binary*)n)->rhs, NULL, end);
            return;
        }
        case EXPR_OP_UNARY: {
            expr_op_unary* u = (expr_op_unary*)n;
            if (!is_unary_op_postfix(u->node.op_kind)) {
                if (start) {
                    src_range_large r;
                    src_range_unpack(u->node.srange, &r);
                    *start = r.start;
                }
                if (end) ast_node_get_bounds(u->child, NULL, end);
                return;
            }
            // for postfix unary op
        } // fallthrough
        default: {
            src_range_large r;
            src_range_unpack(n->srange, &r);
            if (start) *start = r.start;
            if (end) *end = r.end;
            return;
        }
    }
}
char* op_to_str(operator_kind t)
{
    switch (t) {
        case OP_ASSIGN: return "=";
        case OP_ADD: return "+";
        case OP_ADD_ASSIGN: return "+=";
        case OP_SUB: return "-";
        case OP_SUB_ASSIGN: return "-=";
        case OP_MUL: return "*";
        case OP_MUL_ASSIGN: return "*=";
        case OP_DIV: return "/";
        case OP_DIV_ASSIGN: return "/=";
        case OP_MOD: return "%";
        case OP_MOD_ASSIGN: return "%=";
        case OP_LSHIFT: return "<<";
        case OP_LSHIFT_ASSIGN: return "<<=";
        case OP_RSHIFT: return ">>";
        case OP_RSHIFT_ASSIGN: return ">>=";
        case OP_LESS_THAN: return "<";
        case OP_LESS_THAN_OR_EQUAL: return "<=";
        case OP_GREATER_THAN: return ">";
        case OP_GREATER_THAN_OR_EQUAL: return ">=";
        case OP_EQUAL: return "==";
        case OP_UNEQAL: return "!=";
        case OP_AND: return "&&";
        case OP_BITWISE_AND: return "&";
        case OP_BITWISE_AND_ASSIGN: return "&=";
        case OP_OR: return "||";
        case OP_BITWISE_OR: return "|";
        case OP_BITWISE_OR_ASSIGN: return "|=";
        case OP_XOR: return "^^";
        case OP_BITWISE_XOR: return "^";
        case OP_BITWISE_XOR_ASSIGN: return "^=";
        case OP_BITWISE_NOT_ASSIGN: return "~=";

        case OP_DEREF: return "*";
        case OP_ADDRESS_OF: return "&";
        case OP_ESCAPE_SCOPE: return "^";
        case OP_NOT: return "!";
        case OP_BITWISE_NOT: return "~";
        case OP_UNARY_PLUS: return "+";
        case OP_UNARY_MINUS: return "-";
        case OP_PRE_INCREMENT: return "++";
        case OP_PRE_DECREMENT: return "--";
        case OP_POST_INCREMENT: return "++";
        case OP_POST_DECREMENT: return "--";
        case OP_MEMBER_ACCESS: return ".";
        case OP_PP: return "#";
        default: return "<Unknown Operator>";
    }
    return 0;
}
ast_body* ast_elem_try_get_body(ast_elem* s)
{
    if (ast_elem_is_scope(s)) {
        return &((scope*)s)->body;
    }
    switch (s->kind) {
        case EXPR_BLOCK: return &((expr_block*)s)->ebb.body;
        case EXPR_MATCH: return &((expr_match*)s)->body;
        case EXPR_LOOP: return &((expr_loop*)s)->ebb.body;
        case TRAIT_IMPL:
        case TRAIT_IMPL_GENERIC:
        case TRAIT_IMPL_GENERIC_INST: return &((trait_impl_base*)s)->body;
        case EXPR_PASTE_EVALUATION:
        case STMT_PASTE_EVALUATION: return &((paste_evaluation*)s)->body;
        case MF_EXTEND:
        case MF_MODULE: return &((module_frame*)s)->body;
        default: break;
    }
    return NULL;
}
ast_body* ast_elem_get_body(ast_elem* s)
{
    ast_body* res = ast_elem_try_get_body(s);
    if (!res) {
        panic("tried to get body from ast node without body");
        assert(false);
    }
    return res;
}
type_derivatives* ast_elem_get_type_derivs(ast_elem* e)
{
    switch (e->kind) {
        case SC_STRUCT_GENERIC_INST:
        case SC_STRUCT: {
            return &((sc_struct*)e)->type_derivs;
        } break;
        case SYM_PRIMITIVE: {
            return &((primitive*)e)->type_derivs;
        } break;
        case TYPE_ARRAY:
        case TYPE_SLICE:
        case TYPE_POINTER: {
            return &((type_base*)e)->type_derivs;
        } break;
        default: assert(false); break;
    }
    return NULL;
}
bool ast_elem_is_struct(ast_elem* s)
{
    return s->kind == SC_STRUCT || s->kind == SC_STRUCT_GENERIC_INST;
}
bool ast_elem_is_trait_impl(ast_elem* s)
{
    return s->kind == TRAIT_IMPL || s->kind == TRAIT_IMPL_GENERIC_INST;
}
bool ast_elem_is_var(ast_elem* s)
{
    return s->kind == SYM_VAR || s->kind == SYM_VAR_INITIALIZED;
}
bool ast_elem_is_node(ast_elem* e)
{
    return e->kind >= ASTN_FIRST_ID && e->kind <= ASTN_LAST_ID;
}
bool ast_body_is_public(ast_body* body)
{
    switch (body->owning_node->kind) {
        case ELEM_MDG_NODE: return true;
        case MF_EXTEND: return true;
        case MF_MODULE: return true;
        case STMT_PASTE_EVALUATION: return ast_body_is_public(body->parent);
        default: return false;
    }
}
ast_body* ast_body_get_non_paste_parent(ast_body* b)
{
    while (b->owning_node->kind == STMT_PASTE_EVALUATION ||
           b->owning_node->kind == EXPR_PASTE_EVALUATION) {
        b = b->parent;
        assert(b);
    }
    return b;
}
ast_body* ast_body_get_parent_module_body(ast_body* b)
{
    while (b->owning_node->kind != ELEM_MDG_NODE) {
        b = b->parent;
        assert(b);
    }
    return b;
}

src_map* ast_body_get_smap(ast_body* b)
{
    while (true) {
        // HACK: find a proper solution for this
        src_map* smap = src_range_get_smap(((ast_node*)b->owning_node)->srange);
        if (smap) return smap;
        b = b->parent;
        assert(b);
    }
}

void import_group_get_data(
    ast_node* n, import_group_data** ig_data, import_module_data** im_data,
    const char** name, mdg_node** group_parent)
{
    switch (n->kind) {
        case SYM_NAMED_MOD_IMPORT_GROUP: {
            sym_named_mod_import_group* ig = (sym_named_mod_import_group*)n;
            if (ig_data) *ig_data = &ig->ig_data;
            if (im_data) *im_data = NULL;
            if (name) *name = ig->osym.sym.name;
            if (group_parent) *group_parent = ig->group_parent;
        } break;
        case SYM_NAMED_SYM_IMPORT_GROUP: {
            sym_named_sym_import_group* ig = (sym_named_sym_import_group*)n;
            if (ig_data) *ig_data = &ig->ig_data;
            if (im_data) *im_data = &ig->im_data;
            if (name) *name = ig->osym.sym.name;
            if (group_parent) *group_parent = ig->im_data.imported_module;
        } break;
        case ASTN_ANONYMOUS_MOD_IMPORT_GROUP: {
            astn_anonymous_mod_import_group* ig =
                (astn_anonymous_mod_import_group*)n;
            if (ig_data)
                *ig_data = &((astn_anonymous_mod_import_group*)n)->ig_data;
            if (im_data) *im_data = NULL;
            if (name) *name = NULL;
            if (group_parent) *group_parent = ig->group_parent;
        } break;
        case ASTN_ANONYMOUS_SYM_IMPORT_GROUP: {
            astn_anonymous_sym_import_group* ig =
                (astn_anonymous_sym_import_group*)n;
            if (ig_data)
                *ig_data = &((astn_anonymous_sym_import_group*)n)->ig_data;
            if (im_data)
                *im_data = &((astn_anonymous_sym_import_group*)n)->im_data;
            if (name) *name = NULL;
            if (group_parent) *group_parent = ig->im_data.imported_module;
        } break;
        default: assert(false);
    }
}

bool ast_elem_is_import_group(ast_elem* s)
{
    switch (s->kind) {
        case SYM_NAMED_MOD_IMPORT_GROUP:
        case SYM_NAMED_SYM_IMPORT_GROUP:
        case ASTN_ANONYMOUS_SYM_IMPORT_GROUP:
        case ASTN_ANONYMOUS_MOD_IMPORT_GROUP: return true;
        default: return false;
    }
}

pp_resolve_node** ast_node_try_get_pprn_ptr(ast_node* n)
{
    switch (n->kind) {
        case SC_FUNC:
        case SC_FUNC_GENERIC: return &((sc_func_base*)n)->sc.body.pprn;
        case SYM_VAR:
        case SYM_VAR_INITIALIZED: return &((sym_var*)n)->pprn;
        case EXPR_BLOCK: return &((expr_block*)n)->ebb.body.pprn;
        case EXPR_LOOP: return &((expr_loop*)n)->ebb.body.pprn;
        case SC_STRUCT: return &((sc_struct*)n)->sb.sc.body.pprn;
        case SYM_NAMED_SYM_IMPORT_GROUP:
            return &((sym_named_sym_import_group*)n)->im_data.waiting_pprn.pprn;
        case ASTN_ANONYMOUS_SYM_IMPORT_GROUP:
            return &((astn_anonymous_sym_import_group*)n)
                        ->im_data.waiting_pprn.pprn;
        case SYM_IMPORT_MODULE:
            return &((sym_import_module*)n)->im_data.waiting_pprn.pprn;
        case EXPR_PP: return &((expr_pp*)n)->pprn;
        case EXPR_PASTE_EVALUATION:
        case STMT_PASTE_EVALUATION:
            return &((paste_evaluation*)n)->body.pprn;
            // used for freeing pp_exprs replaced by pastes
        default: return NULL;
    }
}
pp_resolve_node** ast_node_get_pprn_ptr(ast_node* n)
{
    pp_resolve_node** pprnp = ast_node_try_get_pprn_ptr(n);
    if (!pprnp) panic("invalid pprn node type");
    return pprnp;
}
