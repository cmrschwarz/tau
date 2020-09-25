#include "parser.h"
#include "error_log.h"
#include "file_map.h"
#include "print_ast.h"
#include "tauc.h"
#include "lexer.h"
#include "utils/math_utils.h"
#include "utils/panic.h"
#include "utils/zero.h"
#include "resolver.h"
#include "utils/debug_utils.h"
#include <stddef.h>
#include <assert.h>

#define PEEK(p, t)                                                             \
    do {                                                                       \
        t = lx_peek(&(p)->lx);                                                 \
        if (!t) return PE_LX_ERROR;                                            \
    } while (false)
#define PEEK_SND(p, t)                                                         \
    do {                                                                       \
        t = lx_peek_2nd(&(p)->lx);                                             \
        if (!t) return PE_LX_ERROR;                                            \
    } while (false)

bool body_supports_exprs(ast_node_kind pt);
typedef struct modifier_data_s {
    bool comptime_mod;
    access_modifier access_mod;
    bool extern_mod;
    bool static_mod;
    bool implicit_mod;
    bool const_mod;
    bool relative_import_mod;
    bool func_is_op_mod;
} modifier_data;

static const modifier_data MODIFIERS_NONE = {.comptime_mod = false,
                                             .access_mod = AM_NONE,
                                             .extern_mod = false,
                                             .static_mod = false,
                                             .implicit_mod = false,
                                             .const_mod = false,
                                             .relative_import_mod = false,
                                             .func_is_op_mod = false};

typedef struct modifier_status_s {
    modifier_data data;
    ureg start;
    ureg end;
} modifier_status;

parse_error parse_statement(parser* p, ast_node** tgt);
parse_error parse_scope_body(parser* p, scope* s, ureg param_count);
parse_error parse_module_frame_body(
    parser* p, module_frame* mf, mdg_node* n, ureg param_count);
parse_error parse_expression(parser* p, ast_node** tgt);
parse_error parse_expression_of_prec(parser* p, ast_node** ex, ureg prec);
parse_error parse_brace_delimited_body(
    parser* p, ast_body* b, ast_node* parent, ureg param_count);
parse_error parse_braced_namable_body(
    parser* p, ast_node* parent, ast_body* b, char** name);
parse_error parse_expr_in_parens(
    parser* p, ast_node* parent, ureg start, ureg end, ast_node** tgt);
static inline parse_error parse_delimited_body(
    parser* p, ast_body* b, ast_node* parent, ureg param_count,
    ast_node* first_stmt, ureg bstart, ureg bend, token_kind delimiter);
parse_error handle_semicolon_after_statement(parser* p, ast_node* s);

static const unsigned char op_precedence[] = {
    [OP_POST_INCREMENT] = 15,
    [OP_POST_DECREMENT] = 15,
    [OP_CALL] = 15,
    [OP_MACRO_CALL] = 15,
    [OP_MACRO_STR_CALL] = 15,
    [OP_ACCESS] = 15,
    [OP_SCOPE_ACCESS] = 15,
    [OP_MEMBER_ACCESS] = 15,
    [OP_PRE_INCREMENT] = 14,
    [OP_PRE_DECREMENT] = 14,
    [OP_UNARY_PLUS] = 14,
    [OP_UNARY_MINUS] = 14,
    [OP_NOT] = 14,
    [OP_BITWISE_NOT] = 14,
    [OP_DEREF] = 14,
    [OP_ADDRESS_OF] = 14,
    [OP_ESCAPE_SCOPE] = 14,
    [OP_PP] = 14,

    [OP_BITWISE_AND] = 13,

    [OP_BITWISE_XOR] = 12,

    [OP_BITWISE_OR] = 11,

    [OP_MUL] = 10,
    [OP_DIV] = 10,
    [OP_MOD] = 10,

    [OP_ADD] = 9,
    [OP_SUB] = 9,

    [OP_LSHIFT] = 8,
    [OP_RSHIFT] = 8,

    [OP_CAST] = 7,

    [OP_LESS_THAN] = 6,
    [OP_LESS_THAN_OR_EQUAL] = 6,
    [OP_GREATER_THAN] = 6,
    [OP_GREATER_THAN_OR_EQUAL] = 6,

    [OP_EQUAL] = 5,
    [OP_UNEQAL] = 5,

    [OP_AND] = 4,

    [OP_XOR] = 3,

    [OP_OR] = 2,

    [OP_ASSIGN] = 1,
    [OP_ADD_ASSIGN] = 1,
    [OP_SUB_ASSIGN] = 1,
    [OP_MUL_ASSIGN] = 1,
    [OP_DIV_ASSIGN] = 1,
    [OP_MOD_ASSIGN] = 1,
    [OP_LSHIFT_ASSIGN] = 1,
    [OP_RSHIFT_ASSIGN] = 1,
    [OP_BITWISE_AND_ASSIGN] = 1,
    [OP_BITWISE_XOR_ASSIGN] = 1,
    [OP_BITWISE_OR_ASSIGN] = 1,
    [OP_BITWISE_NOT_ASSIGN] = 1,
};

#define OP_PREC_BASELINE 0
#define OP_PREC_MAX 15

static inline bool is_left_associative(operator_kind t)
{
    switch (t) {
        case OP_ASSIGN:
        case OP_ADD_ASSIGN:
        case OP_SUB_ASSIGN:
        case OP_MUL_ASSIGN:
        case OP_DIV_ASSIGN:
        case OP_MOD_ASSIGN:
        case OP_LSHIFT_ASSIGN:
        case OP_RSHIFT_ASSIGN:
        case OP_BITWISE_AND_ASSIGN:
        case OP_BITWISE_XOR_ASSIGN:
        case OP_BITWISE_OR_ASSIGN:
        case OP_BITWISE_NOT_ASSIGN: return false;
        default: return true;
    }
}

static inline operator_kind token_to_binary_op(token* t)
{
    switch (t->kind) {
        case TK_PLUS: return OP_ADD;
        case TK_PLUS_EQUALS: return OP_ADD_ASSIGN;

        case TK_MINUS: return OP_SUB;
        case TK_MINUS_EQUALS: return OP_SUB_ASSIGN;

        case TK_STAR: return OP_MUL;
        case TK_STAR_EQUALS: return OP_MUL_ASSIGN;

        case TK_SLASH: return OP_DIV;
        case TK_SLASH_EQUALS: return OP_DIV_ASSIGN;

        case TK_PERCENT: return OP_MOD;
        case TK_PERCENT_EQUALS: return OP_MOD_ASSIGN;

        case TK_DOUBLE_LESS_THAN: return OP_LSHIFT;
        case TK_DOUBLE_LESS_THAN_EQUALS: return OP_LSHIFT_ASSIGN;

        case TK_DOUBLE_GREATER_THAN: return OP_RSHIFT;
        case TK_DOUBLE_GREATER_THAN_EQUALS: return OP_RSHIFT_ASSIGN;

        case TK_LESS_THAN: return OP_LESS_THAN;
        case TK_LESS_THAN_EQUALS: return OP_LESS_THAN_OR_EQUAL;

        case TK_GREATER_THAN: return OP_GREATER_THAN;
        case TK_GREATER_THAN_EQUALS: return OP_GREATER_THAN_OR_EQUAL;

        case TK_EQUALS: return OP_ASSIGN;
        case TK_DOUBLE_EQUALS: return OP_EQUAL;
        case TK_EXCLAMATION_MARK_EQUALS: return OP_UNEQAL;

        case TK_AND: return OP_BITWISE_AND;
        case TK_AND_EQUALS: return OP_BITWISE_AND_ASSIGN;
        case TK_DOUBLE_AND: return OP_AND;

        case TK_CARET: return OP_BITWISE_XOR;
        case TK_CARET_EQUALS: return OP_BITWISE_XOR_ASSIGN;
        case TK_DOUBLE_CARET: return OP_XOR;

        case TK_PIPE: return OP_BITWISE_OR;
        case TK_PIPE_EQUALS: return OP_BITWISE_OR_ASSIGN;
        case TK_DOUBLE_PIPE: return OP_OR;

        case TK_TILDE_EQUALS: return OP_BITWISE_NOT_ASSIGN;
        case TK_DOT: return OP_MEMBER_ACCESS;
        case TK_DOUBLE_COLON: return OP_SCOPE_ACCESS;

        default: return OP_NOOP;
    }
}
bool ast_node_may_drop_semicolon(ast_node* e)
{
    switch (e->kind) {
        case SC_FUNC:
        case SC_FUNC_GENERIC:
        case MF_MODULE:
        case MF_MODULE_GENERIC:
        case MF_EXTEND:
        case MF_EXTEND_GENERIC:
        case SC_STRUCT:
        case SC_STRUCT_GENERIC:
        case SC_TRAIT:
        case SC_TRAIT_GENERIC:
        case TRAIT_IMPL:
        case TRAIT_IMPL_GENERIC:
        case SC_MACRO:
        case EXPR_WHILE:
        case EXPR_LOOP:
        case EXPR_MATCH:
        case EXPR_MACRO_CALL:
        case EXPR_BLOCK: return true;
        case EXPR_IF: {
            expr_if* i = (expr_if*)e;
            if (i->else_body) {
                return (i->else_body->kind == EXPR_BLOCK);
            }
            return (i->if_body->kind == EXPR_BLOCK);
        }
        case EXPR_PP: {
            return ast_node_may_drop_semicolon(((expr_pp*)e)->pp_expr);
        }
        default: return false;
    }
}
static inline operator_kind token_to_prefix_unary_op(token* t)
{
    switch (t->kind) {
        case TK_MINUS: return OP_UNARY_MINUS;
        case TK_PLUS: return OP_UNARY_PLUS;
        case TK_TILDE: return OP_BITWISE_NOT;
        case TK_EXCLAMATION_MARK: return OP_NOT;
        case TK_STAR: return OP_DEREF;
        case TK_AND: return OP_ADDRESS_OF;
        case TK_PERCENT: return OP_DEREF;
        case TK_CARET: return OP_ESCAPE_SCOPE;
        case TK_DOLLAR: return OP_RREF_OF;
        case TK_DOUBLE_PLUS: return OP_PRE_INCREMENT;
        case TK_DOUBLE_MINUS: return OP_PRE_DECREMENT;
        default: return OP_NOOP;
    }
}
static inline operator_kind token_to_postfix_unary_op(parser* p, token* t)
{
    switch (t->kind) {
        case TK_KW_AS: return OP_CAST;
        case TK_DOUBLE_PLUS: return OP_POST_INCREMENT;
        case TK_DOUBLE_MINUS: return OP_POST_DECREMENT;
        case TK_PAREN_OPEN: return OP_CALL;
        case TK_EXCLAMATION_MARK: return OP_MACRO_STR_CALL;
        case TK_BRACE_OPEN: // fallthrough
        case TK_AT: {
            if (!p->disable_macro_body_call) return OP_MACRO_CALL;
            return OP_NOOP;
        }
        case TK_BRACKET_OPEN: return OP_ACCESS;
        default: return OP_NOOP;
    }
}
static inline bool is_kw_valid_label(token_kind t)
{
    switch (t) {
        case TK_KW_IF:
        case TK_KW_ELSE:
        case TK_KW_LOOP:
        case TK_KW_MATCH: return true;
        default: return false;
    }
}
static inline bool is_cond_kw(token* t, const char* cond_kw)
{
    return t->kind == TK_IDENTIFIER && string_cmp_cstr(t->str, cond_kw) == 0;
}
static inline void* alloc_ppool(parser* p, ureg size, pool* pool)
{
    return pool_alloc(pool, size);
}
static inline void* alloc_perm(parser* p, ureg size)
{
    return alloc_ppool(p, size, &p->lx.tc->permmem);
}
static inline char* alloc_string_ppool(parser* p, string s, pool* pool)
{
    ureg len = string_len(s);
    char* mem = (char*)alloc_ppool(
        p, ceil_to_mult_of_pow_two(len + 1, sizeof(void*)), pool);
    if (!mem) return NULL;
    memcpy(mem, s.start, len);
    mem[len] = '\0';
    return mem;
}
static inline char* alloc_string_temp(parser* p, string s)
{
    return alloc_string_ppool(p, s, &p->lx.tc->tempmem);
}
static inline char* alloc_string_perm(parser* p, string s)
{
    return alloc_string_ppool(p, s, &p->lx.tc->permmem);
}
static inline parse_error
parser_error_1a(parser* p, char* msg, ureg start, ureg end, char* annot)
{
    error_log_report_annotated(
        p->lx.tc->err_log, ES_PARSER, false, msg, p->lx.smap, start, end,
        annot);
    return PE_ERROR;
}
static inline parse_error parser_error_2a(
    parser* p, char* msg, ureg start, ureg end, char* annot, ureg start2,
    ureg end2, char* annot2)
{
    error_log_report_annotated_twice(
        p->lx.tc->err_log, ES_PARSER, false, msg, p->lx.smap, start, end, annot,
        p->lx.smap, start2, end2, annot2);
    return PE_ERROR;
}
static inline parse_error parser_error_3a(
    parser* p, char* msg, ureg start, ureg end, char* annot, ureg start2,
    ureg end2, char* annot2, ureg start3, ureg end3, char* annot3)
{
    error_log_report_annotated_thrice(
        p->lx.tc->err_log, ES_PARSER, false, msg, p->lx.smap, start, end, annot,
        p->lx.smap, start2, end2, annot2, p->lx.smap, start3, end3, annot3);
    return PE_ERROR;
}
void free_failed_ast_node_list_symtabs(
    list_builder* lb, void** list_start, ast_node*** intended_target)
{
    list_builder_rev_iter it;
    list_builder_rev_iter_init(&it, lb, list_start);
    while (true) {
        ast_node** n =
            (ast_node**)list_builder_rev_iter_prev(&it, sizeof(ast_node*));
        if (!n) break;
        free_astn_symtabs(*n);
    }
    list_builder_drop_list(lb, list_start);
    if (intended_target) *intended_target = (ast_node**)NULL_PTR_PTR;
}
static inline body_parse_data* get_bpd(parser* p)
{
    return (body_parse_data*)sbuffer_back(
        &p->body_stack, sizeof(body_parse_data));
}

static inline void
init_bpd(body_parse_data* bpd, ast_node* node, ast_body* body)
{
    bpd->node = node;
    bpd->body = body;
    const element_occurence_counts init_elem_counts = {0, 0, 0, 0};
    bpd->elem_counts = init_elem_counts;
    bpd->shared_elem_counts = init_elem_counts;
}
bool element_occurence_counts_zeroed(element_occurence_counts* eoc)
{
    return eoc->decl_count == 0 && eoc->usings_count == 0 &&
           eoc->impl_count == 0 && eoc->generic_impl_count == 0;
}
static inline int push_bpd(parser* p, ast_node* n, ast_body* b)
{
    body_parse_data* bpd =
        sbuffer_append(&p->body_stack, sizeof(body_parse_data));
    if (bpd == NULL) return ERR;
    init_bpd(bpd, n, b);
    if (bpd->body) {
        // this gets reset in pop_bpd,
        // but we need it early for get_non_paste_parent
        // to check for shared decls
        bpd->body->owning_node = n;
    }
    return OK;
}
static inline symbol_table*
create_symtab_from_eoc(element_occurence_counts* eoc)
{
    return symbol_table_create(
        eoc->decl_count, eoc->usings_count, eoc->impl_count,
        eoc->generic_impl_count);
}
static inline int
amend_potential_symtab(ast_body* b, element_occurence_counts* eoc)
{
    if (b->symtab) {
        int r =
            symbol_table_amend(&b->symtab, eoc->decl_count, eoc->usings_count);
        if (r) return r;
    }
    else {
        b->symtab = create_symtab_from_eoc(eoc);
        if (!b->symtab) return ERR;
    }
    return OK;
}
static inline int
handle_paste_bpd(parser* p, body_parse_data* bpd, symbol_table** st)
{
    if (!element_occurence_counts_zeroed(&bpd->elem_counts)) {
        ast_body* npp = ast_body_get_non_paste_parent(p->paste_parent_body);
        if (amend_potential_symtab(npp, &bpd->elem_counts)) return ERR;
    }
    if (!element_occurence_counts_zeroed(&bpd->shared_elem_counts)) {
        if (amend_potential_symtab(
                p->paste_parent_shared_body, &bpd->shared_elem_counts))
            return ERR;
    }
    bpd->body->symtab = NULL;
    bpd->body->parent = p->paste_parent_body;
    return OK;
}
static inline void drop_bpd(parser* p)
{
    sbuffer_remove_back(&p->body_stack, sizeof(body_parse_data));
}
static inline int pop_bpd(parser* p, parse_error prec_pe)
{
    sbuffer_iterator i = sbuffer_iterator_begin_at_end(&p->body_stack);
    body_parse_data bpd = *(body_parse_data*)sbuffer_iterator_previous(
        &i, sizeof(body_parse_data));
    sbuffer_remove_next(&p->body_stack, &i, sizeof(body_parse_data));
    if (prec_pe) return OK;
    ast_body* bd = bpd.body;
    bd->owning_node = bpd.node;
    if (bpd.body == p->paste_block) {
        return handle_paste_bpd(p, &bpd, &bd->symtab);
    }
    bool mf = ast_elem_is_module_frame((ast_elem*)bpd.node);
    if (!element_occurence_counts_zeroed(&bpd.shared_elem_counts)) {
        assert(mf);
        // assert(*mf is member of current module*);
        atomic_ureg_add(
            &p->current_module->decl_count, bpd.shared_elem_counts.decl_count);
        atomic_ureg_add(
            &p->current_module->using_count,
            bpd.shared_elem_counts.usings_count);
        atomic_ureg_add(
            &p->current_module->impl_count, bpd.shared_elem_counts.impl_count);
        atomic_ureg_add(
            &p->current_module->generic_impl_count,
            bpd.shared_elem_counts.generic_impl_count);
    }
    if (!element_occurence_counts_zeroed(&bpd.elem_counts)) {
        bd->symtab = create_symtab_from_eoc(&bpd.elem_counts);
        if (!bd->symtab) return ERR;
    }
    else {
        bd->symtab = NULL;
    }
    if (mf) {
        bd->parent = &p->current_module->body;
    }
    else {
        body_parse_data* parent = (body_parse_data*)sbuffer_iterator_previous(
            &i, sizeof(body_parse_data));
        assert(parent && parent->body);
        bd->parent = parent->body;
    }
    return OK;
}

char* get_context_msg(parser* p, ast_node* node)
{
    if (!node) return NULL;
    switch (node->kind) {
        case MF_MODULE: return "in this module";
        case MF_MODULE_GENERIC: return "in this generic module";
        case MF_EXTEND: return "in this extend statement";
        case MF_EXTEND_GENERIC: return "in this generic extend statement";
        case SC_FUNC: return "in this function";
        case SC_FUNC_GENERIC: return "in this generic function";
        case SC_STRUCT: return "in this struct";
        case SC_STRUCT_GENERIC: return "in this generic struct";
        case SC_TRAIT: return "in this struct";
        case SC_TRAIT_GENERIC: return "in this generic struct";
        case EXPR_BLOCK: return "in this block expression";
        case EXPR_DO_WHILE: return "in this do while expression";
        case EXPR_WHILE: return "in this while expression";
        case EXPR_LOOP: return "in this loop expression";
        case EXPR_CONTINUE: return "in this continue statement";
        case EXPR_BREAK: return "in this break statement";
        case EXPR_MATCH: return "in this match expression";
        case EXPR_IF: return "in this if expression";
        case EXPR_LAMBDA: return "in this lambda";
        case EXPR_PP: return "in this preprocessor expression";
        case SYM_VAR:
        case SYM_VAR_INITIALIZED: return "in this variable declaration";
        case TRAIT_IMPL: return "in this trait impl";
        case TRAIT_IMPL_GENERIC: return "in this generic trait impl";
        case SYM_IMPORT_SYMBOL:
        case SYM_NAMED_MOD_IMPORT_GROUP:
        case ASTN_ANONYMOUS_MOD_IMPORT_GROUP:
        case SYM_NAMED_SYM_IMPORT_GROUP:
        case ASTN_ANONYMOUS_SYM_IMPORT_GROUP:
        case SYM_IMPORT_MODULE: return "in this import statement";
        case SYM_NAMED_USE:
        case STMT_USE: return "in this using statement";
        case STMT_COMPOUND_ASSIGN:
            return "in this compound assignment statement";

        default: panic("unexpected parent context");
    }
    return NULL;
}
ast_body* get_current_body(parser* p)
{
    return get_bpd(p)->body;
}
static inline element_occurence_counts*
curr_scope_get_appropriate_eoc(parser* p, access_modifier am)
{
    body_parse_data* bpd = get_bpd(p);
    bool shared = false;
    if (am != AM_LOCAL && am != AM_NONE) {
        if (bpd->body && bpd->body->owning_node) {
            ast_body* npp = ast_body_get_non_paste_parent(bpd->body);
            if (ast_elem_is_module_frame((ast_elem*)npp->owning_node)) {
                shared = true;
            }
        }
    }
    if (shared) {
        return &bpd->shared_elem_counts;
    }
    else {
        return &bpd->elem_counts;
    }
}
static inline void
curr_scope_add_uses(parser* p, access_modifier am, ureg count)
{
    curr_scope_get_appropriate_eoc(p, am)->usings_count += count;
}
static inline void
curr_scope_add_decls(parser* p, access_modifier am, ureg count)
{
    curr_scope_get_appropriate_eoc(p, am)->decl_count += count;
}

static inline void
parser_error_1a_pc(parser* p, char* msg, ureg start, ureg end, char* annot)
{
    ast_node* parent = get_bpd(p)->node;
    if (parent != (ast_node*)p->file_root) {
        char* bpmmsg = get_context_msg(p, parent);
        if (bpmmsg != NULL) {
            src_range_large sr;
            src_range_unpack(parent->srange, &sr);
            parser_error_2a(
                p, msg, start, end, annot, sr.start, sr.end, bpmmsg);
        }
        return;
    }
    parser_error_1a(p, msg, start, end, annot);
}
static inline void parser_error_2a_pc(
    parser* p, char* msg, ureg start, ureg end, char* annot, ureg start2,
    ureg end2, char* annot2)
{
    ast_node* parent = get_bpd(p)->node;
    if (parent != (ast_node*)p->file_root) {
        char* bpmmsg = get_context_msg(p, parent);
        if (bpmmsg != NULL) {
            src_range_large sr;
            src_range_unpack(parent->srange, &sr);
            parser_error_3a(
                p, msg, start, end, annot, start2, end2, annot2, sr.start,
                sr.end, bpmmsg);
            return;
        }
    }
    parser_error_2a(p, msg, start, end, annot, start2, end2, annot2);
}
static inline void ast_node_init(ast_node* n, ast_node_kind kind)
{
    n->kind = kind;
    ast_node_set_default_flags(n);
}
static inline void ast_node_init_with_mods(
    ast_node* n, ast_node_kind kind, const modifier_data* md)
{
    ast_node_init(n, kind);
    if (md->comptime_mod) ast_node_set_comptime(n);
    ast_node_set_access_mod(
        n, md->access_mod == AM_NONE ? AM_LOCAL : md->access_mod);
    if (md->extern_mod) ast_node_set_extern_func(n);
    if (md->static_mod) ast_node_set_static(n);
    if (md->implicit_mod) ast_node_set_implicit(n);
    if (md->const_mod) ast_node_set_const(n);
    if (md->relative_import_mod) ast_node_set_relative_import(n);
    if (md->func_is_op_mod) ast_node_set_func_is_op(n);
}
static inline void ast_node_init_with_op(
    ast_node* n, ast_node_kind kind, operator_kind opk, const modifier_data* md)
{
    ast_node_init_with_mods(n, kind, md);
    n->op_kind = opk;
}
static inline void
ast_node_init_with_pk(ast_node* n, ast_node_kind kind, primitive_kind pk)
{
    ast_node_init(n, kind);
    n->pt_kind = pk;
}
int parser_init(parser* p, thread_context* tc)
{
    int r = lx_init(&p->lx, tc);
    if (r) return r;
    r = list_builder_init(&p->lx.tc->listb, &p->lx.tc->tempmem, 64);
    if (r) {
        lx_fin(&p->lx);
        return r;
    }
    r = sbuffer_init(&p->body_stack, sizeof(body_parse_data) * 16);
    if (r) {
        list_builder_fin(&p->lx.tc->listb);
        lx_fin(&p->lx);
        return r;
    }
    p->paste_block = NULL;
    p->disable_macro_body_call = false;
    return OK;
}
void parser_fin(parser* p)
{
    sbuffer_fin(&p->body_stack);
    lx_fin(&p->lx);
}
static inline parse_error
ast_node_fill_srange(parser* p, ast_node* n, ureg start, ureg end)
{
    n->srange = src_range_pack_lines(p->lx.tc, start, end);
    if (n->srange == SRC_RANGE_INVALID) return PE_FATAL;
    return PE_OK;
}

static inline parse_error
sym_fill_srange(parser* p, symbol* s, ureg start, ureg end)
{
    src_range_large srl;
    srl.start = start;
    srl.end = end;
    srl.smap = NULL;
    if (ast_node_get_access_mod(&s->node) != AM_LOCAL) {
        if (ast_elem_is_module_frame((ast_elem*)get_bpd(p)->node)) {
            srl.smap = p->lx.smap;
        }
    }
    s->node.srange = src_range_large_pack(p->lx.tc, &srl);
    if (s->node.srange == SRC_RANGE_INVALID) return PE_FATAL;
    return PE_OK;
}
static inline parse_error
parse_literal(parser* p, ast_node_kind nk, primitive_kind pk, ast_node** tgt)
{
    token* t = lx_aquire(&p->lx);
    expr_literal* l = (expr_literal*)alloc_perm(p, sizeof(expr_literal));
    if (!l) return PE_FATAL;
    ast_node_init_with_pk(&l->node, nk, pk);
    l->value.str = alloc_string_temp(p, t->str);
    if (!l->value.str) return PE_FATAL;
    if (ast_node_fill_srange(p, &l->node, t->start, t->end)) return PE_FATAL;
    lx_void(&p->lx);
    *tgt = (ast_node*)l;
    return PE_OK;
}
static inline parse_error parse_identifier(parser* p, ast_node** tgt)
{
    token* t = lx_aquire(&p->lx);
    expr_identifier* ident =
        (expr_identifier*)alloc_perm(p, sizeof(expr_identifier));
    if (!ident) return PE_FATAL;
    ast_node_init(&ident->node, EXPR_IDENTIFIER);
    ident->value.str = alloc_string_temp(p, t->str);
    if (!ident->value.str) return PE_FATAL;
    if (ast_node_fill_srange(p, &ident->node, t->start, t->end))
        return PE_FATAL;
    lx_void(&p->lx);
    *tgt = (ast_node*)ident;
    return PE_OK;
}
parse_error parse_param_decl(
    parser* p, sym_param* tgt, ureg ctx_start, ureg ctx_end, bool generic,
    char* msg_context)
{
    parse_error pe;
    token* t;
    PEEK(p, t);
    if (t->kind != TK_IDENTIFIER) {
        parser_error_2a(
            p, "invalid parameter syntax", t->start, t->end,
            "expected parameter identifier", ctx_start, ctx_end, msg_context);
        return PE_ERROR;
    }
    if (ast_node_fill_srange(p, (ast_node*)tgt, t->start, t->end))
        return PE_FATAL;
    tgt->sym.name = alloc_string_perm(p, t->str);
    if (!tgt->sym.name) return PE_FATAL;
    // TODO: flags parsing
    ast_node_init((ast_node*)tgt, generic ? SYM_GENERIC_PARAM : SYM_PARAM);
    lx_void(&p->lx);
    PEEK(p, t);
    if (t->kind != TK_COLON) {
        if (!generic) {
            parser_error_2a(
                p, "invalid parameter syntax", t->start, t->end,
                "expected ':' after identifier", ctx_start, ctx_end,
                msg_context);
            return PE_ERROR;
        }
        tgt->default_value = NULL;
        tgt->type = NULL;
        return PE_OK;
    }
    lx_void(&p->lx);
    PEEK(p, t);
    if (t->kind == TK_EQUALS) {
        lx_void(&p->lx);
        tgt->type = NULL;
        pe = parse_expression(p, &tgt->default_value);
        if (pe == PE_EOEX) {
            PEEK(p, t);
            parser_error_2a(
                p, "invalid parameter declaration syntax", t->start, t->end,
                "expected expression after '='", ctx_start, ctx_end,
                msg_context);
            return PE_ERROR;
        }
        if (pe) return pe;
    }
    else {
        pe = parse_expression_of_prec(
            p, &tgt->type, op_precedence[OP_EQUAL] + 1);
        if (pe == PE_EOEX) {
            PEEK(p, t);
            parser_error_2a(
                p, "invalid parameter declaration syntax", t->start, t->end,
                "expected type expression after ':'", ctx_start, ctx_end,
                msg_context);
            return PE_ERROR;
        }
        if (pe) return pe;
        PEEK(p, t);
        if (t->kind == TK_EQUALS) {
            lx_void(&p->lx);
            pe = parse_expression(p, &tgt->default_value);
            if (pe == PE_EOEX) {
                PEEK(p, t);
                parser_error_2a(
                    p, "invalid parameter declaration syntax", t->start, t->end,
                    "expected expression after '='", ctx_start, ctx_end,
                    msg_context);
                return PE_ERROR;
            }
            if (pe) return pe;
        }
        else {
            tgt->default_value = NULL;
        }
    }
    return PE_OK;
}
parse_error parse_expr_node_list(
    parser* p, ast_node* prefetch, ast_node*** tgt, ureg* elem_count,
    char* type, token_kind expected_trailer)
{
    token* t;
    PEEK(p, t);
    if (t->kind == expected_trailer) {
        if (!prefetch) {
            *tgt = NULL;
            *elem_count = 0;
            return PE_OK;
        }
        *tgt = alloc_perm(p, sizeof(ast_node*));
        if (!*tgt) return PE_FATAL;
        **tgt = prefetch;
        *elem_count = 1;
        return PE_OK;
    }
    void** list_start = list_builder_start(&p->lx.tc->listb2);
    if (prefetch) list_builder_add(&p->lx.tc->listb2, prefetch);
    while (true) {
        ast_node* ex;
        parse_error pe = parse_expression(p, &ex);
        if (pe == PE_EOEX) {
            PEEK(p, t);
            char* msg = error_log_cat_strings_3(
                p->lx.tc->err_log, "invalid ", type, " syntax");
            if (!msg) return PE_FATAL;
            parser_error_1a(
                p, msg, t->start, t->end, "expected expression after ','");
            return PE_ERROR;
        }
        if (pe != PE_OK) return pe;
        int r = list_builder_add(&p->lx.tc->listb2, (void*)ex);
        if (r) return PE_FATAL;
        PEEK(p, t);
        if (t->kind == TK_COMMA) {
            lx_void(&p->lx);
        }
        else if (t->kind == expected_trailer) {
            break;
        }
        else {
            return PE_ERROR;
        }
    }
    *tgt = (ast_node**)list_builder_pop_list(
        &p->lx.tc->listb2, list_start, &p->lx.tc->permmem, elem_count, 0, 0);
    if (!*tgt) {
        free_failed_ast_node_list_symtabs(&p->lx.tc->listb2, list_start, tgt);
        return PE_FATAL;
    }
    return PE_OK;
}
static inline parse_error
parse_array_or_slice(parser* p, ast_node** ex, ureg prec)
{
    token* t = lx_aquire(&p->lx);
    ureg t_start = t->start;
    ureg t_end = t->end;
    lx_void(&p->lx);
    PEEK(p, t);
    ast_node* arr_len;
    parse_error pe = PE_OK;
    if (t->kind == TK_BRACKET_CLOSE) {
        arr_len = NULL;
    }
    else if (t->kind == TK_IDENTIFIER && string_eq_cstr(t->str, "_")) {
        arr_len = (ast_node*)NULL_PTR_PTR;
        lx_void(&p->lx);
        PEEK(p, t);
    }
    else {
        pe = parse_expression(p, &arr_len);
        if (pe && pe != PE_EOEX) return pe;
        PEEK(p, t);
    }
    if (pe == PE_EOEX || t->kind != TK_BRACKET_CLOSE) {
        parser_error_2a(
            p, "array brackets missmatch", t->start, t->end,
            "expected ']' to end array bounds", t_start, t_end,
            "array type expression started here");
        return PE_ERROR;
    }
    lx_void(&p->lx);
    PEEK(p, t);
    bool const_arr = false;
    if (t->kind == TK_KW_CONST) {
        const_arr = true;
        lx_void(&p->lx);
    }
    ast_node* base_expr;
    bool dmbc = p->disable_macro_body_call;
    p->disable_macro_body_call = true;
    pe = parse_expression_of_prec(p, &base_expr, prec);
    p->disable_macro_body_call = dmbc;
    if (pe == PE_EOEX) {
        PEEK(p, t);
        parser_error_2a(
            p, "invalid array syntax", t->start, t->end,
            "expected array base type expression", t_start, t_end,
            "array declaration started here");
        return PE_ERROR;
    }
    if (pe) return pe;
    ureg type_expr_end;
    ast_node_get_bounds(base_expr, NULL, &type_expr_end); // PERF: :(
    expr_slice_type* ste;
    if (!arr_len) {
        ste = alloc_perm(p, sizeof(expr_slice_type));
        if (!ste) return PE_FATAL;
        ast_node_init(&ste->node, EXPR_SLICE_TYPE);
    }
    else {
        expr_array_type* arr = alloc_perm(p, sizeof(expr_array_type));
        if (!arr) return PE_FATAL;
        ste = (expr_slice_type*)arr;
        ast_node_init((ast_node*)arr, EXPR_ARRAY_TYPE);
        arr->length_spec = ((void**)arr_len == NULL_PTR_PTR) ? NULL : arr_len;
    }
    if (const_arr) ast_node_set_const(&ste->node);
    ste->base_type = base_expr;
    if (ast_node_fill_srange(p, &ste->node, t_start, t->end)) {
        return PE_FATAL;
    }
    PEEK(p, t);
    if (t->kind != TK_BRACE_OPEN) {
        *ex = (ast_node*)ste;
        return PE_OK;
    }
    lx_void(&p->lx);
    ast_node** elems = NULL;
    ureg elem_count;
    pe = parse_expr_node_list(
        p, NULL, &elems, &elem_count, "array", TK_BRACE_CLOSE);
    if (pe) return pe;
    t = lx_consume(&p->lx);
    if (!t) return PE_LX_ERROR;
    if (t->kind != TK_BRACE_CLOSE) {
        parser_error_2a(
            p, "array brace missmatch", t->start, t->end,
            "expected '}' to end array expression", t_start, t_end,
            "array declaration started here");
        return PE_ERROR;
    }
    expr_array* ea = alloc_perm(p, sizeof(expr_array));
    if (!ea) return PE_FATAL;
    ea->elem_count = elem_count;
    ea->elements = elems;
    ea->explicit_decl = ste;
    ast_node_init(&ea->node, EXPR_ARRAY);
    ast_node_fill_srange(p, &ea->node, t_start, t->end);
    *ex = (ast_node*)ea;
    return PE_OK;
}
static inline parse_error parse_tuple_after_first_comma(
    parser* p, ureg t_start, ureg t_end, ast_node** ex)
{
    expr_tuple* tp = alloc_perm(p, sizeof(expr_tuple));
    ast_node_init((ast_node*)tp, EXPR_TUPLE);
    void** list = list_builder_start(&p->lx.tc->listb2);
    list_builder_add(&p->lx.tc->listb2, *ex);
    token* t;
    PEEK(p, t);
    bool err = false;
    while (true) {
        if (t->kind == TK_PAREN_CLOSE) break;
        parse_error pe = parse_expression(p, ex);
        if (pe) {
            if (pe == PE_EOEX) {
                err = true;
                break;
            }
            return pe;
        }
        list_builder_add(&p->lx.tc->listb2, *ex);
        PEEK(p, t);
        if (t->kind == TK_COMMA) {
            lx_void(&p->lx);
            PEEK(p, t);
            continue;
        }
        if (t->kind == TK_PAREN_CLOSE) break;
        err = true;
        break;
    }
    if (err) {
        parser_error_2a(
            p, "tuple parenthesis missmatch", t->start, t->end,
            "reached end of expression due to unexpected token", t_start, t_end,
            "didn't find a matching parenthesis for this tuple");
        return PE_ERROR;
    }
    tp->elements = (ast_node**)list_builder_pop_list(
        &p->lx.tc->listb2, list, &p->lx.tc->permmem, &tp->elem_count, 0, 0);
    if (!tp->elements) {
        free_failed_ast_node_list_symtabs(
            &p->lx.tc->listb2, list, &tp->elements);
        return PE_FATAL;
    }
    if (ast_node_fill_srange(p, (ast_node*)tp, t_start, t->end))
        return PE_FATAL;
    lx_void(&p->lx);
    *ex = (ast_node*)tp;
    return PE_OK;
}
static inline parse_error
build_expr_parentheses(parser* p, ureg t_start, ureg t_end, ast_node** ex)
{
    token* t;
    PEEK(p, t);
    if (t->kind != TK_PAREN_CLOSE) {
        parser_error_2a(
            p, "parenthesis missmatch", t->start, t->end,
            "reached end of expression", t_start, t_end,
            "didn't find a match for this parenthesis");
        return PE_ERROR;
    }
    lx_void(&p->lx);
    expr_parentheses* pr =
        (expr_parentheses*)alloc_perm(p, sizeof(expr_parentheses));
    if (!pr) return PE_FATAL;
    ast_node_init((ast_node*)pr, EXPR_PARENTHESES);
    pr->child = *ex;
    if (ast_node_fill_srange(p, &pr->node, t_start, t->end)) return PE_FATAL;
    *ex = (ast_node*)pr;
    return PE_OK;
}
static inline parse_error
build_empty_tuple(parser* p, ureg t_start, ureg t_end, ast_node** ex)
{
    expr_tuple* tp = alloc_perm(p, sizeof(expr_tuple));
    if (!tp) return PE_FATAL;
    ast_node_init((ast_node*)tp, EXPR_TUPLE);
    if (ast_node_fill_srange(p, (ast_node*)tp, t_start, t_end)) return PE_FATAL;
    tp->elements = NULL;
    *ex = (ast_node*)tp;
    return PE_OK;
}
static inline parse_error
reject_modifiers(parser* p, token* t, modifier_status* mods)
{
    if (memcmp(&mods->data, &MODIFIERS_NONE, sizeof(modifier_data)) == 0) {
        return PE_OK;
    }
    char* loc_msg = error_log_cat_strings_2(
        p->lx.tc->err_log, token_strings[t->kind],
        " does not accept any modifiers");
    if (!loc_msg) return PE_FATAL;
    parser_error_2a(
        p, loc_msg, mods->start, mods->end, "invalid modifier(s)", t->start,
        t->end, "before this statement");
    return PE_ERROR;
}
static inline parse_error
parse_paren_group_or_tuple(parser* p, token* t, ast_node** ex)
{
    ureg t_start = t->start;
    ureg t_end = t->end;
    lx_void(&p->lx);
    PEEK(p, t);
    if (t->kind == TK_PAREN_CLOSE) {
        lx_void(&p->lx);
        return build_empty_tuple(p, t_start, t->end, ex);
    }
    bool disable_mbc = p->disable_macro_body_call;
    p->disable_macro_body_call = false;
    parse_error pe = parse_expression_of_prec(p, ex, OP_PREC_BASELINE);
    if (pe != PE_OK && pe != PE_EOEX) {
        p->disable_macro_body_call = disable_mbc;
        return pe;
    }
    t = lx_peek(&p->lx);
    if (!t) {
        p->disable_macro_body_call = disable_mbc;
        return PE_LX_ERROR;
    }
    if (t->kind == TK_COMMA) {
        lx_void(&p->lx);
        pe = parse_tuple_after_first_comma(p, t_start, t_end, ex);
    }
    else if (t->kind == TK_PAREN_CLOSE) {
        pe = build_expr_parentheses(p, t_start, t_end, ex);
    }
    else {
        parser_error_2a(
            p, "unexpected token after expression", t->start, t->end,
            "expected comma or closing parenthesis", t_start, t_end,
            "in parenthesized expression starting here");
        return PE_ERROR;
    }
    p->disable_macro_body_call = disable_mbc;
    return pe;
}
typedef union tuple_ident_node {
    sym_var var;
    expr_identifier ident;
} tuple_ident_node;

static inline parse_error
parse_uninitialized_var_in_tuple(parser* p, token* t, ast_node** ex)
{
    sym_var* v = alloc_perm(p, sizeof(tuple_ident_node));
    if (!v) return PE_FATAL;
    ast_node_init((ast_node*)v, SYM_VAR);
    v->osym.visible_within_body = NULL;
    ast_node_set_compound_decl(&v->osym.sym.node);
    v->osym.sym.name = alloc_string_perm(p, t->str);
    if (!v->osym.sym.name) return PE_FATAL;
    if (ast_node_fill_srange(p, (ast_node*)v, t->start, t->end))
        return PE_FATAL;
    lx_void_n(&p->lx, 2);
    PEEK(p, t);
    if (t->kind == TK_COMMA || t->kind == TK_PAREN_CLOSE) {
        v->type = NULL;
        *ex = (ast_node*)v;
        return PE_OK;
    }
    ureg t_start = t->start;
    parse_error pe = parse_expression(p, &v->type);
    if (!pe) {
        *ex = (ast_node*)v;
        return PE_OK;
    }
    if (pe == PE_EOEX) {
        PEEK(p, t);
        parser_error_1a(
            p, "invalid var declaration syntax", t_start, t->end,
            "expected a type expression or a comma");
        return PE_ERROR;
    }
    return pe;
}

static inline parse_error
build_ident_node_in_tuple(parser* p, token* t, ast_node** ex)
{
    tuple_ident_node* tin = alloc_perm(p, sizeof(tuple_ident_node));
    if (!tin) return PE_FATAL;
    sym_var* v = &tin->var;
    ast_node_init(&v->osym.sym.node, SYM_VAR);
    v->osym.sym.name = alloc_string_perm(p, t->str);
    v->osym.visible_within_body = NULL; // TODO
    if (!v->osym.sym.name) return PE_FATAL;
    if (ast_node_fill_srange(p, (ast_node*)v, t->start, t->end))
        return PE_FATAL;
    v->type = NULL;
    *ex = (ast_node*)tin;
    lx_void(&p->lx);
    return PE_OK;
}
static inline void turn_ident_nodes_to_exprs(ast_node** elems, ureg elem_count)
{
    for (ureg i = 0; i < elem_count; i++) {
        if ((**elems).kind == SYM_VAR) {
            ast_node* n = *elems;
            tuple_ident_node* tin = (tuple_ident_node*)n;
            if (tin->var.type == NULL && !ast_node_get_compound_decl(*elems)) {
                ureg srange = tin->var.osym.sym.node.srange;
                tin->ident.value.str = tin->var.osym.sym.name;
                n->srange = srange;
                ast_node_init(n, EXPR_IDENTIFIER);
            }
        }
        else if ((**elems).kind == EXPR_TUPLE) {
            expr_tuple* t = *((expr_tuple**)elems);
            turn_ident_nodes_to_exprs(t->elements, t->elem_count);
        }
        elems++;
    }
}

static inline parse_error
parse_paren_group_or_tuple_or_compound_decl_with_enabled_mbc(
    parser* p, token* t, ast_node** ex, ast_node*** elem_list, ureg* elem_count,
    ureg* list_end, ureg* decl_count, ureg* ident_count)
{
    parse_error pe;
    ureg t_start = t->start;
    ureg t_end = t->end;
    lx_void(&p->lx);
    PEEK(p, t);
    if (t->kind == TK_PAREN_CLOSE) {
        lx_void(&p->lx);
        return build_empty_tuple(p, t_start, t->end, ex);
    }
    void** element_list = NULL;
    if (t->kind != TK_IDENTIFIER) {
        pe = parse_expression(p, ex);
        if (pe == PE_EOEX) {
            PEEK(p, t);
            parser_error_2a(
                p, "unexpected token after opening parenthesis", t->start,
                t->end,
                "expected an expression, a declaration or a closing "
                "parenthesis",
                t_start, t_end, "opening parenthesis here");
            return PE_ERROR;
        }
        if (pe) return pe;
        PEEK(p, t);
        if (t->kind == TK_COMMA) {
            element_list = list_builder_start(&p->lx.tc->listb2);
            if (list_builder_add(&p->lx.tc->listb2, *ex)) return PE_FATAL;
        }
        else {
            pe = build_expr_parentheses(p, t_start, t_end, ex);
            return pe;
        }
    }
    else {
        token* t2;
        t2 = lx_peek_2nd(&p->lx);
        if (!t2) return PE_LX_ERROR;
        if (t2->kind == TK_COLON) {
            (*decl_count)++;
            pe = parse_uninitialized_var_in_tuple(p, t, ex);
            if (pe) return pe;
            PEEK(p, t);
        }
        else if (t2->kind != TK_COMMA) {
            pe = parse_expression(p, ex);
            if (pe) return pe;
            PEEK(p, t);
            if (t->kind == TK_PAREN_CLOSE) {
                pe = build_expr_parentheses(p, t_start, t_end, ex);
                return pe;
            }
            PEEK(p, t);
        }
        else {
            pe = build_ident_node_in_tuple(p, t, ex);
            if (pe) return pe;
            t = t2;
            (*ident_count)++;
        }
        element_list = list_builder_start(&p->lx.tc->listb2);
        if (list_builder_add(&p->lx.tc->listb2, *ex)) return PE_FATAL;
    }
    while (true) {
        if (t->kind == TK_COMMA) {
            lx_void(&p->lx);
            PEEK(p, t);
        }
        else if (t->kind != TK_PAREN_CLOSE) {
            parser_error_2a(
                p, "unexpected token in tuple", t->start, t->end,
                "expected a comma or a closing parenthesis", t_start, t_end,
                "tuple starts here");
            return PE_ERROR;
        }
        if (t->kind == TK_PAREN_CLOSE) {
            lx_void(&p->lx);
            ureg res_elem_count;
            ast_node** res_elem_list = (ast_node**)list_builder_pop_list(
                &p->lx.tc->listb2, element_list, &p->lx.tc->permmem,
                &res_elem_count, 0, 0);
            if (!res_elem_list) {
                free_failed_ast_node_list_symtabs(
                    &p->lx.tc->listb2, element_list, NULL);
                return PE_FATAL;
            }
            if (elem_list) {
                *elem_list = res_elem_list;
                *elem_count = res_elem_count;
                *list_end = t->end;
            }
            else {
                expr_tuple* tp = alloc_perm(p, sizeof(expr_tuple));
                if (!tp) return PE_FATAL;
                ast_node_init((ast_node*)tp, EXPR_TUPLE);
                if (ast_node_fill_srange(p, (ast_node*)tp, t_start, t->end)) {
                    return PE_FATAL;
                }
                tp->elements = res_elem_list;
                tp->elem_count = res_elem_count;
                *ex = (ast_node*)tp;
            }
            return PE_OK;
        }
        if (t->kind == TK_IDENTIFIER) {
            token* t2;
            t2 = lx_peek_2nd(&p->lx);
            if (!t2) return PE_LX_ERROR;
            if (t2->kind == TK_COLON) {
                pe = parse_uninitialized_var_in_tuple(p, t, ex);
                if (pe) return pe;
                (*decl_count)++;
            }
            else if (t2->kind == TK_COMMA || t2->kind == TK_PAREN_CLOSE) {
                pe = build_ident_node_in_tuple(p, t, ex);
                if (pe) return pe;
                (*ident_count)++;
            }
            else {
                pe = parse_expression(p, ex);
                if (pe) return pe;
            }
        }
        else if (t->kind == TK_PAREN_OPEN) {
            pe = parse_paren_group_or_tuple_or_compound_decl_with_enabled_mbc(
                p, t, ex, NULL, NULL, NULL, decl_count, ident_count);
            if (pe) return pe;
        }
        else {
            pe = parse_expression(p, ex);
            if (pe == PE_EOEX) {
                parser_error_2a(
                    p, "unexpected token in tuple", t->start, t->end,
                    "expected an expression, a declaration or a closing "
                    "parenthesis",
                    t_start, t_end, "tuple starts here");
                return PE_ERROR;
            }
            if (pe) return pe;
        }
        if (list_builder_add(&p->lx.tc->listb2, *ex)) return PE_FATAL;
        PEEK(p, t);
    }
}
static inline parse_error parse_paren_group_or_tuple_or_compound_decl(
    parser* p, token* t, ast_node** ex, ast_node*** elem_list, ureg* elem_count,
    ureg* list_end, ureg* decl_count, ureg* ident_count)
{
    bool disable_mbc = p->disable_macro_body_call;
    p->disable_macro_body_call = false;
    parse_error pe =
        parse_paren_group_or_tuple_or_compound_decl_with_enabled_mbc(
            p, t, ex, elem_list, elem_count, list_end, decl_count, ident_count);
    p->disable_macro_body_call = disable_mbc;
    return pe;
}
static inline parse_error
parse_prefix_unary_op(parser* p, operator_kind op, ast_node** ex)
{
    token* t = lx_aquire(&p->lx);
    expr_op_unary* ou = (expr_op_unary*)alloc_perm(p, sizeof(expr_op_unary));
    if (!ou) return PE_FATAL;
    if (ast_node_fill_srange(p, &ou->node, t->start, t->end)) return PE_FATAL;
    lx_void(&p->lx);
    if (op == OP_DEREF) {
        PEEK(p, t);
        if (t->kind == TK_KW_CONST) {
            ast_node_set_const(&ou->node);
            lx_void(&p->lx);
        }
    }
    ast_node_init_with_op((ast_node*)ou, EXPR_OP_UNARY, op, &MODIFIERS_NONE);
    parse_error pe = parse_expression_of_prec(
        p, &ou->child, op_precedence[op] + is_left_associative(op));
    if (pe) {
        PEEK(p, t);
        if (pe == PE_EOEX) {
            src_range_large s;
            src_range_unpack(ou->node.srange, &s);
            parser_error_2a(
                p, "missing operand for unary operator", t->start, t->end,
                "reached end of expression due to unexpected token", s.start,
                s.end, "missing operand for this operator");
        }
        return PE_ERROR;
    }
    *ex = (ast_node*)ou;
    return PE_OK;
}
parse_error get_breaking_target(
    parser* p, ast_node* requiring, ureg req_start, ureg req_end,
    const char** label, ureg* lbl_end)
{
    // TODO: break targets might be hidden due to macros
    token* t;
    PEEK(p, t);
    if (t->kind == TK_AT) {
        token* t2;
        PEEK_SND(p, t2);
        if (t2->kind == TK_IDENTIFIER) {
            char* lbl = alloc_string_perm(p, t2->str);
            if (!lbl) return PE_FATAL;
            *label = lbl;
        }
        else if (is_kw_valid_label(t2->kind)) {
            *label = token_strings[t2->kind];
        }
        else {
            parser_error_3a(
                p, "expected label identifier", t2->start, t2->end,
                "expected label identifier", t->start, t->end,
                "after this label indicator", req_start, req_end,
                get_context_msg(p, (ast_node*)requiring));
            return PE_ERROR;
        }
        lx_void_n(&p->lx, 2);
        *lbl_end = t2->end;
    }
    else {
        *label = NULL;
    }
    return PE_OK;
}
parse_error parse_continue(parser* p, ast_node** tgt)
{
    token* t = lx_aquire(&p->lx);
    ureg start = t->start;
    ureg end = t->end;
    lx_void(&p->lx);
    expr_continue* c = alloc_perm(p, sizeof(expr_continue));
    if (!c) return PE_FATAL;
    ast_node_init((ast_node*)c, EXPR_CONTINUE);
    parse_error pe = get_breaking_target(
        p, (ast_node*)c, start, end, &c->target.label, &end);
    if (pe) return pe;
    if (ast_node_fill_srange(p, (ast_node*)c, start, end)) return PE_FATAL;
    *tgt = (ast_node*)c;
    return PE_OK;
}
parse_error parse_return(parser* p, ast_node** tgt)
{
    token* t = lx_aquire(&p->lx);
    ureg start = t->start;
    ureg end = t->end;
    lx_void(&p->lx);
    expr_return* r = alloc_perm(p, sizeof(expr_return));
    if (!r) return PE_FATAL;
    ast_node_init((ast_node*)r, EXPR_RETURN);
    PEEK(p, t);
    if (t->kind == TK_SEMICOLON) {
        r->value = NULL;
        if (ast_node_fill_srange(p, (ast_node*)r, start, t->end)) {
            return PE_FATAL;
        }
    }
    else {
        parse_error pe = parse_expression(p, &r->value);
        if (pe == PE_EOEX) {
            PEEK(p, t);
            parser_error_2a(
                p, "unexpected token in return expression", t->start, t->end,
                "expected expression or ';'", start, end,
                "in this return expression");
            return PE_ERROR;
        }
        if (pe) return pe;
        if (ast_node_fill_srange(
                p, (ast_node*)r, start, src_range_get_end(r->value->srange)))
            return PE_FATAL;
    }
    if (r->node.srange == SRC_RANGE_INVALID) return PE_FATAL;
    *tgt = (ast_node*)r;
    r->target = NULL;
    return PE_OK;
}
parse_error parse_break(parser* p, ast_node** tgt)
{
    token* t = lx_aquire(&p->lx);
    ureg start = t->start;
    ureg end = t->end;
    lx_void(&p->lx);
    PEEK(p, t);
    expr_break* g = alloc_perm(p, sizeof(expr_break));
    if (!g) return PE_FATAL;
    ast_node_init((ast_node*)g, EXPR_BREAK);
    parse_error pe = get_breaking_target(
        p, (ast_node*)g, start, end, &g->target.label, &end);
    if (pe) return pe;
    pe = parse_expression(p, &g->value);
    if (pe == PE_EOEX) {
        g->value = NULL;
    }
    else {
        if (pe) return pe;
    }
    if (ast_node_fill_srange(p, (ast_node*)g, start, end)) return PE_FATAL;
    *tgt = (ast_node*)g;
    return PE_OK;
}

static inline parse_error parse_expr_block(
    parser* p, char* label, ureg bstart, ureg bend, ast_node* first_stmt,
    ast_node** ex)
{
    if (!first_stmt) lx_void(&p->lx);
    expr_block* b = alloc_perm(p, sizeof(expr_block));
    if (!b) return PE_FATAL;
    b->ebb.ctype = NULL;
    ast_node_init((ast_node*)b, EXPR_BLOCK);
    *ex = (ast_node*)b;
    b->ebb.name = label;
    parse_error pe = parse_delimited_body(
        p, &b->ebb.body, (ast_node*)b, 0, first_stmt, bstart, bend,
        TK_BRACE_CLOSE);
    if (pe) return pe;
    pe = ast_node_fill_srange(
        p, (ast_node*)b, bstart, src_range_get_end(b->ebb.body.srange));
    return pe;
}

static inline parse_error parse_expr_block_or_array(parser* p, ast_node** ex)
{
    token* t = lx_aquire(&p->lx);
    ureg bstart = t->start;
    ureg bend = t->end;
    lx_void(&p->lx);
    ast_node fake_node;
    fake_node.kind = EXPR_BLOCK;
    // TODO: deal with target searching break statements?
    if (push_bpd(p, &fake_node, NULL)) return PE_FATAL;
    ast_node* first_expr;
    parse_error pe = parse_statement(p, &first_expr);
    if (pe) {
        drop_bpd(p);
        return pe;
    }
    PEEK(p, t);
    if (t->kind == TK_SEMICOLON) lx_void(&p->lx);
    if (t->kind == TK_SEMICOLON || ast_node_may_drop_semicolon(first_expr)) {
        return parse_expr_block(p, NULL, bstart, bend, first_expr, ex);
    }
    if (t->kind == TK_COMMA) {
        lx_void(&p->lx);
    }
    drop_bpd(p);
    expr_array* arr = alloc_perm(p, sizeof(expr_array));
    ast_node_init((ast_node*)arr, EXPR_ARRAY);
    arr->ctype = NULL;
    if (!arr) return PE_FATAL;
    pe = parse_expr_node_list(
        p, first_expr, &arr->elements, &arr->elem_count, "array",
        TK_BRACE_CLOSE);
    if (pe) return pe;
    PEEK(p, t);
    assert(t->kind == TK_BRACE_CLOSE);
    lx_void(&p->lx);
    *ex = (ast_node*)arr;
    pe = ast_node_fill_srange(p, (ast_node*)arr, bstart, t->end);
    if (pe) return pe;
    arr->explicit_decl = NULL;
    return PE_OK;
}

parse_error parse_loop(parser* p, ast_node** tgt)
{
    token* t = lx_aquire(&p->lx);
    ureg start = t->start;
    lx_void(&p->lx);
    expr_loop* l = alloc_perm(p, sizeof(expr_loop));
    if (!l) return PE_FATAL;
    l->ebb.ctype = NULL;
    if (ast_node_fill_srange(p, (ast_node*)l, start, t->end)) return PE_FATAL;
    ast_node_init((ast_node*)l, EXPR_LOOP);
    *tgt = (ast_node*)l;
    l->ebb.name = NULL;
    return parse_braced_namable_body(
        p, (ast_node*)l, &l->ebb.body, &l->ebb.name);
}
parse_error parse_paste(parser* p, ast_node** tgt)
{
    token* t = lx_aquire(&p->lx);
    ureg start = t->start;
    lx_void(&p->lx);
    PEEK(p, t);
    if (t->kind != TK_PAREN_OPEN) {
        return parser_error_2a(
            p, "invalid paste syntax", t->start, t->end,
            "expected opening parenthesis", start, t->end, NULL);
    }
    lx_void(&p->lx);
    ast_node* expr;
    parse_error pe = parse_expression(p, &expr);
    if (pe) return pe;
    PEEK(p, t);
    if (t->kind != TK_PAREN_CLOSE) {
        return parser_error_2a(
            p, "invalid paste syntax", t->start, t->end,
            "expected paste string", start, t->end, NULL);
    }
    lx_void(&p->lx);
    expr_paste_str* ps = alloc_perm(p, sizeof(expr_paste_str));
    if (!ps) return PE_FATAL;
    ast_node_init((ast_node*)ps, EXPR_PASTE_STR);
    ps->value = NULL;
    pe = ast_node_fill_srange(p, (ast_node*)ps, start, t->end);
    if (pe) return pe;
    ps->value = expr;
    *tgt = (ast_node*)ps;
    return PE_OK;
}
parse_error parse_match(parser* p, ast_node** tgt)
{
    token* t = lx_aquire(&p->lx);
    ureg start = t->start;
    ureg t_end = t->end;
    ureg body_start;
    lx_void(&p->lx);
    expr_match* em = alloc_perm(p, sizeof(expr_match));
    if (!em) return PE_FATAL;
    ast_node_init((ast_node*)em, EXPR_MATCH);
    parse_error pe =
        parse_expr_in_parens(p, (ast_node*)em, start, t_end, &em->match_expr);
    if (pe == PE_EOEX) {
        PEEK(p, t);
        parser_error_2a(
            p, "invalid match syntax", t->start, t->end,
            "expected match expression", start, t_end, "in this match");
    }
    if (pe) return pe;
    ast_node_fill_srange(
        p, (ast_node*)em, start, src_range_get_end(em->match_expr->srange));
    PEEK(p, t);
    if (t->kind == TK_AT) {
        token* t2;
        PEEK_SND(p, t2);
        if (t2->kind != TK_IDENTIFIER) {
            parser_error_2a(
                p, "invalid label syntax", t2->start, t2->end,
                "expected label identifier", t->start, t->end,
                "label started here");
            return PE_ERROR;
        }
        em->name = alloc_string_perm(p, t2->str);
        if (!em->name) return PE_FATAL;
        body_start = t->start;
        ureg lbl_end = t2->end;
        lx_void_n(&p->lx, 2);
        PEEK(p, t);
        if (t->kind != TK_BRACE_OPEN) {
            parser_error_3a(
                p, "invalid match syntax", t->start, t->end,
                "expected opening brace", body_start, lbl_end,
                "after this label", start, t_end, "in this match expression");
            return PE_ERROR;
        }
    }
    else if (t->kind != TK_BRACE_OPEN) {
        ureg e_end;
        ast_node_get_bounds((ast_node*)em, NULL, &e_end);
        parser_error_2a(
            p, "invalid match syntax", t->start, t->end,
            "expected block or labeled block", start, e_end, "in this match");
        return PE_ERROR;
    }
    body_start = t->start;
    em->name = NULL;
    lx_void(&p->lx);
    push_bpd(p, (ast_node*)em, &em->body);
    void** list = list_builder_start(&p->lx.tc->listb2);
    while (true) {
        PEEK(p, t);
        if (t->kind == TK_BRACE_CLOSE) {
            em->body.elements = (ast_node**)list_builder_pop_list_zt(
                &p->lx.tc->listb2, list, &p->lx.tc->permmem);
            if (!em->body.elements) {
                free_failed_ast_node_list_symtabs(
                    &p->lx.tc->listb2, list, NULL);
                pop_bpd(p, PE_FATAL);
                return PE_FATAL;
            }
            *tgt = (ast_node*)em;
            lx_void(&p->lx);
            em->body.srange =
                src_range_pack(p->lx.tc, body_start, t->end, p->lx.smap);
            pop_bpd(p, PE_OK);
            return PE_OK;
        }
        match_arm* ma = alloc_perm(p, sizeof(match_arm));
        if (!ma) return PE_FATAL;
        pe = parse_expression(p, &ma->condition);
        if (pe == PE_EOEX) {
            parser_error_1a(
                p, "invalid match syntax", t->start, t->end,
                "expected match arm condition");
            pe = PE_ERROR;
            break;
        }
        if (pe) return pe;
        t = lx_peek(&p->lx);
        if (!t) {
            pe = PE_LX_ERROR;
            break;
        }
        if (t->kind != TK_FAT_ARROW) {
            ureg exp_start, exp_end;
            ast_node_get_bounds(ma->condition, &exp_start, &exp_end);
            parser_error_2a(
                p, "invalid match syntax", t->start, t->end, "expected '=>'",
                exp_start, exp_end, "after this match condition");
            pe = PE_ERROR;
            break;
        }
        lx_void(&p->lx);
        pe = parse_expression(p, &ma->value);
        if (pe == PE_EOEX) {
            PEEK(p, t);
            parser_error_1a_pc(
                p, "invalid match syntax", t->start, t->end,
                "expected match arm expression");
            pe = PE_ERROR;
            break;
        }
        if (pe) return pe;
        t = lx_peek(&p->lx);
        if (!t) {
            pe = PE_LX_ERROR;
            break;
        }
        if (t->kind == TK_SEMICOLON) {
            lx_void(&p->lx);
        }
        else if (ma->value->kind != EXPR_BLOCK) {
            ureg arm_start, arm_end;
            ast_node_get_bounds(ma->condition, &arm_start, NULL);
            arm_end = src_range_get_end(ma->value->srange);
            parser_error_2a(
                p, "invalid match syntax", t->start, t->end,
                "expected semicolon", arm_start, arm_end,
                "after this match arm expression");
            pe = PE_ERROR;
            break;
        }
        list_builder_add(&p->lx.tc->listb2, ma);
    }
    list_builder_drop_list(&p->lx.tc->listb2, list);
    pop_bpd(p, pe);
    return pe;
}
static inline parse_error parse_labeled_block(parser* p, ast_node** tgt)
{
    token* t = lx_aquire(&p->lx);
    ureg start = t->start;
    token* t2;
    PEEK_SND(p, t2);
    if (t2->kind != TK_IDENTIFIER) {
        parser_error_2a(
            p, "invalid label syntax", t2->start, t2->end,
            "expected label identifier", t->start, t->end,
            "label started here");
        return PE_ERROR;
    }
    char* label = alloc_string_perm(p, t2->str);
    if (!label) return PE_FATAL;
    ureg lbl_end = t2->end;
    lx_void_n(&p->lx, 2);
    PEEK(p, t);
    if (t->kind != TK_BRACE_OPEN) {
        parser_error_2a(
            p, "expected block after label", t->start, t->end,
            "expected open brace to begin block", start, lbl_end,
            "after this block label");
        return PE_ERROR;
    }
    return parse_expr_block(p, label, start, t->end, NULL, tgt);
}
parse_error parse_control_block(parser* p, ast_node** tgt)
{
    token* t;
    PEEK(p, t);
    if (t->kind == TK_BRACE_OPEN) {
        return parse_expr_block(p, NULL, t->start, t->end, NULL, tgt);
    }
    if (t->kind == TK_AT) {
        return parse_labeled_block(p, tgt);
    }
    return parse_expression(p, tgt);
}
parse_error parse_if(parser* p, ast_node** tgt)
{
    token* t = lx_aquire(&p->lx);
    ureg start = t->start;
    ureg end = t->end;
    lx_void(&p->lx);
    expr_if* i = alloc_perm(p, sizeof(expr_if));
    if (!i) return PE_FATAL;
    i->prpbn = NULL;
    i->ctype = NULL;
    parse_error pe =
        parse_expr_in_parens(p, (ast_node*)i, start, end, &i->condition);
    if (pe) return pe;
    if (ast_node_fill_srange(p, (ast_node*)i, start, end)) return PE_FATAL;
    ast_node_init((ast_node*)i, EXPR_IF);
    *tgt = (ast_node*)i;
    pe = parse_control_block(p, &i->if_body);
    if (pe) return pe;
    PEEK(p, t);
    if (t->kind == TK_KW_ELSE) {
        lx_void(&p->lx);
        pe = parse_control_block(p, &i->else_body);
    }
    else {
        i->else_body = NULL;
    }
    return pe;
}
static inline parse_error parse_pp_expr(parser* p, ast_node** tgt)
{
    token* t = lx_aquire(&p->lx);
    ureg start = t->start;
    lx_void(&p->lx);
    expr_pp* sp = alloc_perm(p, sizeof(expr_pp));
    if (!sp) return PE_FATAL;
    sp->ctype = NULL;
    ast_node_init(&sp->node, EXPR_PP);
    ast_node_set_pp_expr_res_used(&sp->node);
    sp->pprn = NULL;
    sp->result = NULL;
    sp->result_buffer.pasted_src = NULL;
    parse_error pe =
        parse_expression_of_prec(p, &sp->pp_expr, op_precedence[OP_PP]);
    if (pe) return pe;
    pe = ast_node_fill_srange(
        p, (ast_node*)sp, start, src_range_get_end(sp->pp_expr->srange));
    *tgt = (ast_node*)sp;
    return pe;
}
static inline parse_error parse_value_expr(parser* p, ast_node** ex, ureg prec)
{
    token* t;
    PEEK(p, t);
    switch (t->kind) {
        case TK_PAREN_OPEN: return parse_paren_group_or_tuple(p, t, ex);

        case TK_BRACKET_OPEN: return parse_array_or_slice(p, ex, prec);

        case TK_BRACE_OPEN: return parse_expr_block_or_array(p, ex);

        case TK_KW_LOOP: return parse_loop(p, ex);

        case TK_KW_MATCH: return parse_match(p, ex);

        case TK_HASH: return parse_pp_expr(p, ex);

        case TK_KW_IF: return parse_if(p, ex);

        case TK_KW_PASTE: return parse_paste(p, ex);

        case TK_KW_RETURN: return parse_return(p, ex);

        case TK_KW_BREAK: return parse_break(p, ex);

        case TK_KW_CONTINUE: return parse_continue(p, ex);

        case TK_NUMBER: return parse_literal(p, EXPR_LITERAL, PT_FLUID_INT, ex);
        case TK_STRING: return parse_literal(p, EXPR_LITERAL, PT_STRING, ex);
        case TK_BINARY_STRING:
            // TODO: handle this properly
            return parse_literal(p, EXPR_LITERAL, PT_BINARY_STRING, ex);

        case TK_IDENTIFIER: return parse_identifier(p, ex);
        case TK_AT: return parse_labeled_block(p, ex);

        default: {
            return PE_EOEX; // investigate: shouldn't this be unexp. tok?
        } break;
    }
}
static inline parse_error
parse_macro_block_call(parser* p, ast_node** ex, ast_node* lhs)
{
    expr_macro_call* emc = alloc_perm(p, sizeof(expr_macro_call));
    if (!emc) return PE_FATAL;
    emc->arg_count = 0;
    emc->args = (ast_node**)NULL_PTR_PTR;
    ast_node_init((ast_node*)emc, EXPR_MACRO_CALL);
    emc->lhs = lhs;
    parse_error pe =
        parse_braced_namable_body(p, (ast_node*)emc, &emc->body, &emc->name);
    if (pe) return pe;
    if (ast_node_fill_srange(
            p, &emc->node, src_range_get_start(lhs->srange),
            src_range_get_end(emc->body.srange))) {
        return PE_FATAL;
    }
    *ex = (ast_node*)emc;
    return PE_OK;
}
static inline parse_error
parse_macro_str_call(parser* p, ast_node** ex, ast_node* lhs)
{
    ureg start = lx_aquire(&p->lx)->start; // initial '!'
    lx_void(&p->lx);
    expr_macro_str_call* emsc = alloc_perm(p, sizeof(expr_macro_str_call));
    if (!emsc) return PE_FATAL;
    emsc->lhs = lhs;
    ast_node_init((ast_node*)emsc, EXPR_MACRO_STR_CALL);
    token* t = lx_consume_macro_string(&p->lx);
    if (!t) return PE_LX_ERROR;
    emsc->str_param.start = alloc_string_perm(p, t->str);
    if (!emsc->str_param.start) return PE_FATAL;
    emsc->str_param.end = ptradd(emsc->str_param.start, string_len(t->str));
    *ex = (ast_node*)emsc;
    return ast_node_fill_srange(p, (ast_node*)emsc, start, t->end);
}
static inline parse_error parse_call(parser* p, ast_node** ex, ast_node* lhs)
{
    token* t = lx_aquire(&p->lx);
    ureg t_start = t->start;
    lx_void(&p->lx);
    ureg arg_count;
    ast_node** args;
    parse_error pe = parse_expr_node_list(
        p, NULL, &args, &arg_count, "call", TK_PAREN_CLOSE);
    // EMSG: suboptimal e.g. for case {,,}
    if (pe == PE_ERROR) {
        PEEK(p, t);
        parser_error_2a(
            p, "unclosed call expression", t->start, t->end,
            "reached end of expression due to unexpected token", t_start,
            t_start + 1, "didn't find a matching parenthesis for this");
        return PE_ERROR;
    }
    if (pe != PE_OK) return pe;
    PEEK(p, t);
    ureg paren_end = t->end;
    lx_void(&p->lx);
    PEEK(p, t);
    if (t->kind == TK_BRACE_OPEN || t->kind == TK_AT) {
        expr_macro_call* emc = alloc_perm(p, sizeof(expr_macro_call));
        if (!emc) return PE_FATAL;
        emc->arg_count = arg_count;
        emc->args = args;
        ast_node_init((ast_node*)emc, EXPR_MACRO_CALL);
        emc->lhs = lhs;
        pe = parse_braced_namable_body(
            p, (ast_node*)emc, &emc->body, &emc->name);
        if (pe) return pe;
        if (ast_node_fill_srange(
                p, &emc->node, src_range_get_start(lhs->srange),
                src_range_get_end(emc->body.srange))) {
            return PE_FATAL;
        }
        *ex = (ast_node*)emc;
        return PE_OK;
    }
    else {
        expr_call* call = alloc_perm(p, sizeof(expr_call));
        if (!call) return PE_FATAL;
        call->arg_count = arg_count;
        call->args = args;
        if (ast_node_fill_srange(p, &call->node, t_start, paren_end))
            return PE_FATAL;
        // TODO: do we really need the OP_CALL here?
        ast_node_init_with_op(
            (ast_node*)call, EXPR_CALL, OP_CALL, &MODIFIERS_NONE);
        call->lhs = lhs;
        *ex = (ast_node*)call;
        return PE_OK;
    }
}
static inline parse_error parse_access(parser* p, ast_node** ex, ast_node* lhs)
{
    token* t = lx_aquire(&p->lx);
    ureg t_start = t->start;
    lx_void(&p->lx);
    expr_access* acc = alloc_perm(p, sizeof(expr_access));
    if (!acc) return PE_FATAL;
    parse_error pe = parse_expr_node_list(
        p, NULL, &acc->args, &acc->arg_count, "access operator",
        TK_BRACKET_CLOSE);
    // EMSG: suboptimal e.g. for case {,,}
    if (pe == PE_ERROR) {
        PEEK(p, t);
        parser_error_2a(
            p, "unclosed access operator", t->start, t->end,
            "reached end of expression due to unexpected token", t_start,
            t_start + 1, "didn't find a matching bracket for this");
        return PE_ERROR;
    }
    if (pe != PE_OK) return pe;
    PEEK(p, t);
    if (ast_node_fill_srange(p, &acc->node, t_start, t->end)) return PE_FATAL;
    lx_void(&p->lx);
    ast_node_init_with_op(
        (ast_node*)acc, EXPR_ACCESS, OP_ACCESS, &MODIFIERS_NONE);
    acc->lhs = lhs;
    *ex = (ast_node*)acc;
    return PE_OK;
}
static inline parse_error
parse_expr_cast(parser* p, ast_node** ex, ast_node* lhs)
{
    token* t = lx_aquire(&p->lx);
    lx_void(&p->lx);

    expr_cast* ec = (expr_cast*)alloc_perm(p, sizeof(expr_cast));
    if (!ec) return PE_FATAL;
    if (ast_node_fill_srange(p, &ec->node, t->start, t->end)) return PE_FATAL;
    ast_node_init((ast_node*)ec, EXPR_CAST);
    ec->value = lhs;
    parse_error pe =
        parse_expression_of_prec(p, &ec->target_type, OP_PREC_MAX + 1);
    if (pe) {
        if (pe == PE_EOEX) {
            PEEK(p, t);
            src_range_large sr;
            src_range_unpack(ec->node.srange, &sr);
            parser_error_2a(
                p, "missing target type for cast operator", t->start, t->end,
                "reached end of expression", sr.start, sr.end,
                "missing type for this cast");
            return PE_ERROR;
        }
        return pe;
    }
    *ex = (ast_node*)ec;
    return PE_OK;
}
static inline parse_error parse_postfix_unary_op(
    parser* p, operator_kind op, ast_node** ex, ast_node* lhs)
{
    if (op == OP_CALL) {
        return parse_call(p, ex, lhs);
    }
    else if (op == OP_ACCESS) {
        return parse_access(p, ex, lhs);
    }
    else if (op == OP_MACRO_CALL) {
        return parse_macro_block_call(p, ex, lhs);
    }
    else if (op == OP_MACRO_STR_CALL) {
        return parse_macro_str_call(p, ex, lhs);
    }
    else if (op == OP_CAST) {
        return parse_expr_cast(p, ex, lhs);
    }
    token* t = lx_aquire(&p->lx);
    lx_void(&p->lx);
    expr_op_unary* ou = (expr_op_unary*)alloc_perm(p, sizeof(expr_op_unary));
    if (!ou) return PE_FATAL;
    ast_node_init_with_op((ast_node*)ou, EXPR_OP_UNARY, op, &MODIFIERS_NONE);
    ou->child = lhs;
    if (ast_node_fill_srange(p, &ou->node, t->start, t->end)) return PE_FATAL;
    *ex = (ast_node*)ou;
    return PE_OK;
}
static inline parse_error
parse_scope_access(parser* p, ast_node** ex, ast_node* lhs, bool member)
{
    token* t = lx_aquire(&p->lx);
    lx_void(&p->lx);
    expr_scope_access* esa =
        (expr_scope_access*)alloc_perm(p, sizeof(expr_scope_access));
    if (!esa) return PE_FATAL;
    if (ast_node_fill_srange(p, &esa->node, t->start, t->end)) return PE_FATAL;
    ast_node_init_with_op(
        (ast_node*)esa, member ? EXPR_MEMBER_ACCESS : EXPR_SCOPE_ACCESS,
        member ? OP_MEMBER_ACCESS : OP_SCOPE_ACCESS, &MODIFIERS_NONE);
    esa->lhs = lhs;
    PEEK(p, t);
    if (t->kind != TK_IDENTIFIER) {
        return parser_error_1a(
            p,
            member ? "invalid operand for the member access operator"
                   : "invalid operand for the scope access operator",
            t->start, t->end, "expected an identifier");
    }
    esa->target.name = alloc_string_perm(p, t->str);
    if (!esa->target.name) return PE_FATAL;
    esa->target_srange = src_range_pack_lines(p->lx.tc, t->start, t->end);
    if (esa->target_srange == SRC_RANGE_INVALID) return PE_FATAL;
    *ex = (ast_node*)esa;
    lx_void(&p->lx);
    return PE_OK;
}
static inline parse_error
parse_binary_op(parser* p, operator_kind op, ast_node** ex, ast_node* lhs)
{

    if (op == OP_SCOPE_ACCESS || op == OP_MEMBER_ACCESS) {
        return parse_scope_access(p, ex, lhs, (op == OP_MEMBER_ACCESS));
    }
    token* t = lx_aquire(&p->lx);
    lx_void(&p->lx);

    expr_op_binary* ob = (expr_op_binary*)alloc_perm(p, sizeof(expr_op_binary));
    if (!ob) return PE_FATAL;
    if (ast_node_fill_srange(p, &ob->node, t->start, t->end)) return PE_FATAL;
    ast_node_init_with_op((ast_node*)ob, EXPR_OP_BINARY, op, &MODIFIERS_NONE);
    parse_error pe = parse_expression_of_prec(
        p, &ob->rhs, op_precedence[op] + is_left_associative(op));
    if (pe) {
        if (pe == PE_EOEX) {
            PEEK(p, t);
            src_range_large sr;
            src_range_unpack(ob->node.srange, &sr);
            parser_error_2a(
                p, "missing operand for infix operator", t->start, t->end,
                "reached end of expression", sr.start, sr.end,
                "missing operand for this operator");
            return PE_ERROR;
        }
        return pe;
    }
    ob->lhs = *ex;
    *ex = (ast_node*)ob;
    return PE_OK;
}
parse_error
parse_expression_of_prec_post_value(parser* p, ast_node** ex, ureg prec)
{
    token* t;
    parse_error pe;
    operator_kind op;
    // parse arbitrarily many postfix operators
    while (true) {
        PEEK(p, t);
        op = token_to_postfix_unary_op(p, t);
        if (op != OP_NOOP) {
            if (op_precedence[op] < prec) return PE_OK;
            pe = parse_postfix_unary_op(p, op, ex, *ex);
            if (pe) return pe;
        }
        else {
            op = token_to_binary_op(t);
            if (op == OP_NOOP) break;
            if (op_precedence[op] < prec) return PE_OK;
            pe = parse_binary_op(p, op, ex, *ex);
            if (pe) return pe;
        }
    }
    return PE_OK;
}
parse_error parse_expression_of_prec(parser* p, ast_node** ex, ureg prec)
{
    token* t;
    PEEK(p, t);
    *ex = NULL;
    parse_error pe;
    // parse one prefix op(recursive) or a plain value
    operator_kind op = token_to_prefix_unary_op(t);
    if (op != OP_NOOP) {
        pe = parse_prefix_unary_op(p, op, ex);
        if (pe) return pe;
    }
    else {
        pe = parse_value_expr(p, ex, prec);
        if (pe) return pe;
    }
    return parse_expression_of_prec_post_value(p, ex, prec);
}
parse_error parse_expr_in_parens(
    parser* p, ast_node* parent, ureg start, ureg end, ast_node** ex)
{
    token* t;
    PEEK(p, t);
    if (t->kind != TK_PAREN_OPEN) {
        parser_error_2a(
            p, "expected opening parenthesis", t->start, t->end,
            "required '(' here", start, end,
            get_context_msg(p, (ast_node*)parent));
        return PE_ERROR;
    }
    lx_void(&p->lx);
    parse_error pe = parse_expression(p, ex);
    if (pe == PE_EOEX) {
        PEEK(p, t);
        src_range_large srl;
        src_range_unpack(parent->srange, &srl);
        parser_error_2a(
            p, "expected expression", t->start, t->end, "expected expression",
            start, end, get_context_msg(p, (ast_node*)parent));
        return PE_ERROR;
    }
    if (pe) return pe;
    PEEK(p, t);
    if (t->kind != TK_PAREN_CLOSE) {
        src_range_large srl;
        src_range_unpack(parent->srange, &srl);
        parser_error_2a(
            p, "expected closing parenthesis", t->start, t->end,
            "expected ')' here", start, end,
            get_context_msg(p, (ast_node*)parent));
        return PE_ERROR;
    }
    lx_void(&p->lx);
    // ast_node_fill_srange(p, *ex, start, t->end);
    return pe;
}
parse_error parse_expression(parser* p, ast_node** ex)
{
    return parse_expression_of_prec(p, ex, OP_PREC_BASELINE);
}
void report_missing_semicolon(parser* p, ureg start, ureg end)
{
    token* t = lx_aquire(&p->lx);
    parser_error_2a(
        p, "missing semicolon ", t->start, t->end,
        "expected ';' to terminate the statement", start, end,
        "statement here");
}
parse_error handle_semicolon_after_statement(parser* p, ast_node* s)
{
    token* t;
    PEEK(p, t);
    if (t->kind != TK_SEMICOLON) {
        if (!ast_node_may_drop_semicolon(s)) {
            ureg start = src_range_get_start(s->srange);
            ureg end = src_range_get_end(s->srange);
            report_missing_semicolon(p, start, end);
            return PE_ERROR;
        }
    }
    else {
        // src_range_set_end(p->lx.tc, &s->srange, t->end);
        // if (s->srange == SRC_RANGE_INVALID) return PE_FATAL;
        lx_void(&p->lx);
    }
    return PE_OK;
}
static inline parse_error parse_delimited_module_frame(
    parser* p, module_frame* mf, token_kind delimiter_1, token_kind delimiter_2,
    bool* content)
{
    bool cntnt = false;
    // to allow deallocation in case of early failiure
    mf->body.elements = (ast_node**)NULL_PTR_PTR;
    mf->body.pprn = NULL;
    mf->body.symtab = NULL;
    if (push_bpd(p, (ast_node*)mf, &mf->body)) return PE_FATAL;
    void* requires_list_start = list_builder_start_blocklist(&p->lx.tc->listb);
    void** element_list_start = list_builder_start(&p->lx.tc->listb2);
    token* t;
    t = lx_peek(&p->lx);
    if (!t) {
        if (pop_bpd(p, PE_LX_ERROR)) return PE_FATAL;
        return PE_LX_ERROR;
    }
    parse_error pe = PE_OK;
    ast_node* target;

    while (t->kind != delimiter_1 && t->kind != delimiter_2) {
        pe = parse_statement(p, &target);
        bool stmt = true;
        if (pe == PE_NO_STMT) {
            stmt = false;
        }
        else if (pe) {
            break;
        }
        pe = handle_semicolon_after_statement(p, target);
        if (pe) break;
        if (stmt) {
            if (list_builder_add(&p->lx.tc->listb2, target)) {
                pe = PE_FATAL;
                break;
            }
            cntnt = true;
        }
        t = lx_peek(&p->lx);
        if (!t) {
            pe = PE_LX_ERROR;
            break;
        }
    }
    mf->body.elements = (ast_node**)list_builder_pop_list_zt(
        &p->lx.tc->listb2, element_list_start, &p->lx.tc->permmem);
    mf->requires = (file_require*)list_builder_pop_block_list_zt(
        &p->lx.tc->listb, requires_list_start, &p->lx.tc->permmem);
    if (mf->requires->fmh) cntnt = true;
    if (!mf->body.elements) {
        free_failed_ast_node_list_symtabs(
            &p->lx.tc->listb2, element_list_start, &mf->body.elements);
        pe = PE_FATAL;
    }
    parse_error pop_pe = pe;
    if (!pe && content && !cntnt) pop_pe = PE_NO_STMT;
    if (pop_bpd(p, pop_pe)) return PE_FATAL;
    if (!mf->requires) return PE_FATAL;
    if (pe) return pe;
    if (content) *content = cntnt;
    return PE_OK;
}
parse_error
parse_eof_delimited_module_frame(parser* p, module_frame* mf, bool* content)
{
    return parse_delimited_module_frame(p, mf, TK_EOF, TK_EOF, content);
}
parse_error parser_parse_file(parser* p, job_parse* j)
{
    lx_status s = lx_open_file(&p->lx, j->file);
    p->current_file = j->file;
    if (s) {
        if (s == LX_STATUS_FILE_UNAVAILABLE) {
            if (j->requiring_smap != NULL) {
                src_range_large srl;
                src_range_unpack(j->requiring_srange, &srl);
                error_log_report_annotated(
                    p->lx.tc->err_log, ES_TOKENIZER, false,
                    "required file is not available", j->requiring_smap,
                    srl.start, srl.end, "required here");
            }
            else {
                char* file_path = file_map_head_tmalloc_path(&j->file->head);
                if (!file_path) return PE_FATAL;
                char* msg = error_log_cat_strings_3(
                    p->lx.tc->err_log, "the requested file \"", file_path,
                    "\" is not available");
                tfree(file_path);
                error_log_report_general(
                    p->lx.tc->err_log, ES_TOKENIZER, false, msg);
            }
            if (src_file_done_parsing(j->file, p->lx.tc, true)) return PE_FATAL;
        }
        return PE_LX_ERROR;
    }
    p->current_module = p->lx.tc->t->mdg.root_node;
    p->file_root = &j->file->root;
    p->file_root->smap = p->lx.smap;
    ast_node_init((ast_node*)&j->file->root, MF_EXTEND);
    bool content = false;
    token* t;
    t = lx_peek(&(p)->lx);
    parse_error pe;
    if (!t) {
        pe = PE_LX_ERROR;
    }
    else {
        src_range_large srl;
        srl.start = t->start;
        pe = parse_eof_delimited_module_frame(p, &j->file->root, &content);
        if (pe) {
            free_body_symtabs(&j->file->root.body);
        }
        else {
            t = lx_peek(&(p)->lx);
            if (!t) {
                if (!pe) pe = PE_LX_ERROR;
            }
            else {
                srl.end = t->end;
                srl.smap = p->lx.smap;
                p->file_root->node.srange =
                    src_range_large_pack(p->lx.tc, &srl);
                if (p->file_root->node.srange == SRC_RANGE_INVALID)
                    pe = PE_FATAL;
            }
        }
    }
    lx_close_file(&p->lx);
    if (pe == PE_FATAL) return pe;
    int r = thread_context_preorder_job(p->lx.tc);
    if (content && !pe) {
        if (aseglist_add(
                &p->lx.tc->t->mdg.root_node->module_frames, &j->file->root))
            return RE_FATAL;
    }
    if (src_file_done_parsing(j->file, p->lx.tc, pe != PE_OK)) pe = PE_FATAL;
    if (r) return PE_FATAL;
    return pe;
}
parse_error init_paste_evaluation_parse(
    parser* p, expr_pp* epp, ast_node_kind kind, ast_body* parent_body,
    ast_body* parent_shared_body, paste_evaluation** eval)
{
    pasted_source* ps = epp->result_buffer.pasted_src;
    *ps->paste_data.last_next = NULL;
    pasted_str* str = ps->paste_data.first;
    ps->read_data.paste_str = str;
    ps->read_data.read_str = NULL;
    ps->read_data.read_pos = NULL;
    paste_evaluation* pe = alloc_perm(p, sizeof(paste_evaluation));
    if (!pe) return RE_FATAL;
    ps->source_pp_smap = ast_body_get_smap(parent_body);
    ps->source_pp_srange = epp->pp_expr->srange;
    pe->node.kind = kind;
    pe->node.flags = AST_NODE_FLAGS_DEFAULT;
    pe->body.parent = parent_body;
    pe->body.owning_node = (ast_node*)pe;
    pe->body.pprn = NULL;
    pe->body.symtab = NULL;
    pe->body.elements = (ast_node**)NULL_PTR_PTR; // in case we fail
    int r = lx_open_paste(&p->lx, ps);
    if (r) return PE_FATAL;
    src_map* paste_smap = p->lx.smap;
    p->current_file = src_map_get_file(paste_smap);
    if (r) return PE_LX_ERROR;
    if (p->lx.tc->t->verbosity_flags & VERBOSITY_FLAG_PASTES) {
        ureg lx_pos = ptrdiff(p->lx.file_buffer_head, p->lx.file_buffer_start);
        // TODO: make configurable
        char buffer[1025];
        ureg read_size;
        tput("parsing paste: '");
        src_map_seek_set(paste_smap, 0);
        do {
            src_map_read(paste_smap, sizeof(buffer) - 1, &read_size, buffer);
            buffer[read_size] = '\0';
            tput(buffer);
        } while (read_size == sizeof(buffer) - 1);
        tputs("'");
        tflush();
        src_map_seek_set(paste_smap, lx_pos);
    }
    pe->node.srange = src_range_pack(p->lx.tc, 0, 0, paste_smap);
    epp->result_buffer.paste_eval = pe;
    ast_node_set_pp_expr_contains_paste_eval((ast_node*)epp);
    *eval = pe;
    p->file_root = NULL;
    p->paste_parent_body = parent_body;
    p->paste_parent_shared_body = parent_shared_body;
    return PE_OK;
}
parse_error parser_parse_paste_expr(
    parser* p, expr_pp* epp, ast_body* parent_body,
    ast_body* parent_shared_body)
{
    paste_evaluation* eval;
    parse_error pe = init_paste_evaluation_parse(
        p, epp, EXPR_PASTE_EVALUATION, parent_body, parent_shared_body,
        (paste_evaluation**)&eval);
    if (pe) return pe;
    pe = push_bpd(p, (ast_node*)parent_body->owning_node, parent_body);
    if (pe) return pe;
    pe = parse_expression(p, &eval->expr);
    drop_bpd(p);
    if (pe) return pe;
    token* t = lx_peek(&p->lx);
    if (t->kind != TK_EOF) {
        src_range_large srl;
        src_range_unpack(eval->pasted_src->source_pp_srange, &srl);
        error_log_report_annotated_thrice(
            p->lx.tc->err_log, ES_PARSER, false, "invalid paste expression",
            p->lx.smap, t->start, t->end,
            "token follows after end of expression", p->lx.smap,
            src_range_get_start(eval->expr->srange),
            src_range_get_end(eval->expr->srange), "pasted expression",
            srl.smap, srl.start, srl.end, "pasted in here");
        return PE_ERROR;
    }
    lx_close_paste(&p->lx);
    return PE_OK;
}
parse_error parser_parse_paste_stmt(
    parser* p, expr_pp* epp, ast_body* parent_body,
    ast_body* parent_shared_body)
{
    paste_evaluation* eval;
    parse_error pe = init_paste_evaluation_parse(
        p, epp, STMT_PASTE_EVALUATION, parent_body, parent_shared_body,
        (paste_evaluation**)&eval);
    if (pe) return pe;
    p->paste_block = &eval->body;
    eval->expr = NULL;
    pe = parse_delimited_body(
        p, &eval->body, (ast_node*)eval, 0, NULL, 0, 1, TK_EOF);
    lx_close_paste(&p->lx);
    return pe;
}
static inline const char* access_modifier_string(access_modifier am)
{
    switch (am) {
        case AM_PRIVATE: return token_strings[TK_KW_PRIVATE];
        case AM_PROTECTED: return token_strings[TK_KW_PROTECTED];
        case AM_PUBLIC: return token_strings[TK_KW_PUBLIC];
        default: return NULL;
    }
}
static inline int
report_redundant_specifier(parser* p, const char* spec, ureg start, ureg end)
{
    char* msg = error_log_cat_strings_3(
        p->lx.tc->err_log, "redundant ", spec, " specifier");
    if (!msg) return ERR;
    parser_error_1a(p, "redundant access modifiers specified", start, end, msg);
    return OK;
}
static inline parse_error ast_flags_from_kw_set_access_mod(
    parser* p, modifier_status* mods, access_modifier am)
{
    access_modifier old_am = mods->data.access_mod;
    if (old_am != AM_NONE) {
        if (old_am == am) {
            report_redundant_specifier(
                p, access_modifier_string(am), mods->start, mods->end);
        }
        else {
            const char* msgstrs[5];
            msgstrs[0] = "'";
            msgstrs[1] = access_modifier_string(am);
            msgstrs[2] = "' conflicts with previous '";
            msgstrs[3] = access_modifier_string(old_am);
            msgstrs[4] = "'";
            char* msg = error_log_cat_strings(p->lx.tc->err_log, 5, msgstrs);
            if (!msg) return PE_FATAL;
            error_log_report_annotated(
                p->lx.tc->err_log, ES_PARSER, false,
                "conflicting access modifiers specified", p->lx.smap,
                mods->start, mods->end, msg);
        }
        return PE_ERROR;
    }
    mods->data.access_mod = am;
    return PE_OK;
}
parse_error parse_var_decl(parser* p, modifier_status* mods, ast_node** n)
{
    token* t = lx_aquire(&p->lx);
    ureg end = t->end;
    char* ident = alloc_string_perm(p, t->str);
    if (!ident) return PE_FATAL;
    lx_void(&p->lx);
    t = lx_aquire(&p->lx);
    ureg col_end = t->end;
    lx_void(&p->lx);
    parse_error pe;
    ast_node* type;
    ast_node* value;
    PEEK(p, t);
    if (t->kind == TK_EQUALS) {
        ureg eq_end = t->end;
        lx_void(&p->lx);
        type = NULL;
        pe = parse_expression(p, &value);
        if (pe == PE_EOEX) {
            PEEK(p, t);
            parser_error_2a(
                p, "invalid declaration syntax", t->start, t->end,
                "expected expression after '='", mods->start, eq_end,
                "begin of declaration");
            return PE_ERROR;
        }
        if (pe) return pe;
    }
    else {
        pe = parse_expression_of_prec(p, &type, op_precedence[OP_EQUAL] + 1);
        if (pe == PE_EOEX) {
            PEEK(p, t);
            parser_error_2a(
                p, "invalid declaration syntax", t->start, t->end,
                "expected type or '='", mods->start, col_end,
                "begin of declaration");
            return PE_ERROR;
        }
        if (pe) return pe;
        PEEK(p, t);
        if (t->kind == TK_EQUALS) {
            ureg eq_end = t->end;
            lx_void(&p->lx);
            pe = parse_expression(p, &value);
            if (pe == PE_EOEX) {
                parser_error_2a(
                    p, "unexpeted token in declaration", t->start, t->end,
                    "expected expression", mods->start, eq_end,
                    "in this declaration");
                return PE_ERROR;
            }
            if (pe) return pe;
            PEEK(p, t);
        }
        else {
            value = NULL;
        }
    }
    sym_var* v;
    if (value) {
        sym_var_initialized* vi = alloc_perm(p, sizeof(sym_var_initialized));
        if (!vi) return PE_FATAL;
        vi->initial_value = value;
        v = (sym_var*)vi;
        ast_node_init_with_mods((ast_node*)v, SYM_VAR_INITIALIZED, &mods->data);
    }
    else {
        v = alloc_perm(p, sizeof(sym_var));
        if (!v) return PE_FATAL;
        ast_node_init_with_mods((ast_node*)v, SYM_VAR, &mods->data);
    }
    v->pprn = NULL;
    v->type = type;
    v->osym.sym.name = ident;
    v->osym.visible_within_body = NULL; // TODO: parse this
    if (!v->osym.sym.name) return PE_FATAL;
    *n = (ast_node*)v;
    if (sym_fill_srange(p, (symbol*)v, mods->start, end)) return PE_FATAL;
    curr_scope_add_decls(p, ast_node_get_access_mod(*n), 1);
    return PE_OK;
}
void free_failed_param_block_list_symtabs(list_builder* lb, void** list_start)
{
    list_builder_rev_iter it;
    list_builder_rev_iter_init(&it, lb, list_start);
    while (true) {
        sym_param* p =
            (sym_param*)list_builder_rev_iter_prev(&it, sizeof(sym_param));
        if (!p) break;
        free_astn_symtabs((ast_node*)p);
    }
    list_builder_drop_list(lb, list_start);
}
parse_error parse_param_list(
    parser* p, ast_node* parent, sym_param** tgt, ureg* param_count,
    bool generic, ureg ctx_start, ureg ctx_end, char* msg)
{
    token* t;
    token_kind end_tok = generic ? TK_BRACKET_CLOSE : TK_PAREN_CLOSE;
    PEEK(p, t);
    if (t->kind == end_tok) {
        lx_void(&p->lx);
        *tgt = NULL; // kinda uneccessary, but helps with debugging
        *param_count = 0;
        return PE_OK;
    }
    void** param_list = list_builder_start_blocklist(&p->lx.tc->listb2);
    parse_error pe;
    do {
        sym_param param;
        pe = parse_param_decl(p, &param, ctx_start, ctx_end, generic, msg);
        if (pe) break;
        list_builder_add_block(&p->lx.tc->listb2, &param, sizeof(sym_param));
        t = lx_peek(&p->lx);
        if (!t) {
            pe = PE_LX_ERROR;
            break;
        }
        if (t->kind == TK_COMMA) {
            lx_void(&p->lx);
        }
        else if (t->kind != end_tok) {
            char* e1 = generic ? "invalid generic parameter list syntax"
                               : "invalid parameter list syntax";
            char* e2 = generic ? "expected ',' or ']' in generic parameter list"
                               : "expected ',' or ')' in parameter list";
            error_log_report_annotated_twice(
                p->lx.tc->err_log, ES_PARSER, false, e1, p->lx.smap, t->start,
                t->end, e2, p->lx.smap, ctx_start, ctx_end, msg);
            pe = PE_ERROR;
            break;
        }
    } while (t->kind != end_tok);
    if (pe) {
        free_failed_param_block_list_symtabs(&p->lx.tc->listb2, param_list);
        return pe;
    }
    lx_void(&p->lx);
    *tgt = (sym_param*)list_builder_pop_block_list(
        &p->lx.tc->listb2, param_list, &p->lx.tc->permmem, param_count, 0, 0);
    if (!*tgt) {
        free_failed_param_block_list_symtabs(&p->lx.tc->listb2, param_list);
        return PE_FATAL;
    }
    *param_count = *param_count / sizeof(sym_param);
    return PE_OK;
}
parse_error parse_func_decl(parser* p, modifier_status* mods, ast_node** n)
{
    lx_void(&p->lx);
    token* t;
    parse_error pe;
    PEEK(p, t);
    if (t->kind != TK_IDENTIFIER) {
        parser_error_2a(
            p, "invalid function declaration syntax", t->start, t->end,
            "expected function identifier", mods->start, t->end,
            "in this function declaration");
        return PE_ERROR;
    }
    ureg decl_end = t->end;
    char* name = alloc_string_perm(p, t->str);
    if (!name) return PE_FATAL;
    lx_void(&p->lx);
    PEEK(p, t);
    sc_func_base* fnb;
    sc_func_generic* fng = NULL;
    sc_func* fn = NULL;
    if (t->kind == TK_BRACKET_OPEN) {
        fng = alloc_perm(p, sizeof(sc_func_generic));
        if (!fng) return PE_FATAL;
        fnb = (sc_func_base*)fng;
        lx_void(&p->lx);
        pe = parse_param_list(
            p, (ast_node*)fng, &fng->generic_params, &fng->generic_param_count,
            true, mods->start, decl_end, "in this function declaration");
        if (pe) return pe;
        PEEK(p, t);
    }
    else {
        fn = alloc_perm(p, sizeof(sc_func));
        if (!fn) return PE_FATAL;
        fnb = (sc_func_base*)fn;
    }
    fnb->sc.osym.sym.name = name;
    fnb->sc.osym.visible_within_body = NULL; // TODO
    ast_node_init_with_mods(
        (ast_node*)fnb, fng ? SC_FUNC_GENERIC : SC_FUNC, &mods->data);
    pe = sym_fill_srange(p, (symbol*)fnb, mods->start, decl_end);
    if (pe) return pe;
    if (t->kind != TK_PAREN_OPEN) {
        parser_error_2a(
            p, "invalid function declaration syntax", t->start, t->end,
            "expected '(' to start parameter list", mods->start, decl_end,
            "in this function declaration");
        return PE_ERROR;
    }
    lx_void(&p->lx);
    pe = parse_param_list(
        p, (ast_node*)fnb, &fnb->params, &fnb->param_count, false, mods->start,
        decl_end, "in this function declaration");
    if (pe) return pe;
    *n = (ast_node*)fnb;
    curr_scope_add_decls(p, ast_node_get_access_mod(*n), 1);
    PEEK(p, t);
    if (t->kind == TK_ARROW) {
        lx_void(&p->lx);
        ast_node* ret_type;
        assert(p->disable_macro_body_call == false);
        p->disable_macro_body_call = true;
        pe = parse_expression(p, &ret_type);
        p->disable_macro_body_call = false;
        if (pe == PE_EOEX) {
            parser_error_2a(
                p, "unexpected end of expression", t->start, t->end,
                "expected function return type", mods->start, decl_end,
                "in this function declaration");
            return PE_ERROR;
        }
        if (pe) return pe;
        fnb->return_type = ret_type;
    }
    else {
        fnb->return_type = NULL;
    }
    fnb->sc.osym.visible_within_body = NULL; // TODO: parse this
    PEEK(p, t);
    if (fn && t->kind == TK_SEMICOLON) {
        fn->fnb.sc.body.elements = (ast_node**)NULL_PTR_PTR;
        fn->fnb.sc.body.srange = SRC_RANGE_INVALID;
        fn->fnb.sc.body.symtab = NULL;
        fnb->sc.body.pprn = NULL;
        if (push_bpd(p, (ast_node*)fn, &fn->fnb.sc.body)) return PE_FATAL;
        curr_scope_add_decls(p, AM_LOCAL, fn->fnb.param_count);
        if (pop_bpd(p, PE_OK)) return PE_FATAL;
        return PE_OK;
    }
    return parse_scope_body(
        p, (scope*)fnb,
        (fng ? fng->generic_param_count : 0) + fnb->param_count);
}
parse_error parse_macro_decl(parser* p, modifier_status* mods, ast_node** n)
{
    lx_void(&p->lx);
    token* t;
    parse_error pe;
    PEEK(p, t);
    if (t->kind != TK_IDENTIFIER) {
        parser_error_2a(
            p, "invalid macro declaration syntax", t->start, t->end,
            "expected macro identifier", mods->start, t->end,
            "in this macro declaration");
        return PE_ERROR;
    }
    ureg decl_end = t->end;
    char* name = alloc_string_perm(p, t->str);
    if (!name) return PE_FATAL;
    lx_void(&p->lx);
    sc_macro* m = alloc_perm(p, sizeof(sc_macro));
    if (!m) return PE_FATAL;
    m->sc.osym.sym.name = name;
    m->sc.osym.visible_within_body = NULL; // TODO
    ast_node_init_with_mods((ast_node*)m, SC_MACRO, &mods->data);
    pe = sym_fill_srange(p, (symbol*)m, mods->start, decl_end);
    if (pe) return pe;
    PEEK(p, t);
    if (t->kind != TK_PAREN_OPEN) {
        parser_error_2a(
            p, "invalid macro declaration syntax", t->start, t->end,
            "expected '(' to start parameter list", mods->start, decl_end,
            "in this macro declaration");
        return PE_ERROR;
    }
    lx_void(&p->lx);
    pe = parse_param_list(
        p, (ast_node*)m, &m->params, &m->param_count, false, mods->start,
        decl_end, "in this macro declaration");
    if (pe) return pe;
    *n = (ast_node*)m;
    curr_scope_add_decls(p, ast_node_get_access_mod(*n), 1);
    PEEK(p, t);
    if (t->kind == TK_SEMICOLON) {
        m->sc.body.elements = (ast_node**)NULL_PTR_PTR;
        m->sc.body.srange = SRC_RANGE_INVALID;
        m->sc.body.symtab = NULL;
        if (push_bpd(p, (ast_node*)m, &m->sc.body)) return PE_FATAL;
        curr_scope_add_decls(p, AM_LOCAL, m->param_count);
        if (pop_bpd(p, PE_OK)) return PE_FATAL;
        return PE_OK;
    }
    // TODO: add multi macros
    m->next = NULL;
    return parse_scope_body(p, (scope*)m, m->param_count);
}
parse_error
parse_trait_impl_decl(parser* p, modifier_status* mods, ast_node** n)
{
    token* t;
    t = lx_aquire(&p->lx);
    lx_void(&p->lx);
    ureg impl_head_end = t->end;
    parse_error pe;
    PEEK(p, t);
    trait_impl_base* tib;
    trait_impl_generic* tig = NULL;
    trait_impl* ti;
    if (t->kind == TK_BRACKET_OPEN) {
        tig = alloc_perm(p, sizeof(trait_impl_generic));
        if (!tig) return PE_FATAL;
        tib = (trait_impl_base*)tig;
        ast_node_init_with_mods(
            (ast_node*)tib, TRAIT_IMPL_GENERIC, &mods->data);
        lx_void(&p->lx);
        pe = parse_param_list(
            p, (ast_node*)tib, &tig->generic_params, &tig->generic_param_count,
            true, mods->start, impl_head_end,
            get_context_msg(p, (ast_node*)tib));
        if (pe) return pe;
        PEEK(p, t);
        curr_scope_get_appropriate_eoc(p, mods->data.access_mod)
            ->generic_impl_count++;
    }
    else {
        ti = alloc_perm(p, sizeof(trait_impl));
        if (!ti) return PE_FATAL;
        tib = (trait_impl_base*)ti;
        ast_node_init_with_mods((ast_node*)tib, TRAIT_IMPL, &mods->data);
        int err = type_map_init(&ti->type_derivs.tm);
        if (err) return PE_FATAL;
        curr_scope_get_appropriate_eoc(p, mods->data.access_mod)->impl_count++;
    }
    ast_node* first_expr;
    assert(p->disable_macro_body_call == false);
    p->disable_macro_body_call = true;
    pe = parse_expression(p, &first_expr);
    p->disable_macro_body_call = false;
    PEEK(p, t);
    if (pe == PE_EOEX) {
        parser_error_2a(
            p, "invalid trait impl syntax", t->start, t->end,
            "expected trait or type name after 'impl'", mods->start, t->end,
            NULL);
        return PE_ERROR;
    }
    ureg decl_end;
    if (is_cond_kw(t, COND_KW_FOR)) {
        lx_void(&p->lx);
        tib->impl_of = first_expr;
        p->disable_macro_body_call = true;
        pe = parse_expression(p, &tib->impl_for);
        p->disable_macro_body_call = false;
        if (pe == PE_EOEX) {
            PEEK(p, t);
            parser_error_2a(
                p, "invalid trait impl syntax", t->start, t->end,
                "expected type name after 'for'", mods->start, t->end, NULL);
            return PE_ERROR;
        }
        ast_node_get_bounds(tib->impl_of, NULL, &decl_end);
    }
    else {
        tib->impl_for = first_expr;
        tib->impl_of = NULL;
        ast_node_get_bounds(tib->impl_for, NULL, &decl_end);
    }
    pe = ast_node_fill_srange(p, (ast_node*)tib, mods->start, decl_end);
    if (pe) return pe;

    *n = (ast_node*)tib;
    curr_scope_add_decls(p, ast_node_get_access_mod(*n), 1);
    PEEK(p, t);
    if (t->kind != TK_BRACE_OPEN) {
        parser_error_2a(
            p, "expected trait impl body", t->start, t->end,
            "expected '{' to begin trait impl body",
            src_range_get_start(tib->node.srange),
            src_range_get_end(tib->node.srange),
            get_context_msg(p, (ast_node*)tib));
        return PE_ERROR;
    }
    return parse_brace_delimited_body(
        p, &tib->body, (ast_node*)tib, tig ? tig->generic_param_count : 0);
}
parse_error parse_struct_decl(parser* p, modifier_status* mods, ast_node** n)
{
    lx_void(&p->lx);
    token* t;
    parse_error pe;
    PEEK(p, t);
    if (t->kind != TK_IDENTIFIER) {
        parser_error_2a(
            p, "invalid struct declaration syntax", t->start, t->end,
            "expected struct identifier", mods->start, t->end,
            "in this struct declaration");
        return PE_ERROR;
    }
    ureg decl_end = t->end;
    char* name = alloc_string_perm(p, t->str);
    if (!name) return PE_FATAL;
    lx_void(&p->lx);
    PEEK(p, t);
    open_symbol* s;
    sc_struct_generic* sg = NULL;
    sc_struct* sp;
    if (t->kind == TK_BRACKET_OPEN) {
        sg = alloc_perm(p, sizeof(sc_struct_generic));
        if (!sg) return PE_FATAL;
        sg->instances = NULL;
        // TODO:  sg->pprn = NULL;
        s = (open_symbol*)sg;
        lx_void(&p->lx);
        pe = parse_param_list(
            p, (ast_node*)s, &sg->generic_params, &sg->generic_param_count,
            true, mods->start, decl_end, "in this struct declaration");
        if (pe) return pe;
        PEEK(p, t);
    }
    else {
        sp = alloc_perm(p, sizeof(sc_struct));
        if (!sp) return PE_FATAL;
        sp->sb.sc.body.pprn = NULL;
        s = (open_symbol*)sp;
        int err = type_map_init(&sp->type_derivs.tm);
        if (err) return PE_FATAL;
    }
    ast_node_init_with_mods(
        (ast_node*)s, sg ? SC_STRUCT_GENERIC : SC_STRUCT, &mods->data);
    s->sym.name = name;
    s->visible_within_body = NULL; // TODO: parse this
    ((sc_struct_base*)s)->extends_spec = NULL;
    pe = sym_fill_srange(p, &s->sym, mods->start, decl_end);
    if (pe) return pe;

    *n = (ast_node*)s;
    curr_scope_add_decls(p, ast_node_get_access_mod(*n), 1);
    return parse_scope_body(p, (scope*)s, sg ? sg->generic_param_count : 0);
}
parse_error check_if_first_stmt(
    parser* p, ast_node** tgt, ureg start, ureg end, bool extend)
{
    // DEBUG
    return PE_OK;
    // TODO: use extend bool to be more precise than "scope" in the err msg
    scope* curr_scope = (scope*)get_bpd(p)->node;
    if (curr_scope != (scope*)p->file_root) {
        parser_error_1a_pc(
            p, "block free scope statement not allowed here", start, end,
            "this statement type is only allowed at file scope");
        return PE_ERROR;
    }
    // set own target to null so the children list will be null terminated
    *tgt = NULL;
    ast_node* last_culprit = NULL;
    for (ast_node** n = curr_scope->body.elements; *n; n++) {
        if (ast_elem_is_any_import((ast_elem*)n)) {
            last_culprit = *n;
        }
    }
    if (last_culprit != NULL) {
        ureg hs = src_range_get_start(last_culprit->srange);
        ureg he = src_range_get_end(last_culprit->srange);
        parser_error_2a_pc(
            p, "non leading scope statement", start, end, "", hs, he,
            "preceeded by this statement");
        return PE_ERROR;
    }
    return PE_OK;
}
parse_error parse_module_frame_decl(
    parser* p, modifier_status* mods, bool extend, ast_node** n)
{
    lx_void(&p->lx);
    token *t, *t2;
    parse_error pe;
    PEEK(p, t);
    if (t->kind != TK_IDENTIFIER) {
        parser_error_2a(
            p, "invalid module declaration syntax", t->start, t->end,
            "expected module identifier", mods->start, t->end,
            "in this module declaration");
        return PE_ERROR;
    }
    ureg decl_end = t->end;

    t2 = lx_peek_2nd(&p->lx);
    if (!t2) return PE_LX_ERROR;
    module_frame* md;
    module_frame_generic* mod_gen = NULL;
    if (t2->kind == TK_BRACKET_OPEN) {
        mod_gen = alloc_perm(p, sizeof(module_frame_generic));
        md = (module_frame*)mod_gen;
        if (!md) return PE_FATAL;
        lx_void_n(&p->lx, 2);
        pe = parse_param_list(
            p, (ast_node*)md, &mod_gen->generic_params,
            &mod_gen->generic_param_count, true, mods->start, decl_end,
            "in this module frame declaration");
        if (pe) return pe;
        PEEK(p, t);
    }
    else {
        md = alloc_perm(p, sizeof(module_frame));
        if (!md) return PE_FATAL;
        lx_void(&p->lx);
    }
    ast_node_kind kind;
    if (extend) {
        kind = mod_gen ? MF_EXTEND_GENERIC : MF_EXTEND;
    }
    else {
        kind = mod_gen ? MF_MODULE_GENERIC : MF_MODULE;
    }
    ast_node_init_with_mods((ast_node*)md, kind, &mods->data);
    md->node.srange =
        src_range_pack(p->lx.tc, mods->start, decl_end, p->lx.smap);
    md->smap = p->lx.smap;
    if (md->node.srange == SRC_RANGE_INVALID) return PE_FATAL;
    mdg_node* mdgn = mdg_found_node(
        p->lx.tc, p->current_module, t->str, md, p->lx.smap, md->node.srange);
    if (mdgn == NULL) return PE_FATAL;
    PEEK(p, t);
    mdg_node* parent = p->current_module;
    p->current_module = mdgn;
    bool single_file_module = false;
    if (t->kind == TK_SEMICOLON) {
        pe = check_if_first_stmt(p, n, mods->start, t->end, false);
        if (!pe) {
            lx_consume(&p->lx);
            single_file_module = true;
            pe = parse_delimited_module_frame(
                p, md, TK_EOF, TK_BRACE_CLOSE, NULL);
        }
    }
    else {
        pe = parse_module_frame_body(
            p, md, mdgn, mod_gen ? mod_gen->generic_param_count : 0);
    }
    p->current_module = parent;
    if (pe) return pe;

    *n = (ast_node*)md;
    // TODO: add a dummy symbol with the module name to avoid redeclaration
    // curr_scope_add_decls(p, ast_node_get_access_mod(flags), 1);
    if (single_file_module) {
        int r = thread_context_preorder_job(p->lx.tc);
        if (r) return RE_FATAL;
    }
    int r = mdg_node_add_frame(mdgn, (module_frame*)md, p->lx.tc);
    if (r) return RE_FATAL;
    ureg up = atomic_ureg_load(&mdgn->unparsed_files);
    if (up == 0 && !extend) {
        r = mdg_node_parsed(&p->lx.tc->t->mdg, mdgn, p->lx.tc);
    }
    if (r) return RE_FATAL;
    return PE_NO_STMT; // consider PE_NO_STMT
}
parse_error parse_trait_decl(parser* p, modifier_status* mods, ast_node** n)
{
    lx_void(&p->lx);
    token* t;
    parse_error pe;
    PEEK(p, t);
    if (t->kind != TK_IDENTIFIER) {
        parser_error_2a(
            p, "invalid trait declaration syntax", t->start, t->end,
            "expected trait identifier", mods->start, t->end,
            "in this trait declaration");
        return PE_ERROR;
    }
    ureg decl_end = t->end;
    char* name = alloc_string_perm(p, t->str);
    if (!name) return PE_FATAL;
    lx_void(&p->lx);
    PEEK(p, t);
    scope* tr;
    sc_trait_generic* tg = NULL;
    if (t->kind == TK_BRACKET_OPEN) {
        tg = alloc_perm(p, sizeof(sc_trait_generic));
        if (!tg) return PE_FATAL;
        tr = (scope*)tg;
        lx_void(&p->lx);
        pe = parse_param_list(
            p, (ast_node*)tr, &tg->generic_params, &tg->generic_param_count,
            true, mods->start, decl_end, "in this trait declaration");
        if (pe) return pe;
        PEEK(p, t);
    }
    else {
        tr = alloc_perm(p, sizeof(sc_trait));
        if (!tr) return PE_FATAL;
    }
    tr->osym.sym.name = name;
    tr->osym.visible_within_body = NULL; // TODO
    ast_node_init_with_mods(
        (ast_node*)tr, tg ? SC_TRAIT_GENERIC : SC_TRAIT, &mods->data);
    pe = sym_fill_srange(p, (symbol*)tr, mods->start, decl_end);
    if (pe) return pe;
    *n = (ast_node*)tr;
    curr_scope_add_decls(p, ast_node_get_access_mod(*n), 1);
    return parse_scope_body(p, tr, tg ? tg->generic_param_count : 0);
}
bool ast_elem_supports_exprs(ast_elem* n)
{
    return !ast_elem_is_module_frame(n) && !ast_elem_is_struct_base(n);
}
bool curr_parent_supports_exprs(parser* p)
{
    return ast_elem_supports_exprs((ast_elem*)get_bpd(p)->node);
}
bool body_customizes_exprs(ast_node_kind pt)
{
    return pt == EXPR_ARRAY;
}
static inline parse_error parse_compound_assignment_after_equals(
    parser* p, ureg t_start, ureg t_end, ast_node** elements, ureg elem_count,
    ast_node** tgt, bool had_colon)
{
    stmt_compound_assignment* ca =
        alloc_perm(p, sizeof(stmt_compound_assignment));
    if (!ca) return PE_FATAL;
    ca->elements = elements;
    ca->elem_count = elem_count;
    ast_node_init((ast_node*)ca, STMT_COMPOUND_ASSIGN);
    if (had_colon) ast_node_set_compound_decl(&ca->node);
    parse_error pe = parse_expression(p, &ca->value);
    token* t;
    if (pe == PE_EOEX) {
        PEEK(p, t);
        parser_error_2a(
            p, "unexpected token", t->start, t->end, "expected expression",
            t_start, t_end, "in this compound assignment statement");
    }
    if (pe) return pe;
    ast_node_fill_srange(
        p, (ast_node*)ca, t_start, src_range_get_end(ca->value->srange));
    *tgt = (ast_node*)ca;
    return PE_OK;
}
parse_error parse_expr_stmt(parser* p, ast_node** tgt)
{
    ast_node* ex;
    parse_error pe;
    token* t;
    PEEK(p, t);
    ureg t_start = t->start;
    if (t->kind == TK_PAREN_OPEN) {
        ureg t_end;
        ureg decl_count = 0;
        ureg ident_count = 0;
        ast_node** elems = NULL;
        ureg elem_count;
        pe = parse_paren_group_or_tuple_or_compound_decl(
            p, t, &ex, &elems, &elem_count, &t_end, &decl_count, &ident_count);
        if (pe) return pe;
        if (elems) {
            PEEK(p, t);
            if (t->kind == TK_COLON) {
                decl_count += ident_count;
                t = lx_peek_2nd(&p->lx);
                if (!t) return PE_LX_ERROR;
                if (t->kind == TK_EQUALS) {
                    lx_void_n(&p->lx, 2);
                    curr_scope_add_decls(p, AM_LOCAL, decl_count);
                    return parse_compound_assignment_after_equals(
                        p, t_start, t->end, elems, elem_count, tgt, true);
                }
            }
            if (t->kind == TK_EQUALS) {
                lx_void(&p->lx);
                turn_ident_nodes_to_exprs(elems, elem_count);
                curr_scope_add_decls(p, AM_LOCAL, decl_count);
                return parse_compound_assignment_after_equals(
                    p, t_start, t->end, elems, elem_count, tgt, false);
            }
            if (decl_count != 0) {
                parser_error_2a(
                    p, "unexpected token", t->start, t->end,
                    "expected '=' or ':=' to parse compound assignment",
                    t_start, t_end,
                    "after this compound which contains declarations");
            }
            else {
                turn_ident_nodes_to_exprs(elems, elem_count);
                expr_tuple* tp = alloc_perm(p, sizeof(expr_tuple));
                if (!tp) return PE_FATAL;
                ast_node_init((ast_node*)tp, EXPR_TUPLE);
                if (ast_node_fill_srange(p, (ast_node*)tp, t_start, t->end))
                    return PE_FATAL;
                tp->elements = elems;
                ex = (ast_node*)tp;
            }
        }
        // EMSG: if there is an invalid token following, this will lead to a
        // "missing semicolon for expression" error down the line which is
        // not
        // ideal
        pe = parse_expression_of_prec_post_value(p, &ex, OP_PREC_BASELINE);
    }
    else {
        operator_kind op = token_to_prefix_unary_op(t);
        if (op != OP_NOOP) {
            pe = parse_prefix_unary_op(p, op, &ex);
            if (pe) return pe;
            pe = parse_expression_of_prec_post_value(p, &ex, OP_PREC_BASELINE);
        }
        else {
            pe = parse_value_expr(p, &ex, OP_PREC_BASELINE);
            if (pe) return pe;
            if (!ast_node_may_drop_semicolon(ex)) {
                pe = parse_expression_of_prec_post_value(
                    p, &ex, OP_PREC_BASELINE);
            }
        }
    }
    if (pe == PE_EOEX) {
        PEEK(p, t);
        parser_error_1a_pc(
            p, "unexpected token in expression statement", t->start, t->end,
            "");
        return PE_ERROR;
    }
    *tgt = ex;
    return pe;
}
parse_error parse_import_with_parent(
    parser* p, modifier_status* mods, ureg kw_end, mdg_node* parent,
    ureg* decl_cnt, ast_node** tgt, bool child);
parse_error parse_use(parser* p, modifier_status* mods, ast_node** tgt)
{
    token* t = lx_aquire(&p->lx);
    ureg end = t->end;
    lx_void(&p->lx);
    PEEK(p, t);
    if (t->kind == TK_IDENTIFIER) {
        token* t2 = lx_peek_2nd(&p->lx);
        if (!t2) return PE_LX_ERROR;
        if (t2->kind == TK_EQUALS) {
            sym_named_use* nu = alloc_perm(p, sizeof(sym_named_use));
            if (!nu) return PE_FATAL;
            ast_node_init_with_mods((ast_node*)nu, SYM_NAMED_USE, &mods->data);
            nu->osym.sym.name = alloc_string_perm(p, t->str);
            nu->osym.visible_within_body = NULL; // TODO
            if (!nu->osym.sym.name) return PE_FATAL;
            lx_void_n(&p->lx, 2);
            parse_error pe = parse_expression(p, &nu->target);
            if (pe == PE_EOEX) {
                parser_error_2a(
                    p, "unexpected token", t->start, t->end,
                    "expected expression", mods->start, end,
                    "in this named using statement");
                return PE_ERROR;
            }
            if (pe) return pe;
            if (ast_node_fill_srange(
                    p, (ast_node*)nu, mods->start,
                    src_range_get_end(nu->target->srange)))
                return PE_FATAL;
            *tgt = (ast_node*)nu;
            curr_scope_add_decls(p, mods->data.access_mod, 1);
            return PE_OK;
        }
    }
    stmt_use* u = alloc_perm(p, sizeof(stmt_use));
    if (!u) return PE_FATAL;
    ast_node_init_with_mods((ast_node*)u, STMT_USE, &mods->data);
    parse_error pe = parse_expression(p, &u->target);
    if (pe == PE_EOEX) {
        parser_error_2a(
            p, "unexpected token", t->start, t->end, "expected expression",
            mods->start, end, "in this using statement");
        return PE_ERROR;
    }
    if (ast_node_fill_srange(
            p, (ast_node*)u, mods->start, src_range_get_end(u->target->srange)))
        return PE_FATAL;
    *tgt = (ast_node*)u;
    curr_scope_add_uses(p, mods->data.access_mod, 1);
    return pe;
}
parse_error parse_braced_imports(
    parser* p, modifier_status* mods, import_group_data* ig_data, ureg kw_end,
    mdg_node* parent, ureg* end, ureg* decl_cnt)
{
    lx_void(&p->lx);
    while (true) {
        ast_node* tgt;
        parse_error pe = parse_import_with_parent(
            p, mods, kw_end, parent, decl_cnt, &tgt, true);
        if (pe) return pe;
        list_append(&ig_data->children_ordered, &p->lx.tc->permmem, tgt);
        token* t;
        PEEK(p, t);
        if (t->kind == TK_COMMA || t->kind == TK_BRACE_CLOSE) {
            if (t->kind == TK_COMMA) {
                lx_void(&p->lx);
            }
            if (t->kind == TK_BRACE_CLOSE) {
                *end = t->end;
                lx_void(&p->lx);
                return PE_OK;
            }
        }
        else {
            return parser_error_2a(
                p, "invalid import syntax", t->start, t->end, "expected , or }",
                mods->start, kw_end, "in this import statement");
        }
    }
}
parse_error parse_symbol_imports(
    parser* p, ast_node* import_group, import_group_data* ig_data,
    modifier_status* mods, ureg kw_end, ureg* end, ureg* decl_cnt)
{
    lx_void(&p->lx);
    token* t;
    PEEK(p, t);
    ureg symstart = 0;
    ureg symend;
    char* id1_str;
    char* symname;
    while (true) {
        if (t->kind != TK_IDENTIFIER) {
            return parser_error_2a(
                p, "invalid import syntax", t->start, t->end,
                symstart ? "expected identifier or )" : "expected identifier",
                mods->start, kw_end, "in this import statement");
        }
        symstart = t->start;
        symend = t->end;
        id1_str = alloc_string_perm(p, t->str);
        if (!id1_str) return PE_FATAL;
        lx_void(&p->lx);
        PEEK(p, t);
        if (t->kind == TK_EQUALS) {
            lx_void(&p->lx);
            PEEK(p, t);
            if (t->kind != TK_IDENTIFIER) {
                return parser_error_2a(
                    p, "invalid import syntax", t->start, t->end,
                    "expected symbol identifier", mods->start, kw_end,
                    "in this import statement");
            }
            symname = alloc_string_perm(p, t->str);
            if (!symname) return PE_FATAL;
            symend = t->end;
            lx_void(&p->lx);
            PEEK(p, t);
        }
        else {
            symname = id1_str;
        }
        if (t->kind == TK_COMMA || t->kind == TK_PAREN_CLOSE) {
            if (t->kind == TK_COMMA) {
                lx_void(&p->lx);
                PEEK(p, t);
            }
            sym_import_symbol* im = alloc_perm(p, sizeof(sym_import_symbol));
            if (!im) return PE_FATAL;
            ast_node_init_with_mods(
                (ast_node*)im, SYM_IMPORT_SYMBOL, &mods->data);
            ast_node_fill_srange(p, (ast_node*)im, symstart, symend);
            im->osym.sym.name = id1_str;
            im->osym.visible_within_body = NULL; // TODO
            im->target.name = symname;
            im->import_group = import_group;
            if (list_append(
                    &ig_data->children_ordered, &p->lx.tc->permmem, im)) {
                return PE_FATAL;
            }
            *decl_cnt = *decl_cnt + 1;
            // fallthrough to allow trailing comma
            if (t->kind == TK_PAREN_CLOSE) {
                *end = t->end;
                lx_void(&p->lx);
                return PE_OK;
            }
        }
        else {
            return parser_error_2a(
                p, "invalid import syntax", t->start, t->end, "expected , or )",
                mods->start, kw_end, "in this import statement");
        }
    }
}
parse_error parse_import_with_parent(
    parser* p, modifier_status* mods, ureg kw_end, mdg_node* parent,
    ureg* decl_cnt, ast_node** tgt, bool child)
{
    mdg_node* relative_to = parent;
    parse_error pe;
    token *t, *t2;
    PEEK(p, t);
    ureg istart = child ? t->start : mods->start;
    ureg end;
    PEEK_SND(p, t2);
    char* name = NULL;
    if (t->kind == TK_IDENTIFIER && t2->kind == TK_EQUALS) {
        name = alloc_string_perm(p, t->str);
        if (!name) return PE_FATAL;
        lx_void_n(&p->lx, 2);
        PEEK(p, t);
    }
    // TODO: maybe provide a special message in case child is true
    if (t->kind == TK_KW_SELF && !child) {
        parent = p->current_module;
        mods->data.relative_import_mod = true;
        if (t2->kind == TK_DOUBLE_COLON) {
            lx_void_n(&p->lx, 2);
            PEEK(p, t);
        }
        else {
            return parser_error_2a(
                p, "invalid import syntax", t->start, t->end,
                "expected ::", mods->start, kw_end, "in this import statement");
        }
    }
    bool has_ident = false;
    while (t->kind == TK_IDENTIFIER) {
        has_ident = true;
        parent = mdg_get_node(
            &p->lx.tc->t->mdg, parent, t->str, MS_UNFOUND_UNNEEDED);
        // if the node doesn't exist it gets created here,
        // we will find out (and report) once all required files are parsed
        // if it is actually missing.
        if (!parent) return PE_FATAL;
        end = t->end;
        lx_void(&p->lx);
        PEEK(p, t);
        if (t->kind == TK_DOUBLE_COLON) {
            lx_void(&p->lx);
            PEEK(p, t);
        }
        else {
            sym_import_module* im = alloc_perm(p, sizeof(sym_import_module));
            if (!im) return PE_FATAL;
            im->im_data.pprn = NULL;
            ast_node_init_with_mods(
                (ast_node*)im, SYM_IMPORT_MODULE, &mods->data);
            ast_node_fill_srange(p, (ast_node*)im, istart, end);
            im->im_data.imported_module = parent;
            im->im_data.importing_module = p->current_module;
            im->osym.visible_within_body = NULL; // TODO
            if (name) {
                im->osym.sym.name = name;
                curr_scope_add_decls(p, mods->data.access_mod, 1);
            }
            else {
                im->osym.sym.name = parent->name;
            }
            if (mdg_node_add_dependency(p->current_module, parent, p->lx.tc)) {
                return PE_FATAL;
            }
            *tgt = (ast_node*)im;
            *decl_cnt = *decl_cnt + 1;
            return PE_OK;
        }
    }
    ureg ndecl_cnt = 0;
    if (name) {
        *decl_cnt = *decl_cnt + 1;
        decl_cnt = &ndecl_cnt;
    }
    import_group_data* ig_data;
    if (t->kind == TK_PAREN_OPEN) {
        import_module_data* im_data;
        if (name) {
            sym_named_sym_import_group* nsig =
                alloc_perm(p, sizeof(sym_named_sym_import_group));
            if (!nsig) return PE_FATAL;
            nsig->osym.sym.name = name;
            nsig->osym.visible_within_body = NULL; // TODO
            ast_node_init_with_mods(
                (ast_node*)nsig, SYM_NAMED_SYM_IMPORT_GROUP, &mods->data);
            *tgt = (ast_node*)nsig;
            ig_data = &nsig->ig_data;
            im_data = &nsig->im_data;
        }
        else {
            astn_anonymous_sym_import_group* asig =
                alloc_perm(p, sizeof(astn_anonymous_sym_import_group));
            if (!asig) return PE_FATAL;
            ast_node_init_with_mods(
                (ast_node*)asig, ASTN_ANONYMOUS_SYM_IMPORT_GROUP, &mods->data);
            *tgt = (ast_node*)asig;
            ig_data = &asig->ig_data;
            im_data = &asig->im_data;
        }
        if (mdg_node_add_dependency(p->current_module, parent, p->lx.tc)) {
            return PE_FATAL;
        }
        if (list_init(&ig_data->children_ordered)) return PE_FATAL;
        mods->data.relative_import_mod = false;
        pe = parse_symbol_imports(
            p, *tgt, ig_data, mods, kw_end, &end, decl_cnt);
        if (pe) return pe;
        im_data->imported_module = parent;
        im_data->importing_module = p->current_module;
        im_data->pprn = NULL;
    }
    else if (t->kind == TK_BRACE_OPEN) {
        if (name) {
            sym_named_mod_import_group* nmig =
                alloc_perm(p, sizeof(sym_named_mod_import_group));
            if (!nmig) return PE_FATAL;
            ast_node_init_with_mods(
                (ast_node*)nmig, SYM_NAMED_MOD_IMPORT_GROUP, &mods->data);
            nmig->osym.sym.name = name;
            nmig->osym.visible_within_body = NULL; // TODO
            *tgt = (ast_node*)nmig;
            ig_data = &nmig->ig_data;
        }
        else {
            astn_anonymous_mod_import_group* amig =
                alloc_perm(p, sizeof(astn_anonymous_mod_import_group));
            if (!amig) return PE_FATAL;
            ast_node_init_with_mods(
                (ast_node*)amig, ASTN_ANONYMOUS_MOD_IMPORT_GROUP, &mods->data);
            *tgt = (ast_node*)amig;
            ig_data = &amig->ig_data;
        }
        if (list_init(&ig_data->children_ordered)) return PE_FATAL;
        mods->data.relative_import_mod = false;
        pe = parse_braced_imports(
            p, mods, ig_data, kw_end, parent, &end, decl_cnt);
        if (pe) return pe;
    }
    else {
        char* expected;
        if (has_ident || child)
            expected = "expected identifier or ( or {";
        else
            expected = "expected identifier or {";
        return parser_error_2a(
            p, "invalid import syntax", t->start, t->end, expected, mods->start,
            kw_end, "in this import statement");
    }
    ig_data->relative_to = relative_to;
    ast_node_fill_srange(p, *tgt, istart, end);
    return PE_OK;
}
parse_error parse_import(parser* p, modifier_status* mods, ast_node** tgt)
{
    token* t = lx_aquire(&p->lx);
    ureg kw_end = t->end;
    lx_void(&p->lx);
    ureg decl_cnt = 0;
    parse_error pe = parse_import_with_parent(
        p, mods, kw_end, p->lx.tc->t->mdg.root_node, &decl_cnt, tgt, false);
    if (pe) return pe;
    curr_scope_add_decls(p, mods->data.access_mod, decl_cnt);
    return PE_OK;
}
parse_error parse_require(parser* p, modifier_status* mods)
{
    token* t = lx_aquire(&p->lx);
    parse_error pe = reject_modifiers(p, t, mods);
    if (pe) return pe;
    ureg end = t->end;
    lx_void(&p->lx);
    if (!ast_elem_is_module_frame((ast_elem*)get_bpd(p)->node)) {
        parser_error_1a_pc(
            p, "invalid scope for require statement", t->start, t->end,
            "require statement only allowed at module scope");
        return PE_ERROR;
    }
    PEEK(p, t);
    bool is_extern = false;
    bool is_dynamic = false;
    bool is_runtime = false;
    if (t->kind == TK_KW_RUNTIME) {
        is_runtime = true;
        lx_void(&p->lx);
        PEEK(p, t);
    }
    if (t->kind == TK_KW_STATIC || t->kind == TK_KW_DYNAMIC) {
        is_extern = true;
        is_dynamic = (t->kind == TK_KW_DYNAMIC);
        lx_void(&p->lx);
        PEEK(p, t);
    }
    else if (is_runtime) {
        parser_error_2a(
            p, "unexpected token", t->start, t->end,
            "runtime require must be static or dynamic", mods->start, end,
            "in this require statement");
    }
    if (t->kind != TK_STRING) {
        parser_error_2a(
            p, "unexpected token", t->start, t->end,
            "expected path pattern as string literal", mods->start, end,
            "in this require statement");
        return PE_ERROR;
    }
    file_require rq;
    rq.is_extern = is_extern;
    rq.is_pp = false; // FIXME: set this to true in parse pp statement
    rq.runtime = is_runtime;
    rq.handled = false;
    rq.srange = src_range_pack_lines(p->lx.tc, mods->start, t->end);
    if (rq.srange == SRC_RANGE_INVALID) return PE_FATAL;
    rwlock_read(&p->current_module->lock);
    bool exploring;
    bool needed =
        module_stage_requirements_needed(p->current_module->stage, &exploring);
    rwlock_end_read(&p->current_module->lock);
    if (!is_extern) {
        src_file* f = file_map_get_file_from_path(
            &p->lx.tc->t->filemap, p->current_file->head.parent, t->str);
        if (!f) return RE_FATAL;
        rq.fmh = (file_map_head*)f;
        int r = src_file_require(
            f, p->lx.tc->t, p->lx.smap, rq.srange, p->current_module, needed);
        if (r == ERR) return PE_FATAL;
    }
    else {
        src_lib* l = file_map_get_lib_from_path(
            &p->lx.tc->t->filemap, p->current_file->head.parent, t->str,
            is_dynamic);
        if (!l) return RE_FATAL;
        rq.fmh = (file_map_head*)l;
        if (needed && !exploring) {
            int r = src_lib_require(
                l, p->lx.tc->t, p->lx.smap, rq.srange, rq.is_pp);
            if (r == ERR) return PE_FATAL;
        }
    }
    lx_void(&p->lx);
    rq.handled = needed;
    int r = list_builder_add_block(&p->lx.tc->listb, &rq, sizeof(rq));
    if (r) return PE_FATAL;
    return PE_NO_STMT;
}
static inline parse_error
prefix_modifiers_from_token(parser* p, modifier_status* mods, token* t)
{
    token_kind kw = t->kind;
    // TODO: enforce order
    switch (kw) {
        case TK_KW_PRIVATE:
            mods->end = t->end;
            return ast_flags_from_kw_set_access_mod(p, mods, AM_PRIVATE);
        case TK_KW_PROTECTED:
            mods->end = t->end;
            return ast_flags_from_kw_set_access_mod(p, mods, AM_PROTECTED);
        case TK_KW_PUBLIC:
            mods->end = t->end;
            return ast_flags_from_kw_set_access_mod(p, mods, AM_PUBLIC);
        case TK_KW_CONST: {
            mods->end = t->end;
            if (mods->data.const_mod) {
                report_redundant_specifier(
                    p, token_strings[TK_KW_CONST], mods->start, t->end);
                return PE_ERROR;
            }
            mods->data.const_mod = true;
        } break;
        case TK_KW_STATIC: {
            mods->end = t->end;
            if (mods->data.static_mod) {
                report_redundant_specifier(
                    p, token_strings[TK_KW_STATIC], mods->start, mods->end);
                return PE_ERROR;
            }
            mods->data.static_mod = true;
        } break;
        case TK_KW_EXTERN: {
            mods->end = t->end;
            if (mods->data.extern_mod) {
                report_redundant_specifier(
                    p, token_strings[TK_KW_EXTERN], mods->start, mods->end);
                return PE_ERROR;
            }
            mods->data.extern_mod = true;
        } break;
        case TK_KW_IMPLICIT: {
            mods->end = t->end;
            if (mods->data.implicit_mod) {
                report_redundant_specifier(
                    p, token_strings[TK_KW_EXTERN], mods->start, mods->end);
                return PE_ERROR;
            }
            mods->data.implicit_mod = true;
        } break;
        default: {
            return PE_EOEX;
        } break;
    }
    return PE_OK;
}
static inline parse_error
parse_pp_stmt(parser* p, modifier_status* mods, ast_node** tgt)
{
    token* t = lx_aquire(&p->lx);
    parse_error pe = reject_modifiers(p, t, mods);
    lx_void(&p->lx);
    if (pe) return pe;
    ast_node* pp_stmt;
    pe = parse_statement(p, &pp_stmt);
    if (pe) return pe;
    ast_elem* ppe = (ast_elem*)pp_stmt;
    // for pp symbols we don't want a pp expr node but just the comptime flag
    // TODO: handle pp modules, pp requires etc.
    if (ast_elem_is_var(ppe) || ast_elem_is_struct_base(ppe) ||
        ast_elem_is_func_base(ppe)) {
        ast_node_set_comptime(pp_stmt);
        *tgt = pp_stmt;
        return PE_OK;
    }
    expr_pp* sp = alloc_perm(p, sizeof(expr_pp));
    if (!sp) return PE_FATAL;
    sp->ctype = NULL;
    ast_node_init(&sp->node, EXPR_PP);
    sp->pprn = NULL;
    sp->pp_expr = pp_stmt;
    sp->result_buffer.pasted_src = NULL;
    pe = ast_node_fill_srange(
        p, (ast_node*)sp, mods->start, src_range_get_end(sp->pp_expr->srange));
    *tgt = (ast_node*)sp;
    return pe;
}
parse_error parse_statement(parser* p, ast_node** tgt)
{
    parse_error pe;
    modifier_status mods;
    mods.data = MODIFIERS_NONE;
    token* t;
    PEEK(p, t);
    mods.start = t->start;
    mods.end = t->start;

    while (true) {
        pe = prefix_modifiers_from_token(p, &mods, t);
        if (pe == PE_OK) {
            mods.end = t->end;
            lx_void(&p->lx);
            PEEK(p, t);
            continue;
        }
        if (pe != PE_EOEX) return pe;
        switch (t->kind) {
            case TK_KW_OP: {
                mods.data.func_is_op_mod = true;
            } // fallthrough
            case TK_KW_FUNC: return parse_func_decl(p, &mods, tgt);
            case TK_KW_MACRO: return parse_macro_decl(p, &mods, tgt);
            case TK_KW_STRUCT: return parse_struct_decl(p, &mods, tgt);
            case TK_KW_IMPL: return parse_trait_impl_decl(p, &mods, tgt);
            case TK_KW_TRAIT: return parse_trait_decl(p, &mods, tgt);
            case TK_KW_MODULE:
                return parse_module_frame_decl(p, &mods, false, tgt);
            case TK_KW_EXTEND:
                return parse_module_frame_decl(p, &mods, true, tgt);
            case TK_KW_USE: return parse_use(p, &mods, tgt);
            case TK_KW_REQUIRE: return parse_require(p, &mods);
            case TK_KW_IMPORT: return parse_import(p, &mods, tgt);
            case TK_HASH: return parse_pp_stmt(p, &mods, tgt);
            case TK_IDENTIFIER: {
                token* t2 = lx_peek_2nd(&p->lx);
                if (!t2) return PE_LX_ERROR;
                if (t2->kind == TK_COLON) {
                    return parse_var_decl(p, &mods, tgt);
                }
                pe = reject_modifiers(p, t, &mods);
                if (pe) return pe;
                return parse_expr_stmt(p, tgt);
            }
            default: {
                pe = reject_modifiers(p, t, &mods);
                if (pe) return pe;
                pe = parse_expr_stmt(p, tgt);
                if (pe == PE_EOEX) {
                    PEEK(p, t);
                    parser_error_1a(
                        p, "unexpected token while expecting a statement",
                        t->start, t->end, "expected begin of statement");
                    return PE_ERROR;
                }
                return pe;
            }
        }
    }
}
static inline parse_error parse_delimited_body(
    parser* p, ast_body* b, ast_node* parent, ureg param_count,
    ast_node* first_stmt, ureg bstart, ureg bend, token_kind delimiter)
{
    token* t;
    parse_error pe = PE_OK;
    // to allow deallocation in case of early failiure
    b->elements = (ast_node**)NULL_PTR_PTR;
    b->pprn = NULL;
    b->symtab = NULL;
    ast_node* target;
    PEEK(p, t);
    if (!first_stmt) {
        if (push_bpd(p, parent, b)) return PE_FATAL;
    }
    void** elements_list_start = list_builder_start(&p->lx.tc->listb2);
    if (first_stmt) {
        body_parse_data* bpd = get_bpd(p);
        bpd->body = b;
        bpd->body->parent = b->parent;
        bpd->node = parent;
        if (list_builder_add(&p->lx.tc->listb2, first_stmt)) {
            pop_bpd(p, PE_FATAL);
            return PE_FATAL;
        }
        if (ast_elem_is_expr_block_base((ast_elem*)first_stmt)) {
            assert(ast_elem_is_expr_block_base((ast_elem*)parent));
            ((expr_block_base*)first_stmt)->body.parent = b;
        }
    }
    curr_scope_add_decls(p, AM_LOCAL, param_count);
    while (t->kind != delimiter) {
        if (t->kind != TK_EOF) {
            pe = parse_statement(p, &target);
            bool stmt = (pe != PE_NO_STMT);
            if (pe && pe != PE_NO_STMT) break;
            pe = handle_semicolon_after_statement(p, target);
            if (pe) break;
            if (stmt && list_builder_add(&p->lx.tc->listb2, target)) {
                pe = PE_FATAL;
                break;
            }
            t = lx_peek(&p->lx);
            if (!t) {
                pe = PE_LX_ERROR;
                break;
            }
        }
        else {
            parser_error_2a_pc(
                p, "unterminated block", t->start, t->end,
                "reached EOF before block was closed", bstart, bend,
                "block starts here");
            pe = PE_ERROR;
            break;
        }
    }
    if (!pe) {
        lx_consume(&p->lx);
        b->srange = src_range_pack_lines(p->lx.tc, bstart, t->end);
        if (b->srange == SRC_RANGE_INVALID) pe = PE_FATAL;
    }
    b->elements = (ast_node**)list_builder_pop_list_zt(
        &p->lx.tc->listb2, elements_list_start, &p->lx.tc->permmem);
    if (!b->elements) {
        free_failed_ast_node_list_symtabs(
            &p->lx.tc->listb2, elements_list_start, &b->elements);
        pe = PE_FATAL;
    }
    if (pop_bpd(p, pe)) {
        return PE_FATAL;
    }
    return pe;
}
parse_error parse_brace_delimited_body(
    parser* p, ast_body* b, ast_node* parent, ureg param_count)
{
    token* t = lx_aquire(&p->lx);
    lx_void(&p->lx);
    return parse_delimited_body(
        p, b, parent, param_count, NULL, t->start, t->end, TK_BRACE_CLOSE);
}
parse_error parse_scope_body(parser* p, scope* s, ureg param_count)
{
    token* t;
    PEEK(p, t);
    if (t->kind != TK_BRACE_OPEN) {
        parser_error_2a(
            p, "expected scope body", t->start, t->end,
            "expected '{' to begin scope",
            src_range_get_start(s->osym.sym.node.srange),
            src_range_get_end(s->osym.sym.node.srange),
            get_context_msg(p, (ast_node*)s));
        return PE_ERROR;
    }
    return parse_brace_delimited_body(p, &s->body, (ast_node*)s, param_count);
}
parse_error parse_module_frame_body(
    parser* p, module_frame* mf, mdg_node* m, ureg param_count)
{
    mdg_node* parent = p->current_module;
    p->current_module = m;
    mf->requires =
        (file_require*)list_builder_start_blocklist(&p->lx.tc->listb);
    parse_error pe = parse_brace_delimited_body(p, &mf->body, (ast_node*)mf, 0);
    mf->requires = (file_require*)list_builder_pop_block_list_zt(
        &p->lx.tc->listb, mf->requires, &p->lx.tc->permmem);
    p->current_module = parent;
    return pe;
}
parse_error
parse_braced_namable_body(parser* p, ast_node* parent, ast_body* b, char** name)
{
    token* t;
    PEEK(p, t);
    if (t->kind == TK_AT) {
        lx_void(&p->lx);
        PEEK(p, t);
        if (t->kind != TK_IDENTIFIER) {
            src_range_large srl;
            src_range_unpack(parent->srange, &srl);
            return parser_error_2a(
                p, "expected block", t->start, t->end,
                "expected open brace or label to begin block", srl.start,
                srl.end, get_context_msg(p, (ast_node*)parent));
        }
        *name = alloc_string_perm(p, t->str);
        if (!*name) return PE_FATAL;
        lx_void(&p->lx);
        PEEK(p, t);
    }
    else {
        *name = NULL;
    }
    if (t->kind == TK_BRACE_OPEN) {
        return parse_brace_delimited_body(p, b, (ast_node*)parent, 0);
    }
    src_range_large srl;
    src_range_unpack(parent->srange, &srl);
    return parser_error_2a(
        p, "expected block", t->start, t->end,
        "expected open brace or label to begin block", srl.start, srl.end,
        get_context_msg(p, (ast_node*)parent));
}
