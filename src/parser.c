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
#include <stddef.h>

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
parse_error parse_statement(parser* p, ast_node** tgt);
parse_error parse_scope_body(parser* p, scope* s, ureg param_count);
parse_error
parse_open_scope_body(parser* p, open_scope* s, mdg_node* n, ureg param_count);
parse_error parse_body(parser* p, body* b, ast_node* parent);
parse_error parse_expression(parser* p, ast_node** tgt);
parse_error parse_expression_of_prec(parser* p, ast_node** ex, ureg prec);
parse_error parse_brace_delimited_body(
    parser* p, body* b, ast_node* parent, ureg param_count);
parse_error
parse_braced_namable_body(parser* p, ast_node* parent, body* b, char** name);
parse_error parse_expr_in_parens(
    parser* p, ast_node* parent, ureg start, ureg end, ast_node** tgt);
static const unsigned char op_precedence[] = {
        [OP_POST_INCREMENT] = 15,
        [OP_POST_DECREMENT] = 15,
        [OP_CALL] = 15,
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
        [OP_POINTER_OF] = 14,
        [OP_REF_OF] = 14,
        [OP_RREF_OF] = 14,
        [OP_CLOSURE_BY_VALUE] = 14,
        [OP_CONST] = 14,
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

#define PREC_BASELINE 0

static inline bool is_left_associative(ast_node_kind t)
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
        case OSC_MODULE:
        case OSC_MODULE_GENERIC:
        case OSC_EXTEND:
        case OSC_EXTEND_GENERIC:
        case SC_STRUCT:
        case SC_STRUCT_GENERIC:
        case SC_TRAIT:
        case SC_TRAIT_GENERIC: return true;
        case EXPR_WHILE:
        case EXPR_LOOP:
        case EXPR_MATCH:
        case EXPR_BLOCK: return true;
        case EXPR_IF: {
            expr_if* i = (expr_if*)e;
            if (i->else_body) {
                return (i->else_body->kind == EXPR_BLOCK);
            }
            else {
                return (i->if_body->kind == EXPR_BLOCK);
            }
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
        case TK_AND: return OP_REF_OF;
        case TK_PERCENT: return OP_POINTER_OF;
        case TK_CARET: return OP_CLOSURE_BY_VALUE;
        case TK_DOLLAR: return OP_RREF_OF;
        case TK_DOUBLE_PLUS: return OP_PRE_INCREMENT;
        case TK_DOUBLE_MINUS: return OP_PRE_DECREMENT;
        case TK_KW_CONST: return OP_CONST;
        case TK_HASH: return OP_PP;
        default: return OP_NOOP;
    }
}
static inline operator_kind token_to_postfix_unary_op(token* t)
{
    switch (t->kind) {
        case TK_DOUBLE_PLUS: return OP_POST_INCREMENT;
        case TK_DOUBLE_MINUS: return OP_POST_DECREMENT;
        case TK_PAREN_OPEN: return OP_CALL;
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
static inline void* alloc_ppool(parser* p, ureg size, pool* pool)
{
    void* mem = pool_alloc(pool, size);
    if (!mem) error_log_report_allocation_failiure(&p->lx.tc->error_log);
    return mem;
}
static inline void* alloc_temp(parser* p, ureg size)
{
    return alloc_ppool(p, size, &p->lx.tc->tempmem);
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
        &p->lx.tc->error_log, ES_PARSER, false, msg, p->lx.file, start, end,
        annot);
    return PE_ERROR;
}
static inline parse_error parser_error_2a(
    parser* p, char* msg, ureg start, ureg end, char* annot, ureg start2,
    ureg end2, char* annot2)
{
    error_log_report_annotated_twice(
        &p->lx.tc->error_log, ES_PARSER, false, msg, p->lx.file, start, end,
        annot, p->lx.file, start2, end2, annot2);
    return PE_ERROR;
}
static inline parse_error parser_error_3a(
    parser* p, char* msg, ureg start, ureg end, char* annot, ureg start2,
    ureg end2, char* annot2, ureg start3, ureg end3, char* annot3)
{
    error_log_report_annotated_thrice(
        &p->lx.tc->error_log, ES_PARSER, false, msg, p->lx.file, start, end,
        annot, p->lx.file, start2, end2, annot2, p->lx.file, start3, end3,
        annot3);
    return PE_ERROR;
}
static inline body_parse_data* get_bpd(parser* p)
{
    sbi i;
    sbi_begin_at_end(&i, &p->body_stack);
    return sbi_previous(&i, sizeof(body_parse_data));
}
static inline void init_bpd(body_parse_data* bpd, ast_node* node, body* body)
{
    bpd->node = node;
    bpd->body = body;
    bpd->decl_count = 0;
    bpd->usings_count = 0;
    bpd->shared_decl_count = 0;
    bpd->shared_usings_count = 0;
}
static inline int push_bpd(parser* p, ast_node* n, body* b)
{
    body_parse_data* bpd =
        sbuffer_append(&p->body_stack, sizeof(body_parse_data));
    if (bpd == NULL) return ERR;
    // make sure the symtab is NULL to prevent it from being freed
    if (b) b->symtab = NULL;
    init_bpd(bpd, n, b);
    return OK;
}
static inline int push_bpd_pp(parser* p, ast_node* n)
{
    sbi i;
    sbi_begin_at_end(&i, &p->body_stack);
    body_parse_data* prev = sbi_previous(&i, sizeof(body_parse_data));
    body_parse_data* bpd =
        sbuffer_append(&p->body_stack, sizeof(body_parse_data));
    if (bpd == NULL) return ERR;
    init_bpd(bpd, n, prev->body);
    return OK;
}
static inline int pop_bpd_pp(parser* p, parse_error pe)
{
    body_parse_data bpd_popped;
    body_parse_data* bpd;
    sbi it;
    sbi_begin_at_end(&it, &p->body_stack);
    bpd_popped = *(body_parse_data*)sbi_previous(&it, sizeof(body_parse_data));
    sbuffer_remove(&p->body_stack, &it, sizeof(body_parse_data));
    bpd = sbi_previous(&it, sizeof(body_parse_data));
    // since we are a pp node there must at least be the base node
    assert(bpd->body == bpd_popped.body);
    ureg pp_level = 0;
    do {
        pp_level++;
        bpd = sbi_previous(&it, sizeof(body_parse_data));
    } while (bpd && bpd->body == bpd_popped.body);
    for (ureg i = 0; i < pp_level; i++) {
        if (!bpd || bpd->node != NULL) {
            assert(i == pp_level - 1);
            bpd = sbuffer_insert(&p->body_stack, &it, sizeof(body_parse_data));
            if (!bpd) return ERR;
            init_bpd(bpd, NULL, bpd_popped.body);
            break;
        }
        bpd = sbi_previous(&it, sizeof(body_parse_data));
    }
    bpd->decl_count += bpd_popped.decl_count;
    bpd->usings_count += bpd_popped.usings_count;
    bpd->shared_decl_count += bpd_popped.shared_decl_count;
    bpd->shared_usings_count += bpd_popped.shared_usings_count;
    return OK;
}
static inline int pop_bpd(parser* p, parse_error pe)
{
    sbi i;
    sbi_begin_at_end(&i, &p->body_stack);
    body_parse_data bpd =
        *(body_parse_data*)sbi_previous(&i, sizeof(body_parse_data));
    assert(bpd.node); // make sure it's not a pp node
    if (bpd.shared_decl_count > 0 || bpd.shared_usings_count > 0) {
        assert(ast_elem_is_open_scope((ast_elem*)bpd.node));
        // assert(*osc is member of current module*);
        atomic_ureg_add(&p->current_module->decl_count, bpd.shared_decl_count);
        atomic_ureg_add(
            &p->current_module->using_count, bpd.shared_usings_count);
    }
    body* bd = bpd.body;
    symbol_table** st = &bd->symtab;
    while (true) {
        sbuffer_remove(&p->body_stack, &i, sizeof(body_parse_data));
        body_parse_data* bpd2 =
            (body_parse_data*)sbi_previous(&i, sizeof(body_parse_data));
        bool has_pp = (bpd2 && bpd2->node == NULL);
        if (!pe) {
            if (symbol_table_init(
                    st, bpd.decl_count, bpd.usings_count, has_pp,
                    (ast_elem*)bpd.node))
                return ERR;
        }
        else {
            *st = NULL;
        }
        if (*st) st = &(**st).pp_symtab;
        if (!has_pp) break;
        bpd = *bpd2;
        // we don't support shared pp decls for now :(
        assert(bpd.shared_decl_count == 0 && bpd.shared_usings_count == 0);
    }
    if (*st) *st = NULL;
    return OK;
}

char* get_context_msg(parser* p, ast_node* node)
{
    if (!node) return NULL;
    switch (node->kind) {
        case SC_FUNC: return "in this function";
        case SC_FUNC_GENERIC: return "in this generic function";
        case SC_STRUCT: return "in this struct";
        case SC_STRUCT_GENERIC: return "in this generic struct";
        case SC_TRAIT: return "in this struct";
        case SC_TRAIT_GENERIC: return "in this generic struct";
        case OSC_MODULE: return "in this module";
        case OSC_MODULE_GENERIC: return "in this generic module";
        case OSC_EXTEND: return "in this extend statement";
        case EXPR_BLOCK: return "in this block expression";
        case EXPR_DO_WHILE: return "in this do while expression";
        case EXPR_WHILE: return "in this while expression";
        case EXPR_LOOP: return "in this loop expression";
        case EXPR_CONTINUE: return "in this continue statement";
        case EXPR_BREAK: return "in this break statement";
        case EXPR_MATCH: return "in this match expression";
        case EXPR_IF: return "in this if expression";
        case OSC_EXTEND_GENERIC: return "in this generic extend statement";
        case EXPR_LAMBDA: return "in this lambda";
        case SYM_VAR:
        case SYM_VAR_INITIALIZED: return "in this variable declaration";
        case SYM_IMPORT_SYMBOL:
        case SYM_IMPORT_GROUP:
        case SYM_IMPORT_MODULE: return "in this import statement";
        case SYM_NAMED_USING:
        case STMT_USING: return "in this using statement";
        case STMT_COMPOUND_ASSIGN:
            return "in this compound assignment statement";
        default: panic("unexpected parent context");
    }
    return NULL;
}
body* get_current_body(parser* p)
{
    return get_bpd(p)->body;
}
static inline void
curr_scope_add_usings(parser* p, access_modifier am, ureg count)
{
    body_parse_data* bpd = get_bpd(p);
    if (am == AM_UNSPECIFIED) {
        bpd->usings_count += count;
    }
    else {
        if (ast_elem_is_open_scope((ast_elem*)bpd->node)) {
            bpd->shared_usings_count += count;
        }
        else {
            bpd->usings_count += count;
        }
    }
}
static inline void
curr_scope_add_decls(parser* p, access_modifier am, ureg count)
{
    body_parse_data* bpd = get_bpd(p);
    if (am == AM_UNSPECIFIED) {
        bpd->decl_count += count;
    }
    else {
        if (ast_elem_is_open_scope((ast_elem*)bpd->node)) {
            bpd->shared_decl_count += count;
        }
        else {
            bpd->decl_count += count;
        }
    }
}

static inline void
parser_error_1a_pc(parser* p, char* msg, ureg start, ureg end, char* annot)
{
    ast_node* parent = get_bpd(p)->node;
    if (parent != (ast_node*)&p->lx.file->root) {
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
    if (parent != (ast_node*)&p->lx.file->root) {
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

static inline void parser_error_unexpected_token(
    parser* p, token* t, token_kind exp_tt, char* msg, char* ctx,
    ureg ctx_start, ureg ctx_end)
{
    char* expstr = "expected ";
    ureg explen = strlen(expstr);
    ureg toklen = strlen(token_strings[exp_tt]);
    char* ann =
        (char*)error_log_alloc(&p->lx.tc->error_log, explen + toklen + 3);
    if (!ann) return;
    memcpy(ann, expstr, explen);
    ann[explen] = '\'';
    memcpy(ann + explen + 1, token_strings[exp_tt], toklen);
    ann[explen + 1 + toklen] = '\'';
    ann[explen + 1 + toklen + 1] = '\0';
    parser_error_2a(p, msg, t->start, t->end, ann, ctx_start, ctx_end, ctx);
}
static inline void
ast_node_init_with_flags(ast_node* n, ast_node_kind kind, ast_node_flags flags)
{
    n->kind = kind;
    n->flags = flags;
}
static inline void
ast_node_init_with_op(ast_node* n, ast_node_kind kind, operator_kind opk)
{
    ast_node_init_with_flags(n, kind, AST_NODE_FLAGS_DEFAULT);
    n->operator_kind = opk;
}
static inline void
ast_node_init_with_pk(ast_node* n, ast_node_kind kind, primitive_kind pk)
{
    ast_node_init_with_flags(n, kind, AST_NODE_FLAGS_DEFAULT);
    n->primitive_kind = pk;
}
static inline void ast_node_init(ast_node* n, ast_node_kind kind)
{
    ast_node_init_with_flags(n, kind, AST_NODE_FLAGS_DEFAULT);
}

static inline void body_init_empty(body* b)
{
    b->elements = (ast_node**)NULL_PTR_PTR;
    b->symtab = NULL;
}
int parser_init(parser* p, thread_context* tc)
{
    int r = lx_init(&p->lx, tc);
    if (r) return r;
    r = list_builder_init(&p->lx.tc->list_builder, &p->lx.tc->tempmem, 64);
    if (r) {
        lx_fin(&p->lx);
        return r;
    }
    r = sbuffer_init(&p->body_stack, sizeof(body_parse_data) * 16);
    if (r) {
        list_builder_fin(&p->lx.tc->list_builder);
        lx_fin(&p->lx);
        return r;
    }
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
    srl.file = NULL;
    if (ast_node_flags_get_access_mod(s->node.flags) != AM_UNSPECIFIED) {
        if (ast_elem_is_open_scope((ast_elem*)get_bpd(p)->node)) {
            srl.file = p->lx.file;
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
    parser* p, sym_param* tgt, ureg ctx_start, ureg ctx_end, char* msg_context)
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
    tgt->symbol.name = alloc_string_perm(p, t->str);
    if (!tgt->symbol.name) return PE_FATAL;
    // TODO: flags parsing
    ast_node_init((ast_node*)tgt, SYM_PARAM);
    lx_void(&p->lx);
    PEEK(p, t);
    if (t->kind != TK_COLON) {
        parser_error_2a(
            p, "invalid parameter syntax", t->start, t->end,
            "expected ':' after identifier", ctx_start, ctx_end, msg_context);
        return PE_ERROR;
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
    curr_scope_add_decls(p, AM_UNSPECIFIED, 1);
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
        else {
            *tgt = alloc_perm(p, sizeof(ast_node*));
            if (!*tgt) return PE_FATAL;
            **tgt = prefetch;
            *elem_count = 1;
            return PE_OK;
        }
    }
    void** list_start = list_builder_start(&p->lx.tc->list_builder);
    if (prefetch) list_builder_add(&p->lx.tc->list_builder, prefetch);
    while (true) {
        ast_node* ex;
        parse_error pe = parse_expression(p, &ex);
        if (pe == PE_EOEX) {
            PEEK(p, t);
            char* msg = error_log_cat_strings_3(
                &p->lx.tc->error_log, "invalid ", type, " syntax");
            if (!msg) return PE_FATAL;
            parser_error_1a(
                p, msg, t->start, t->end, "expected expression after ','");
            return PE_ERROR;
        }
        if (pe != PE_OK) return pe;
        int r = list_builder_add(&p->lx.tc->list_builder, (void*)ex);
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
        &p->lx.tc->list_builder, list_start, &p->lx.tc->permmem, elem_count, 0,
        0);
    if (!*tgt) return PE_FATAL;
    return PE_OK;
}
static inline parse_error parse_array(parser* p, token* t, ast_node** ex)
{
    ureg t_start = t->start;
    ureg t_end = t->end;
    lx_void(&p->lx);
    expr_array* arr = alloc_perm(p, sizeof(expr_array));
    if (!arr) return PE_FATAL;
    ast_node_init((ast_node*)arr, EXPR_ARRAY);
    parse_error pe = parse_expr_node_list(
        p, NULL, &arr->elements, &arr->elem_count, "array", TK_BRACKET_CLOSE);
    // TODO: EMSG: suboptimal e.g. for case [,,]
    if (pe == PE_ERROR) {
        PEEK(p, t);
        parser_error_2a(
            p, "array brackets missmatch", t->start, t->end,
            "reached end of expression due to unexpected token", t_start, t_end,
            "didn't find a matching bracket for this array");
        return PE_ERROR;
    }
    if (pe != PE_OK) return pe;
    PEEK(p, t);
    if (ast_node_fill_srange(p, &arr->node, t_start, t->end)) return PE_FATAL;
    lx_void(&p->lx);
    *ex = (ast_node*)arr;
    return PE_OK;
}
static inline parse_error parse_tuple_after_first_comma(
    parser* p, ureg t_start, ureg t_end, ast_node** ex)
{
    expr_tuple* tp = alloc_perm(p, sizeof(expr_tuple));
    ast_node_init((ast_node*)tp, EXPR_TUPLE);
    void** list = list_builder_start(&p->lx.tc->list_builder);
    list_builder_add(&p->lx.tc->list_builder, *ex);
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
        list_builder_add(&p->lx.tc->list_builder, *ex);
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
        &p->lx.tc->list_builder, list, &p->lx.tc->permmem, &tp->elem_count, 0,
        0);
    if (!tp->elements) return PE_FATAL;
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
static inline parse_error require_default_flags(
    parser* p, token* t, ast_node_flags flags, ureg start, ureg end)
{
    if (flags == AST_NODE_FLAGS_DEFAULT) return PE_OK;
    char* loc_msg = error_log_cat_strings_2(
        &p->lx.tc->error_log, token_strings[t->kind],
        " does not accept any modifiers");
    if (!loc_msg) return PE_FATAL;
    parser_error_2a(
        p, loc_msg, start, end, "invalid modifier(s)", t->start, t->end,
        "before this statement");
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
    parse_error pe = parse_expression_of_prec(p, ex, PREC_BASELINE);
    if (pe != PE_OK && pe != PE_EOEX) return pe;
    PEEK(p, t);
    if (t->kind == TK_COMMA) {
        lx_void(&p->lx);
        return parse_tuple_after_first_comma(p, t_start, t_end, ex);
    }
    else if (t->kind == TK_PAREN_CLOSE) {
        return build_expr_parentheses(p, t_start, t_end, ex);
    }
    else {
        parser_error_2a(
            p, "unexpected token after expression", t->start, t->end,
            "expected comma or closing parenthesis", t_start, t_end,
            "in parenthesized expression starting here");
        return PE_ERROR;
    }
}
typedef union tuple_ident_node {
    sym_var var;
    expr_identifier ident;
} tuple_ident_node;

static inline parse_error
parse_uninitialized_var_in_tuple(parser* p, token* t, ast_node** ex)
{
    sym_var* v = alloc_perm(p, sizeof(tuple_ident_node));
    ast_node_init((ast_node*)v, SYM_VAR);
    ast_node_flags_set_compound_decl(&v->symbol.node.flags);
    v->symbol.name = alloc_string_perm(p, t->str);
    if (!v->symbol.name) return PE_FATAL;
    if (ast_node_fill_srange(p, (ast_node*)v, t->start, t->end))
        return PE_FATAL;
    lx_void_n(&p->lx, 2);
    PEEK(p, t);
    if (t->kind == TK_COMMA || t->kind == TK_PAREN_CLOSE) {
        v->type = NULL;
        *ex = (ast_node*)v;
        return PE_OK;
    }
    else {
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
}

static inline parse_error
build_ident_node_in_tuple(parser* p, token* t, ast_node** ex)
{
    tuple_ident_node* tin = alloc_perm(p, sizeof(tuple_ident_node));
    sym_var* v = &tin->var;
    ast_node_init(&v->symbol.node, SYM_VAR);
    v->symbol.name = alloc_string_perm(p, t->str);
    if (!v->symbol.name) return PE_FATAL;
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
            tuple_ident_node* tin = (tuple_ident_node*)*elems;
            if (tin->var.type == NULL &&
                !ast_node_flags_get_compound_decl(tin->var.symbol.node.flags)) {
                ureg srange = tin->var.symbol.node.srange;
                tin->ident.value.str = tin->var.symbol.name;
                tin->ident.node.srange = srange;
                ast_node_init((ast_node*)tin, EXPR_IDENTIFIER);
            }
        }
        else if ((**elems).kind == EXPR_TUPLE) {
            expr_tuple* t = *((expr_tuple**)elems);
            turn_ident_nodes_to_exprs(t->elements, t->elem_count);
        }
        elems++;
    }
}
static inline parse_error parse_paren_group_or_tuple_or_compound_decl(
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
                t->end, "expected an expression, a declaration or a closing "
                        "parenthesis",
                t_start, t_end, "opening parenthesis here");
            return PE_ERROR;
        }
        if (pe) return pe;
        PEEK(p, t);
        if (t->kind == TK_COMMA) {
            element_list = list_builder_start(&p->lx.tc->list_builder);
            if (list_builder_add(&p->lx.tc->list_builder, *ex)) return PE_FATAL;
        }
        else {
            return build_expr_parentheses(p, t_start, t_end, ex);
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
                return build_expr_parentheses(p, t_start, t_end, ex);
            }
            PEEK(p, t);
        }
        else {
            pe = build_ident_node_in_tuple(p, t, ex);
            if (pe) return pe;
            t = t2;
            (*ident_count)++;
        }
        element_list = list_builder_start(&p->lx.tc->list_builder);
        if (list_builder_add(&p->lx.tc->list_builder, *ex)) return PE_FATAL;
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
                &p->lx.tc->list_builder, element_list, &p->lx.tc->permmem,
                &res_elem_count, 0, 0);
            if (!res_elem_list) return PE_FATAL;
            if (elem_list) {
                *elem_list = res_elem_list;
                *elem_count = res_elem_count;
                *list_end = t->end;
            }
            else {
                expr_tuple* tp = alloc_perm(p, sizeof(expr_tuple));
                if (!tp) return PE_FATAL;
                ast_node_init((ast_node*)tp, EXPR_TUPLE);
                if (ast_node_fill_srange(p, (ast_node*)tp, t_start, t->end))
                    return PE_FATAL;
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
            pe = parse_paren_group_or_tuple_or_compound_decl(
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
        if (list_builder_add(&p->lx.tc->list_builder, *ex)) return PE_FATAL;
        PEEK(p, t);
    }
}
static inline parse_error
parse_prefix_unary_op(parser* p, ast_node_kind op, ast_node** ex)
{
    token* t = lx_aquire(&p->lx);
    expr_op_unary* ou = (expr_op_unary*)alloc_perm(p, sizeof(expr_op_unary));
    if (!ou) return PE_FATAL;
    if (ast_node_fill_srange(p, &ou->node, t->start, t->end)) return PE_FATAL;
    lx_void(&p->lx);
    ast_node_init_with_op((ast_node*)ou, EXPR_OP_UNARY, op);
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
    ast_node** target, ureg* lbl_end, ast_node_kind break_kind)
{
    // TODO: break targets might be hidden due to macros
    token* t;
    PEEK(p, t);
    sbi it;
    sbi_begin_at_end(&it, &p->body_stack);
    body_parse_data* bpd = sbi_previous(&it, sizeof(body_parse_data));
    if (t->kind == TK_AT) {
        token* t2;
        PEEK_SND(p, t2);
        if (t2->kind == TK_IDENTIFIER) {
            while (true) {
                if (!bpd) {
                    parser_error_2a(
                        p, "invalid break label", t->start, t2->end,
                        "no parent expression with this label", req_start,
                        req_end, get_context_msg(p, (ast_node*)requiring));
                    return PE_ERROR;
                }
                char* n = ast_elem_get_label((ast_elem*)bpd->node, NULL);
                if (n && string_eq_cstr(t2->str, n)) {
                    *target = bpd->node;
                    break;
                }
                bpd = sbi_previous(&it, sizeof(body_parse_data));
            }
        }
        else if (is_kw_valid_label(t->kind)) {
            while (true) {
                if (!bpd) {
                    parser_error_2a(
                        p, "invalid break target", t->start, t2->end,
                        "no parent expression of this kind", req_start, req_end,
                        get_context_msg(p, (ast_node*)requiring));
                    return PE_ERROR;
                }
                if (strcmp(
                        token_strings[t->kind],
                        ast_elem_get_label((ast_elem*)bpd->node, NULL)) == 0) {
                    *target = bpd->node;
                    break;
                }
                bpd = sbi_previous(&it, sizeof(body_parse_data));
            }
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
        while (true) {
            if (!bpd) {
                parser_error_1a(
                    p, "invalid break", req_start, req_end,
                    "no suitable parent expression");
                return PE_ERROR;
            }
            ast_node_kind k = bpd->node->kind;
            if (break_kind == EXPR_CONTINUE) {
                if (k == EXPR_LOOP || k == EXPR_MATCH) {
                    *target = bpd->node;
                    break;
                }
            }
            else {
                if (k == EXPR_LOOP || k == EXPR_MATCH || k == EXPR_BLOCK ||
                    k == EXPR_IF) {
                    *target = bpd->node;
                    break;
                }
            }
            bpd = sbi_previous(&it, sizeof(body_parse_data));
        }
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
        p, (ast_node*)c, start, end, &c->target, &end, EXPR_CONTINUE);
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
    expr_break* r = alloc_perm(p, sizeof(expr_break));
    if (!r) return PE_FATAL;
    ast_node_init((ast_node*)r, EXPR_RETURN);
    PEEK(p, t);
    if (t->kind == TK_SEMICOLON) {
        r->value = NULL;
        if (ast_node_fill_srange(p, (ast_node*)r, start, t->end))
            return PE_FATAL;
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
    sbi it;
    r->target = NULL;
    sbi_begin_at_end(&it, &p->body_stack);
    for (body_parse_data* bpd = sbi_previous(&it, sizeof(body_parse_data));
         bpd != NULL; bpd = sbi_previous(&it, sizeof(body_parse_data))) {
        if (bpd->node && (bpd->node->kind == SC_FUNC ||
                          bpd->node->kind == SC_FUNC_GENERIC)) {
            r->target = bpd->node;
            break;
        }
    }
    if (!r->target) {
        parser_error_1a(
            p, "can't return when not in a function", start,
            src_range_get_end(r->node.srange),
            "this return statement is not in a function");
        return PE_ERROR;
    }
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
        p, (ast_node*)g, start, end, &g->target, &end, EXPR_BREAK);
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
static inline parse_error
parse_expr_block(parser* p, char* label, ureg start, ast_node** ex)
{
    expr_block* b = alloc_perm(p, sizeof(expr_block));
    ast_node_init((ast_node*)b, EXPR_BLOCK);
    b->name = label;
    *ex = (ast_node*)b;
    parse_error pe = parse_brace_delimited_body(p, &b->body, (ast_node*)*ex, 0);
    if (pe) return pe;
    pe = ast_node_fill_srange(
        p, (ast_node*)b, start, src_range_get_end(b->body.srange));
    return pe;
}
parse_error parse_loop(parser* p, ast_node** tgt)
{
    token* t = lx_aquire(&p->lx);
    ureg start = t->start;
    lx_void(&p->lx);
    expr_loop* l = alloc_perm(p, sizeof(expr_loop));
    if (!l) return PE_FATAL;
    if (ast_node_fill_srange(p, (ast_node*)l, start, t->end)) return PE_FATAL;
    ast_node_init((ast_node*)l, EXPR_LOOP);
    *tgt = (ast_node*)l;
    return parse_braced_namable_body(p, (ast_node*)l, &l->body, &l->name);
}
parse_error parse_match(parser* p, ast_node** tgt)
{
    token* t = lx_aquire(&p->lx);
    ureg start = t->start;
    ureg t_end = t->end;
    ureg body_start;
    lx_void(&p->lx);
    expr_match* em = alloc_perm(p, sizeof(expr_match));
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
    else {
        body_start = t->start;
        em->name = NULL;
    }
    lx_void(&p->lx);
    push_bpd(p, (ast_node*)em, &em->body);
    void** list = list_builder_start(&p->lx.tc->list_builder);
    while (true) {
        PEEK(p, t);
        if (t->kind == TK_BRACE_CLOSE) {
            em->body.elements = (ast_node**)list_builder_pop_list_zt(
                &p->lx.tc->list_builder, list, &p->lx.tc->permmem);
            if (!em->body.elements) {
                pop_bpd(p, PE_FATAL);
                return PE_FATAL;
            }
            *tgt = (ast_node*)em;
            lx_void(&p->lx);
            em->body.srange =
                src_range_pack(p->lx.tc, body_start, t->end, p->lx.file);
            pop_bpd(p, PE_OK);
            return PE_OK;
        }
        else {
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
                    p, "invalid match syntax", t->start, t->end,
                    "expected '=>'", exp_start, exp_end,
                    "after this match condition");
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
            list_builder_add(&p->lx.tc->list_builder, ma);
        }
    }
    list_builder_drop_list(&p->lx.tc->list_builder, list);
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
    return parse_expr_block(p, label, start, tgt);
}
parse_error parse_control_block(parser* p, ast_node** tgt)
{
    token* t;
    PEEK(p, t);
    if (t->kind == TK_BRACE_OPEN) {
        return parse_expr_block(p, NULL, t->start, tgt);
    }
    else if (t->kind == TK_AT) {
        return parse_labeled_block(p, tgt);
    }
    else {
        return parse_expression(p, tgt);
    }
}
parse_error parse_if(parser* p, ast_node** tgt)
{
    token* t = lx_aquire(&p->lx);
    ureg start = t->start;
    ureg end = t->end;
    lx_void(&p->lx);
    expr_if* i = alloc_perm(p, sizeof(expr_if));
    if (!i) return PE_FATAL;
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
    ast_node_init(&sp->node, EXPR_PP);
    if (push_bpd_pp(p, (ast_node*)sp)) return PE_FATAL;
    parse_error pe =
        parse_expression_of_prec(p, &sp->pp_expr, op_precedence[OP_PP]);
    if (pop_bpd_pp(p, pe)) return PE_FATAL;
    if (pe) return pe;
    pe = ast_node_fill_srange(
        p, (ast_node*)sp, start, src_range_get_end(sp->pp_expr->srange));
    *tgt = (ast_node*)sp;
    return pe;
}
static inline parse_error parse_value_expr(parser* p, ast_node** ex)
{
    token* t;
    PEEK(p, t);
    switch (t->kind) {
        case TK_PAREN_OPEN: return parse_paren_group_or_tuple(p, t, ex);

        case TK_BRACKET_OPEN: return parse_array(p, t, ex);

        case TK_BRACE_OPEN: return parse_expr_block(p, NULL, t->start, ex);

        case TK_KW_LOOP: return parse_loop(p, ex);

        case TK_KW_MATCH: return parse_match(p, ex);

        case TK_HASH: return parse_pp_expr(p, ex);

        case TK_KW_IF: return parse_if(p, ex);

        case TK_KW_RETURN: return parse_return(p, ex);

        case TK_KW_BREAK: return parse_break(p, ex);

        case TK_KW_CONTINUE: return parse_continue(p, ex);

        case TK_NUMBER: return parse_literal(p, EXPR_LITERAL, PT_INT, ex);
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
static inline parse_error parse_call(parser* p, ast_node** ex, ast_node* lhs)
{
    token* t = lx_aquire(&p->lx);
    ureg t_start = t->start;
    lx_void(&p->lx);
    expr_call* call = alloc_perm(p, sizeof(expr_call));
    if (!call) return PE_FATAL;
    parse_error pe = parse_expr_node_list(
        p, NULL, &call->args, &call->arg_count, "call", TK_PAREN_CLOSE);
    // EMSG: suboptimal e.g. for case {,,}
    if (pe == PE_ERROR) {
        PEEK(p, t);
        parser_error_2a(
            p, "unclosed function call", t->start, t->end,
            "reached end of expression due to unexpected token", t_start,
            t_start + 1, "didn't find a matching parenthesis for this");
        return PE_ERROR;
    }
    if (pe != PE_OK) return pe;
    PEEK(p, t);
    if (ast_node_fill_srange(p, &call->node, t_start, t->end)) return PE_FATAL;
    lx_void(&p->lx);
    ast_node_init_with_op((ast_node*)call, EXPR_CALL, OP_CALL);
    call->lhs = lhs;
    *ex = (ast_node*)call;
    return PE_OK;
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
    ast_node_init_with_op((ast_node*)acc, EXPR_ACCESS, OP_ACCESS);
    acc->lhs = lhs;
    *ex = (ast_node*)acc;
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
    else {
        token* t = lx_aquire(&p->lx);
        lx_void(&p->lx);
        expr_op_unary* ou =
            (expr_op_unary*)alloc_perm(p, sizeof(expr_op_unary));
        if (!ou) return PE_FATAL;
        ast_node_init_with_op((ast_node*)ou, EXPR_OP_UNARY, op);
        ou->child = lhs;
        if (ast_node_fill_srange(p, &ou->node, t->start, t->end))
            return PE_FATAL;
        *ex = (ast_node*)ou;
        return PE_OK;
    }
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
        member ? OP_MEMBER_ACCESS : OP_SCOPE_ACCESS);
    esa->lhs = lhs;
    PEEK(p, t);
    if (t->kind != TK_IDENTIFIER) {
        return parser_error_1a(
            p, member ? "invalid operand for the member access operator"
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

    if (op == OP_SCOPE_ACCESS || op == OP_MEMBER_ACCESS)
        return parse_scope_access(p, ex, lhs, (op == OP_MEMBER_ACCESS));
    token* t = lx_aquire(&p->lx);
    lx_void(&p->lx);
    expr_op_binary* ob = (expr_op_binary*)alloc_perm(p, sizeof(expr_op_binary));
    if (!ob) return PE_FATAL;
    if (ast_node_fill_srange(p, &ob->node, t->start, t->end)) return PE_FATAL;
    ast_node_init_with_op((ast_node*)ob, EXPR_OP_BINARY, op);
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
        op = token_to_postfix_unary_op(t);
        if (op == OP_NOOP) break;
        if (op_precedence[op] < prec) return PE_OK;
        pe = parse_postfix_unary_op(p, op, ex, *ex);
        if (pe) return pe;
    }
    // parse arbitrarily many binary operators
    while (true) {
        op = token_to_binary_op(t);
        if (op == OP_NOOP) break;
        if (op_precedence[op] < prec) return PE_OK;
        pe = parse_binary_op(p, op, ex, *ex);
        if (pe) return pe;
        PEEK(p, t);
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
        pe = parse_value_expr(p, ex);
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
    return parse_expression_of_prec(p, ex, PREC_BASELINE);
}
void report_missing_semicolon(parser* p, ureg start, ureg end)
{
    token* t = lx_aquire(&p->lx);
    parser_error_2a(
        p, "missing semicolon ", t->start, t->end,
        "expected ';' to terminate the statement", start, end,
        "statement started here");
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
static inline parse_error parse_delimited_open_scope(
    parser* p, open_scope* osc, token_kind delimiter_1, token_kind delimiter_2)
{
    if (push_bpd(p, (ast_node*)osc, &osc->scope.body)) return PE_FATAL;

    void* requires_list_start =
        list_builder_start_blocklist(&p->lx.tc->list_builder);
    void** element_list_start = list_builder_start(&p->lx.tc->list_builder2);
    token* t;
    t = lx_peek(&p->lx);
    if (!t) {
        if (pop_bpd(p, PE_LX_ERROR)) return PE_FATAL;
        return PE_LX_ERROR;
    }
    ureg start = t->start;
    parse_error pe;
    ast_node* target;
    while (t->kind != delimiter_1 && t->kind != delimiter_2) {
        pe = parse_statement(p, &target);
        if (pe) {
            if (pe == PE_NO_STMT) {
                pe = PE_OK;
                t = lx_peek(&p->lx);
                if (t) continue;
                pe = PE_LX_ERROR;
            }
            break;
        }
        pe = handle_semicolon_after_statement(p, target);
        if (pe) break;
        if (list_builder_add(&p->lx.tc->list_builder2, target)) {
            pe = PE_FATAL;
            break;
        }
        t = lx_peek(&p->lx);
        if (!t) {
            pe = PE_LX_ERROR;
            break;
        }
    }
    osc->scope.body.elements = (ast_node**)list_builder_pop_list_zt(
        &p->lx.tc->list_builder2, element_list_start, &p->lx.tc->permmem);
    osc->requires = (file_require*)list_builder_pop_block_list_zt(
        &p->lx.tc->list_builder, requires_list_start, &p->lx.tc->permmem);
    if (pop_bpd(p, pe)) return PE_FATAL;
    if (!osc->scope.body.elements) return PE_FATAL;
    if (!osc->requires) return PE_FATAL;
    src_range_large srl;
    srl.start = start;
    srl.end = t->end;
    srl.file = p->lx.file;
    osc->scope.symbol.node.srange = src_range_large_pack(p->lx.tc, &srl);
    if (osc->scope.symbol.node.srange == SRC_RANGE_INVALID) return PE_FATAL;
    if (!osc->requires) return PE_FATAL;
    return pe;
}
parse_error parse_eof_delimited_open_scope(parser* p, open_scope* osc)
{
    return parse_delimited_open_scope(p, osc, TK_EOF, TK_EOF);
}
parse_error parser_parse_file(parser* p, job_parse* j)
{
    int r = lx_open_file(&p->lx, j->file);
    if (r) {
        if (j->requiring_file != NULL) {
            src_range_large srl;
            src_range_unpack(j->requiring_srange, &srl);
            error_log_report_annotated(
                &p->lx.tc->error_log, ES_TOKENIZER, false,
                "required file doesn't exist", j->requiring_file, srl.start,
                srl.end, "required here");
        }
        else {
            char* file_path = tmalloc(src_file_get_path_len(j->file) + 1);
            if (!file_path) return PE_FATAL;
            src_file_write_path(j->file, file_path);
            char* msg = error_log_cat_strings_3(
                &p->lx.tc->error_log, "the requirested file \"", file_path,
                "\" doesn't exist");
            tfree(file_path);
            error_log_report_critical_failiure(&p->lx.tc->error_log, msg);
        }
        return PE_LX_ERROR;
    }
    p->current_module = TAUC.mdg.root_node;
    j->file->root.oscope.scope.symbol.name = TAUC.mdg.root_node->name;
    ast_node_init((ast_node*)&j->file->root, OSC_EXTEND);
    parse_error pe = parse_eof_delimited_open_scope(p, &j->file->root.oscope);
    lx_close_file(&p->lx);
    if (src_file_done_parsing(j->file, p->lx.tc)) pe = PE_FATAL;
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
        &p->lx.tc->error_log, "redundant ", spec, " specifier");
    if (!msg) return ERR;
    parser_error_1a(p, "redundant access modifiers specified", start, end, msg);
    return OK;
}
static inline parse_error ast_node_flags_from_kw_set_access_mod(
    parser* p, ast_node_flags* f, access_modifier am, ureg start, ureg end)
{
    access_modifier old_am = ast_node_flags_get_access_mod(*f);
    if (old_am != AM_UNSPECIFIED) {
        if (old_am == am) {
            report_redundant_specifier(
                p, access_modifier_string(am), start, end);
        }
        else {
            const char* msgstrs[5];
            msgstrs[0] = "'";
            msgstrs[1] = access_modifier_string(am);
            msgstrs[2] = "' conflicts with previous '";
            msgstrs[3] = access_modifier_string(old_am);
            msgstrs[4] = "'";
            char* msg = error_log_cat_strings(&p->lx.tc->error_log, 4, msgstrs);
            if (!msg) return PE_FATAL;
            error_log_report_annotated(
                &p->lx.tc->error_log, ES_PARSER, false,
                "conflicting access modifiers specified", p->lx.file, start,
                end, msg);
        }
        return PE_ERROR;
    }
    ast_node_flags_set_access_mod(f, am);
    return PE_OK;
}
parse_error parse_var_decl(
    parser* p, ast_node_flags flags, ureg start, ureg flags_end, ast_node** n)
{
    token* t = lx_aquire(&p->lx);
    ureg end = t->end;
    string ident = t->str;
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
                "expected expression after '='", start, eq_end,
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
                "expected type or '='", start, col_end, "begin of declaration");
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
                    "expected expression", start, eq_end,
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
        ast_node_init((ast_node*)v, SYM_VAR_INITIALIZED);
    }
    else {
        v = alloc_perm(p, sizeof(sym_var));
        if (!v) return PE_FATAL;
        ast_node_init((ast_node*)v, SYM_VAR);
    }
    v->type = type;
    v->symbol.name = alloc_string_perm(p, ident);
    if (!v->symbol.name) return PE_FATAL;
    v->symbol.node.flags = flags;
    *n = (ast_node*)v;
    if (sym_fill_srange(p, (symbol*)v, start, end)) return PE_FATAL;
    curr_scope_add_decls(p, ast_node_flags_get_access_mod(flags), 1);
    return PE_OK;
}
parse_error parse_param_list(
    parser* p, symbol* parent, sym_param** tgt, ureg* param_count, bool generic,
    ureg ctx_start, ureg ctx_end, char* msg)
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
    void** param_list = list_builder_start_blocklist(&p->lx.tc->list_builder2);
    parse_error pe;
    do {
        sym_param param;
        pe = parse_param_decl(p, &param, ctx_start, ctx_end, msg);
        if (pe) break;
        list_builder_add_block(
            &p->lx.tc->list_builder2, &param, sizeof(sym_param));
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
                &p->lx.tc->error_log, ES_PARSER, false, e1, p->lx.file,
                t->start, t->end, e2, p->lx.file, ctx_start, ctx_end, msg);
            pe = PE_ERROR;
            break;
        }
    } while (t->kind != end_tok);
    if (pe) {
        list_builder_drop_list(&p->lx.tc->list_builder2, param_list);
        return pe;
    }
    lx_void(&p->lx);
    *tgt = (sym_param*)list_builder_pop_block_list(
        &p->lx.tc->list_builder2, param_list, &p->lx.tc->permmem, param_count,
        0, 0);
    *param_count = *param_count / sizeof(sym_param);
    return PE_OK;
}
parse_error parse_func_decl(
    parser* p, ast_node_flags flags, ureg start, ureg flags_end, ast_node** n)
{
    lx_void(&p->lx);
    token* t;
    parse_error pe;
    PEEK(p, t);
    if (t->kind != TK_IDENTIFIER) {
        parser_error_2a(
            p, "invalid function declaration syntax", t->start, t->end,
            "expected function identifier", start, t->end,
            "in this function declaration");
        return PE_ERROR;
    }
    ureg decl_end = t->end;
    char* name = alloc_string_perm(p, t->str);
    if (!name) return PE_FATAL;
    lx_void(&p->lx);
    PEEK(p, t);
    symbol* f;
    sc_func_generic* fng = NULL;
    sc_func* fnp;
    if (t->kind == TK_BRACKET_OPEN) {
        fng = alloc_perm(p, sizeof(sc_func_generic));
        if (!fng) return PE_FATAL;
        f = (symbol*)fng;
        lx_void(&p->lx);
        pe = parse_param_list(
            p, f, &fng->generic_params, &fng->generic_param_count, true, start,
            decl_end, "in this function declaration");
        if (pe) return pe;
        PEEK(p, t);
    }
    else {
        fnp = alloc_perm(p, sizeof(sc_func));
        if (!fnp) return PE_FATAL;
        f = (symbol*)fnp;
    }
    f->name = name;
    ast_node_init((ast_node*)f, fng ? SC_FUNC_GENERIC : SC_FUNC);
    pe = sym_fill_srange(p, f, start, decl_end);
    if (pe) return pe;
    if (t->kind != TK_PAREN_OPEN) {
        parser_error_2a(
            p, "invalid function declaration syntax", t->start, t->end,
            "expected '(' to start parameter list", start, decl_end,
            "in this function declaration");
        return PE_ERROR;
    }
    lx_void(&p->lx);
    pe = parse_param_list(
        p, f, (fng ? &fng->params : &fnp->params),
        (fng ? &fng->param_count : &fnp->param_count), false, start, decl_end,
        "in this function declaration");
    if (pe) return pe;
    *n = (ast_node*)f;
    curr_scope_add_decls(p, ast_node_flags_get_access_mod(flags), 1);
    PEEK(p, t);
    if (t->kind == TK_ARROW) {
        lx_void(&p->lx);
        ast_node* ret_type;
        pe = parse_expression(p, &ret_type);
        if (pe == PE_EOEX) {
            parser_error_2a(
                p, "unexpected end of expression", t->start, t->end,
                "expected function return type", start, decl_end,
                "in this function declaration");
            return PE_ERROR;
        }
        if (pe) return pe;
        if (fng) {
            fng->return_type = ret_type;
        }
        else {
            fnp->return_type = ret_type;
        }
    }
    else {
        if (fng) {
            fng->return_type = NULL;
        }
        else {
            fnp->return_type = NULL;
        }
    }
    return parse_scope_body(
        p, (scope*)f,
        fng ? fng->generic_param_count + fng->param_count : fnp->param_count);
}
parse_error parse_struct_decl(
    parser* p, ast_node_flags flags, ureg start, ureg flags_end, ast_node** n)
{
    lx_void(&p->lx);
    token* t;
    parse_error pe;
    PEEK(p, t);
    if (t->kind != TK_IDENTIFIER) {
        parser_error_2a(
            p, "invalid struct declaration syntax", t->start, t->end,
            "expected struct identifier", start, t->end,
            "in this struct declaration");
        return PE_ERROR;
    }
    ureg decl_end = t->end;
    char* name = alloc_string_perm(p, t->str);
    if (!name) return PE_FATAL;
    lx_void(&p->lx);
    PEEK(p, t);
    symbol* s;
    sc_struct_generic* sg = NULL;
    sc_struct* sp;
    if (t->kind == TK_BRACKET_OPEN) {
        sg = alloc_perm(p, sizeof(sc_struct_generic));
        if (!sg) return PE_FATAL;
        s = (symbol*)sg;
        lx_void(&p->lx);
        pe = parse_param_list(
            p, s, &sg->generic_params, &sg->generic_param_count, true, start,
            decl_end, "in this struct declaration");
        if (pe) return pe;
        PEEK(p, t);
    }
    else {
        sp = alloc_perm(p, sizeof(sc_struct));
        if (!sp) return PE_FATAL;
        s = (symbol*)sp;
    }
    s->name = name;
    pe = sym_fill_srange(p, s, start, decl_end);
    if (pe) return pe;
    ast_node_init_with_flags(
        (ast_node*)s, sg ? SC_STRUCT_GENERIC : SC_STRUCT, flags);
    *n = (ast_node*)s;
    curr_scope_add_decls(p, ast_node_flags_get_access_mod(flags), 1);
    return parse_scope_body(p, (scope*)s, sg ? sg->generic_param_count : 0);
}
parse_error check_if_first_stmt(
    parser* p, ast_node** tgt, ureg start, ureg end, bool extend)
{
    // DEBUG
    return PE_OK;
    // TODO: use extend bool to be more precise than "scope" in the err msg
    scope* curr_scope = (scope*)get_bpd(p)->node;
    if (curr_scope != &p->lx.file->root.oscope.scope) {
        parser_error_1a_pc(
            p, "block free scope statement not allowed here", start, end,
            "this statement type is only allowed at file scope");
        return PE_ERROR;
    }
    // set own target to null so the children list will be null terminated
    *tgt = NULL;
    ast_node* last_culprit = NULL;
    for (ast_node** n = curr_scope->body.elements; *n; n++) {
        if ((**n).kind != SYM_IMPORT_GROUP && (**n).kind != SYM_IMPORT_MODULE &&
            (**n).kind != SYM_IMPORT_SYMBOL) {
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
parse_error parse_module_decl(
    parser* p, ast_node_flags flags, ureg start, ureg flags_end, ast_node** n)
{
    lx_void(&p->lx);
    token *t, *t2;
    parse_error pe;
    PEEK(p, t);
    if (t->kind != TK_IDENTIFIER) {
        parser_error_2a(
            p, "invalid module declaration syntax", t->start, t->end,
            "expected module identifier", start, t->end,
            "in this module declaration");
        return PE_ERROR;
    }
    ureg decl_end = t->end;

    t2 = lx_peek_2nd(&p->lx);
    if (!t2) return PE_FATAL;
    open_scope* md;
    osc_module_generic* mod_gen = NULL;
    if (t2->kind == TK_BRACKET_OPEN) {
        mod_gen = alloc_perm(p, sizeof(osc_module_generic));
        md = (open_scope*)mod_gen;
        if (!md) return PE_FATAL;
        lx_void_n(&p->lx, 2);
        pe = parse_param_list(
            p, (symbol*)md, &mod_gen->generic_params,
            &mod_gen->generic_param_count, true, start, decl_end,
            "in this module declaration");
        if (pe) return pe;
        PEEK(p, t);
    }
    else {
        md = alloc_perm(p, sizeof(osc_module));
        if (!md) return PE_FATAL;
        lx_void(&p->lx);
    }
    mdg_node* mdgn =
        mdg_add_open_scope(&TAUC.mdg, p->current_module, md, t->str);
    if (mdgn == NULL) return PE_FATAL;
    ast_node_init_with_flags(
        (ast_node*)md, mod_gen ? OSC_MODULE_GENERIC : OSC_MODULE, flags);
    md->scope.symbol.node.srange =
        src_range_pack(p->lx.tc, start, decl_end, p->lx.file);
    if (md->scope.symbol.node.srange == SRC_RANGE_INVALID) return PE_FATAL;
    PEEK(p, t);
    mdg_node* parent = p->current_module;
    p->current_module = mdgn;
    if (t->kind == TK_SEMICOLON) {
        pe = check_if_first_stmt(p, n, start, t->end, false);
        if (!pe) {
            lx_consume(&p->lx);
            pe = parse_delimited_open_scope(p, md, TK_EOF, TK_BRACE_CLOSE);
        }
    }
    else {
        pe = parse_open_scope_body(
            p, md, mdgn, mod_gen ? mod_gen->generic_param_count : 0);
    }
    if (pe) {
        p->current_module = parent;
        return pe;
    }
    p->current_module = parent;
    if (*(void**)md->requires == NULL) {
        if (mdg_node_parsed(&TAUC.mdg, mdgn, p->lx.tc)) return PE_FATAL;
    }
    *n = (ast_node*)md;
    curr_scope_add_decls(p, ast_node_flags_get_access_mod(flags), 1);
    // return PE_NO_STMT;
    return PE_OK; // DEBUG
}
parse_error parse_extend_decl(
    parser* p, ast_node_flags flags, ureg start, ureg flags_end, ast_node** n)
{
    lx_void(&p->lx);
    token *t, *t2;
    parse_error pe;
    PEEK(p, t);
    if (t->kind != TK_IDENTIFIER) {
        parser_error_2a(
            p, "invalid extend declaration syntax", t->start, t->end,
            "expected extend identifier", start, t->end,
            "in this extend declaration");
        return PE_ERROR;
    }
    ureg decl_end = t->end;
    t2 = lx_peek_2nd(&p->lx);
    if (!t2) return PE_LX_ERROR;
    open_scope* ex;
    osc_extend_generic* eg = NULL;
    if (t2->kind == TK_BRACKET_OPEN) {
        eg = alloc_perm(p, sizeof(osc_extend_generic));
        if (!eg) return PE_FATAL;
        ex = (open_scope*)eg;
        lx_void_n(&p->lx, 2);
        pe = parse_param_list(
            p, (symbol*)ex, &eg->generic_params, &eg->generic_param_count, true,
            start, decl_end, "in this extend declaration");
        if (pe) return pe;
    }
    else {
        ex = alloc_perm(p, sizeof(osc_extend));
        if (!ex) return PE_FATAL;
        lx_void(&p->lx);
    }
    mdg_node* mdgn =
        mdg_add_open_scope(&TAUC.mdg, p->current_module, ex, t->str);
    if (mdgn == NULL) return PE_FATAL;
    ex->scope.symbol.node.srange =
        src_range_pack(p->lx.tc, start, decl_end, p->lx.file);
    if (ex->scope.symbol.node.srange == SRC_RANGE_INVALID) return PE_FATAL;
    ast_node_init_with_flags(
        (ast_node*)ex, eg ? OSC_EXTEND_GENERIC : OSC_EXTEND, flags);
    PEEK(p, t);
    mdg_node* parent = p->current_module;
    p->current_module = mdgn;
    if (t->kind == TK_SEMICOLON) {
        pe = check_if_first_stmt(p, n, start, t->end, false);
        if (!pe) {
            lx_consume(&p->lx);
            pe = parse_delimited_open_scope(p, ex, TK_EOF, TK_BRACE_CLOSE);
        }
    }
    else {
        pe = parse_open_scope_body(
            p, ex, mdgn, eg ? eg->generic_param_count : 0);
    }
    if (pe) {
        p->current_module = parent;
        return pe;
    }
    p->current_module = parent;
    *n = (ast_node*)ex;
    curr_scope_add_decls(p, ast_node_flags_get_access_mod(flags), 1);
    return PE_OK; // DEBUG
    // return PE_NO_STMT;
}
parse_error parse_trait_decl(
    parser* p, ast_node_flags flags, ureg start, ureg flags_end, ast_node** n)
{
    lx_void(&p->lx);
    token* t;
    parse_error pe;
    PEEK(p, t);
    if (t->kind != TK_IDENTIFIER) {
        parser_error_2a(
            p, "invalid trait declaration syntax", t->start, t->end,
            "expected trait identifier", start, t->end,
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
            p, (symbol*)tr, &tg->generic_params, &tg->generic_param_count, true,
            start, decl_end, "in this trait declaration");
        if (pe) return pe;
        PEEK(p, t);
    }
    else {
        tr = alloc_perm(p, sizeof(sc_trait));
        if (!tr) return PE_FATAL;
    }
    tr->symbol.name = name;
    pe = sym_fill_srange(p, (symbol*)tr, start, decl_end);
    if (pe) return pe;
    ast_node_init_with_flags(
        (ast_node*)tr, tg ? SC_TRAIT_GENERIC : SC_TRAIT, flags);
    *n = (ast_node*)tr;
    curr_scope_add_decls(p, ast_node_flags_get_access_mod(flags), 1);
    return parse_scope_body(p, tr, tg ? tg->generic_param_count : 0);
}
bool ast_elem_supports_exprs(ast_elem* n)
{
    return !ast_elem_is_open_scope(n);
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
    if (had_colon) ast_node_flags_set_compound_decl(&ca->node.flags);
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
                    curr_scope_add_decls(p, AM_UNSPECIFIED, decl_count);
                    return parse_compound_assignment_after_equals(
                        p, t_start, t->end, elems, elem_count, tgt, true);
                }
            }
            if (t->kind == TK_EQUALS) {
                lx_void(&p->lx);
                turn_ident_nodes_to_exprs(elems, elem_count);
                curr_scope_add_decls(p, AM_UNSPECIFIED, decl_count);
                return parse_compound_assignment_after_equals(
                    p, t_start, t->end, elems, elem_count, tgt, false);
            }
            else {
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
        }
        // EMSG: if there is an invalid token following, this will lead to a
        // "missing semicolon for expression" error down the line which is
        // not
        // ideal
        pe = parse_expression_of_prec_post_value(p, &ex, PREC_BASELINE);
    }
    else {
        operator_kind op = token_to_prefix_unary_op(t);
        if (op != OP_NOOP) {
            pe = parse_prefix_unary_op(p, op, &ex);
            if (pe) return pe;
            pe = parse_expression_of_prec_post_value(p, &ex, PREC_BASELINE);
        }
        else {
            pe = parse_value_expr(p, &ex);
            if (pe) return pe;
            if (!ast_node_may_drop_semicolon(ex)) {
                pe = parse_expression_of_prec_post_value(p, &ex, PREC_BASELINE);
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
    parser* p, ast_node_flags flags, ureg start, ureg kw_end, mdg_node* parent,
    ureg* decl_cnt, symbol** tgt);
parse_error parse_using(
    parser* p, ast_node_flags flags, ureg start, ureg flags_end, ast_node** tgt)
{
    token* t = lx_aquire(&p->lx);
    ureg end = t->end;
    lx_void(&p->lx);
    PEEK(p, t);
    if (t->kind == TK_IDENTIFIER) {
        token* t2 = lx_peek_2nd(&p->lx);
        if (!t2) return PE_LX_ERROR;
        if (t2->kind == TK_EQUALS) {
            sym_named_using* nu = alloc_perm(p, sizeof(sym_named_using));
            if (!nu) return PE_FATAL;
            ast_node_init_with_flags((ast_node*)nu, SYM_NAMED_USING, flags);
            nu->symbol.name = alloc_string_perm(p, t->str);
            if (!nu->symbol.name) return PE_FATAL;
            lx_void_n(&p->lx, 2);
            parse_error pe = parse_expression(p, &nu->target);
            if (pe == PE_EOEX) {
                parser_error_2a(
                    p, "unexpected token", t->start, t->end,
                    "expected expression", start, end,
                    "in this named using statement");
                return PE_ERROR;
            }
            if (pe) return pe;
            if (ast_node_fill_srange(
                    p, (ast_node*)nu, start,
                    src_range_get_end(nu->target->srange)))
                return PE_FATAL;
            *tgt = (ast_node*)nu;
            curr_scope_add_decls(p, ast_node_flags_get_access_mod(flags), 1);
            return PE_OK;
        }
    }
    stmt_using* u = alloc_perm(p, sizeof(stmt_using));
    if (!u) return PE_FATAL;
    ast_node_init_with_flags((ast_node*)u, STMT_USING, flags);
    parse_error pe = parse_expression(p, &u->target);
    if (pe == PE_EOEX) {
        parser_error_2a(
            p, "unexpected token", t->start, t->end, "expected expression",
            start, end, "in this using statement");
        return PE_ERROR;
    }
    if (ast_node_fill_srange(
            p, (ast_node*)u, start, src_range_get_end(u->target->srange)))
        return PE_FATAL;
    *tgt = (ast_node*)u;
    curr_scope_add_usings(p, ast_node_flags_get_access_mod(flags), 1);
    return pe;
}
parse_error parse_braced_imports(
    parser* p, ast_node_flags flags, ureg start, ureg kw_end, mdg_node* parent,
    ureg* end, ureg* decl_cnt, symbol** tgt)
{
    lx_void(&p->lx);
    while (true) {
        parse_error pe = parse_import_with_parent(
            p, flags, start, kw_end, parent, decl_cnt, tgt);
        if (pe) return pe;
        tgt = &(**tgt).next;
        token* t;
        PEEK(p, t);
        if (t->kind == TK_COMMA || t->kind == TK_BRACE_CLOSE) {
            if (t->kind == TK_COMMA) {
                lx_void(&p->lx);
            }
            if (t->kind == TK_BRACE_CLOSE) {
                *end = t->end;
                lx_void(&p->lx);
                *tgt = NULL;
                return PE_OK;
            }
        }
        else {
            return parser_error_2a(
                p, "invalid import syntax", t->start, t->end, "expected , or }",
                start, kw_end, "in this import statement");
        }
    }
}
parse_error parse_symbol_imports(
    parser* p, ast_node_flags flags, ureg start, ureg kw_end, ureg* end,
    ureg* decl_cnt, symbol** tgt)
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
                start, kw_end, "in this import statement");
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
                    "expected symbol identifier", start, kw_end,
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
            ast_node_init_with_flags((ast_node*)im, SYM_IMPORT_SYMBOL, flags);
            ast_node_fill_srange(p, (ast_node*)im, symstart, symend);
            im->symbol.name = id1_str;
            im->target.name = symname;
            *tgt = (symbol*)im;
            tgt = &im->symbol.next;
            *decl_cnt = *decl_cnt + 1;
            // fallthrough to allow trailing comma
            if (t->kind == TK_PAREN_CLOSE) {
                *end = t->end;
                lx_void(&p->lx);
                *tgt = NULL;
                return PE_OK;
            }
        }
        else {
            return parser_error_2a(
                p, "invalid import syntax", t->start, t->end, "expected , or )",
                start, kw_end, "in this import statement");
        }
    }
}
parse_error parse_import_with_parent(
    parser* p, ast_node_flags flags, ureg start, ureg kw_end, mdg_node* parent,
    ureg* decl_cnt, symbol** tgt)
{
    token *t, *t2;
    PEEK(p, t);
    ureg istart = (parent == TAUC.mdg.root_node) ? start : t->start;
    ureg end;
    PEEK_SND(p, t2);
    char* name = NULL;
    if (t->kind == TK_IDENTIFIER && t2->kind == TK_EQUALS) {
        name = alloc_string_perm(p, t->str);
        if (!name) return PE_FATAL;
        lx_void_n(&p->lx, 2);
        PEEK(p, t);
    }
    if (t->kind == TK_KW_SELF && TAUC.mdg.root_node == parent) {
        parent = p->current_module;
        ast_node_flags_set_relative_import(&flags);
        if (t2->kind == TK_DOUBLE_COLON) {
            lx_void_n(&p->lx, 2);
            PEEK(p, t);
        }
        else {
            return parser_error_2a(
                p, "invalid import syntax", t->start, t->end, "expected ::",
                start, kw_end, "in this import statement");
        }
    }
    bool has_ident = false;
    while (t->kind == TK_IDENTIFIER) {
        has_ident = true;
        parent = mdg_get_node(&TAUC.mdg, parent, t->str);
        // if the node doesn't exist it gets created here,
        // we will find out (and report) once all required files are parsed if
        // it is actually missing. NULL just means allocation failiure
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
            ast_node_init_with_flags((ast_node*)im, SYM_IMPORT_MODULE, flags);
            ast_node_fill_srange(p, (ast_node*)im, istart, end);
            im->target.mdg_node = parent;
            if (name) {
                im->symbol.name = name;
                curr_scope_add_decls(
                    p, ast_node_flags_get_access_mod(flags), 1);
            }
            else {
                im->symbol.name = parent->name;
            }
            if (mdg_node_add_dependency(p->current_module, parent, p->lx.tc)) {
                return PE_FATAL;
            }
            *tgt = (symbol*)im;
            *decl_cnt = *decl_cnt + 1;
            return PE_OK;
        }
    }
    // TODO: also select this branch if we want unnamed groups
    // for better ast printing
    if (t->kind == TK_PAREN_OPEN || t->kind == TK_BRACE_OPEN) {
        sym_import_group* ig;
        // if (name) { // we even create unnamed groups for better printing
        ig = alloc_perm(p, sizeof(sym_import_group));
        if (!ig) return PE_FATAL;
        ast_node_init_with_flags((ast_node*)ig, SYM_IMPORT_GROUP, flags);
        ig->parent.mdg_node = parent;
        ig->symbol.name = name;
        *tgt = (symbol*)ig;
        tgt = &ig->children.symbols;
        // }
        if (has_ident || name || t->kind != TK_BRACE_OPEN) {
            ureg ndecl_cnt = 0;
            if (name) {
                *decl_cnt = *decl_cnt + 1;
                decl_cnt = &ndecl_cnt;
            }
            parse_error re;
            if (t->kind == TK_PAREN_OPEN) {
                re = parse_symbol_imports(
                    p, flags, start, kw_end, &end, decl_cnt, tgt);
            }
            else {
                re = parse_braced_imports(
                    p, flags, start, kw_end, parent, &end, decl_cnt, tgt);
            }
            if (re) return re;
            if (ig) {
                ast_node_fill_srange(p, (ast_node*)ig, start, end);
            }
            if (name) {
                symbol_table* st;
                if (symbol_table_init(
                        &st, ndecl_cnt, 0, false, (ast_elem*)ig)) {
                    return RE_FATAL;
                }
                resolve_error re = add_import_group_decls(
                    p->lx.tc, p->current_module, p->lx.file, ig, st);
                ig->children.symtab = st;
                if (re) return PE_ERROR;
            }
            return RE_OK;
        }
    }
    char* expected;
    if (parent == TAUC.mdg.root_node)
        expected = "expected identifier";
    else if (has_ident)
        expected = "expected identifier or ( or {";
    else
        expected = "expected identifier or (";
    return parser_error_2a(
        p, "invalid import syntax", t->start, t->end, expected, start, kw_end,
        "in this import statement");
}
parse_error parse_import(
    parser* p, ast_node_flags flags, ureg start, ureg flags_end, ast_node** tgt)
{
    token* t = lx_aquire(&p->lx);
    ureg kw_end = t->end;
    lx_void(&p->lx);
    ureg decl_cnt = 0;
    parse_error pe = parse_import_with_parent(
        p, flags, start, kw_end, TAUC.mdg.root_node, &decl_cnt, (symbol**)tgt);
    if (pe) return pe;
    curr_scope_add_decls(p, ast_node_flags_get_access_mod(flags), decl_cnt);
    return PE_OK;
}
parse_error
parse_require(parser* p, ast_node_flags flags, ureg start, ureg flags_end)
{
    token* t = lx_aquire(&p->lx);
    parse_error pe = require_default_flags(p, t, flags, start, flags_end);
    if (pe) return pe;
    ureg end = t->end;
    lx_void(&p->lx);
    if (!ast_elem_is_open_scope((ast_elem*)get_bpd(p)->node)) {
        parser_error_1a_pc(
            p, "invalid scope for require statement", t->start, t->end,
            "require statement only allowed at module scope");
        return PE_ERROR;
    }
    PEEK(p, t);
    if (t->kind != TK_STRING) {
        parser_error_2a(
            p, "unexpected token", t->start, t->end,
            "expected path pattern as string literal", start, end,
            "in this require statement");
        return PE_ERROR;
    }
    src_file* f = file_map_get_file_from_relative_path(
        &TAUC.file_map, p->lx.file->head.parent, t->str);
    lx_void(&p->lx);
    PEEK(p, t);
    if (t->kind != TK_SEMICOLON) {
        report_missing_semicolon(p, start, end);
        return PE_ERROR;
    }
    file_require rq;
    rq.file = f;
    rq.srange = src_range_pack_lines(p->lx.tc, start, t->end);
    if (rq.srange == SRC_RANGE_INVALID) return PE_FATAL;
    end = t->end;
    lx_void(&p->lx);
    rwslock_read(&p->current_module->stage_lock);
    bool needed = (p->current_module->stage != MS_UNNEEDED);
    int r = list_builder_add_block(&p->lx.tc->list_builder, &rq, sizeof(rq));
    rwslock_end_read(&p->current_module->stage_lock);
    if (r) return PE_FATAL;
    if (needed) {
        int r = src_file_require(f, p->lx.file, rq.srange, p->current_module);
        if (r == ERR) return PE_FATAL;
        if (r != SF_ALREADY_PARSED) {
            atomic_ureg_inc(&p->current_module->unparsed_files);
        }
    }
    return PE_NO_STMT;
}
static inline parse_error ast_node_flags_from_kw(
    parser* p, ast_node_flags* f, token_kind kw, ureg start, ureg end)
{
    // TODO: enforce order
    switch (kw) {
        case TK_KW_PRIVATE:
            return ast_node_flags_from_kw_set_access_mod(
                p, f, AM_PRIVATE, start, end);
        case TK_KW_PROTECTED:
            return ast_node_flags_from_kw_set_access_mod(
                p, f, AM_PROTECTED, start, end);
        case TK_KW_PUBLIC:
            return ast_node_flags_from_kw_set_access_mod(
                p, f, AM_PUBLIC, start, end);
        case TK_KW_CONST: {
            if (ast_node_flags_get_const(*f) != false) {
                report_redundant_specifier(
                    p, token_strings[TK_KW_CONST], start, end);
                return PE_ERROR;
            }
            ast_node_flags_set_const(f);
        } break;
        case TK_KW_SEALED: {
            if (ast_node_flags_get_sealed(*f) != false) {
                report_redundant_specifier(
                    p, token_strings[TK_KW_SEALED], start, end);
                return PE_ERROR;
            }
            ast_node_flags_set_sealed(f);
        } break;
        case TK_KW_VIRTUAL: {
            if (ast_node_flags_get_virtual(*f) != false) {
                report_redundant_specifier(
                    p, token_strings[TK_KW_VIRTUAL], start, end);
                return PE_ERROR;
            }
            ast_node_flags_set_virtual(f);
        } break;
        case TK_KW_STATIC: {
            if (ast_node_flags_get_static(*f) != false) {
                report_redundant_specifier(
                    p, token_strings[TK_KW_STATIC], start, end);
                return PE_ERROR;
            }
            ast_node_flags_set_static(f);
        } break;
        default: {
            return PE_EOEX;
        } break;
    }
    return PE_OK;
}
static inline parse_error parse_pp_stmt(
    parser* p, ast_node_flags flags, ureg start, ureg flags_end, ast_node** tgt)
{
    token* t = lx_aquire(&p->lx);
    parse_error pe = require_default_flags(p, t, flags, start, flags_end);
    lx_void(&p->lx);
    if (pe) return pe;
    expr_pp* sp = alloc_perm(p, sizeof(expr_pp));
    if (!sp) return PE_FATAL;
    ast_node_init(&sp->node, EXPR_PP);
    if (push_bpd_pp(p, (ast_node*)sp)) return PE_FATAL;
    pe = parse_statement(p, &sp->pp_expr);
    if (pop_bpd_pp(p, pe)) return PE_FATAL;
    if (pe) return pe;
    pe = ast_node_fill_srange(
        p, (ast_node*)sp, start, src_range_get_end(sp->pp_expr->srange));
    *tgt = (ast_node*)sp;
    return pe;
}
parse_error parse_statement(parser* p, ast_node** tgt)
{
    parse_error pe;
    ast_node_flags flags = AST_NODE_FLAGS_DEFAULT;
    token* t;
    PEEK(p, t);
    ureg start = t->start;
    ureg flags_end = t->start;

    while (true) {
        pe = ast_node_flags_from_kw(p, &flags, t->kind, start, t->end);
        if (pe == PE_OK) {
            flags_end = t->end;
            lx_void(&p->lx);
            PEEK(p, t);
            continue;
        }
        if (pe != PE_EOEX) return pe;
        switch (t->kind) {
            case TK_KW_FUNC:
                return parse_func_decl(p, flags, start, flags_end, tgt);
            case TK_KW_STRUCT:
                return parse_struct_decl(p, flags, start, flags_end, tgt);
            case TK_KW_TRAIT:
                return parse_trait_decl(p, flags, start, flags_end, tgt);
            case TK_KW_MODULE:
                return parse_module_decl(p, flags, start, flags_end, tgt);
            case TK_KW_EXTEND:
                return parse_extend_decl(p, flags, start, flags_end, tgt);
            case TK_KW_USING:
                return parse_using(p, flags, start, flags_end, tgt);
            case TK_KW_REQUIRE:
                return parse_require(p, flags, start, flags_end);
            case TK_KW_IMPORT:
                return parse_import(p, flags, start, flags_end, tgt);
            case TK_HASH: return parse_pp_stmt(p, flags, start, flags_end, tgt);
            case TK_IDENTIFIER: {
                token* t2 = lx_peek_2nd(&p->lx);
                if (!t2) return PE_LX_ERROR;
                if (t2->kind == TK_COLON) {
                    return parse_var_decl(p, flags, start, flags_end, tgt);
                }
                if (curr_parent_supports_exprs(p)) {
                    pe = require_default_flags(p, t, flags, start, flags_end);
                    if (pe) return pe;
                    return parse_expr_stmt(p, tgt);
                }
                t = lx_peek_2nd(&p->lx);
                parser_error_1a(
                    p, "unexpected token in statement", t->start, t->end,
                    "expected ':' to initiate a declaration");
                return PE_ERROR;
            }
            default: {
                if (curr_parent_supports_exprs(p)) {
                    pe = require_default_flags(p, t, flags, start, flags_end);
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
                parser_error_1a_pc(
                    p, "unexpected token", t->start, t->end,
                    "expected a declaration");
                return PE_ERROR;
            }
        }
    }
}
parse_error parse_brace_delimited_body(
    parser* p, body* b, ast_node* parent, ureg param_count)
{
    token* t = lx_aquire(&p->lx);
    ureg bstart = t->start;
    ureg bend = t->end;
    lx_void(&p->lx);
    parse_error pe = PE_OK;
    PEEK(p, t);
    void** elements_list_start = list_builder_start(&p->lx.tc->list_builder2);
    ast_node* target;
    if (push_bpd(p, parent, b)) return PE_FATAL;
    curr_scope_add_decls(p, AM_UNSPECIFIED, param_count);
    while (t->kind != TK_BRACE_CLOSE) {
        if (t->kind != TK_EOF) {
            pe = parse_statement(p, &target);
            if (pe) {
                if (pe == PE_NO_STMT) {
                    pe = PE_OK;
                    t = lx_peek(&p->lx);
                    if (t) continue;
                    pe = PE_LX_ERROR;
                }
                break;
            }
            pe = handle_semicolon_after_statement(p, target);
            if (pe) break;
            if (list_builder_add(&p->lx.tc->list_builder2, target)) {
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
                p, "unterminated scope", t->start, t->end,
                "reached EOF before scope was closed", bstart, bend,
                "scope starts here");
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
        &p->lx.tc->list_builder2, elements_list_start, &p->lx.tc->permmem);
    if (pop_bpd(p, pe)) return PE_FATAL;
    if (!b->elements) return PE_FATAL;
    return pe;
}
parse_error parse_scope_body(parser* p, scope* s, ureg param_count)
{
    token* t;
    PEEK(p, t);
    if (t->kind != TK_BRACE_OPEN) {
        parser_error_2a(
            p, "expected scope body", t->start, t->end,
            "expected '{' to begin scope",
            src_range_get_start(s->symbol.node.srange),
            src_range_get_end(s->symbol.node.srange),
            get_context_msg(p, (ast_node*)s));
        return PE_ERROR;
    }
    return parse_brace_delimited_body(p, &s->body, (ast_node*)s, param_count);
}
parse_error
parse_open_scope_body(parser* p, open_scope* s, mdg_node* m, ureg param_count)
{
    mdg_node* parent = p->current_module;
    p->current_module = m;
    s->requires =
        (file_require*)list_builder_start_blocklist(&p->lx.tc->list_builder);
    parse_error pe = parse_scope_body(p, (scope*)s, 0);
    s->requires = (file_require*)list_builder_pop_block_list_zt(
        &p->lx.tc->list_builder, s->requires, &p->lx.tc->permmem);
    p->current_module = parent;
    return pe;
}
parse_error
parse_braced_namable_body(parser* p, ast_node* parent, body* b, char** name)
{
    token* t;
    PEEK(p, t);
    if (t->kind == TK_IDENTIFIER) {
        token* t2 = lx_peek_2nd(&p->lx);
        if (!t2) return PE_LX_ERROR;
        if (t2->kind == TK_AT) {
            *name = alloc_string_perm(p, t->str);
            if (!*name) return PE_FATAL;
            lx_void_n(&p->lx, 2);
            PEEK(p, t);
        }
    }
    else {
        *name = NULL;
    }
    if (t->kind == TK_BRACE_OPEN) {
        return parse_brace_delimited_body(p, b, (ast_node*)parent, 0);
    }
    else {
        src_range_large srl;
        src_range_unpack(parent->srange, &srl);
        parser_error_2a(
            p, "expected block", t->start, t->end,
            "expected open brace or label to begin block", srl.start, srl.end,
            get_context_msg(p, (ast_node*)parent));
        return PE_ERROR;
    }
    assert(false);
}
