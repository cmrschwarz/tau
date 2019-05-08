#include "parser.h"
#include "error_log.h"
#include "file_map.h"
#include "print_ast.h"
#include "tauc.h"
#include "tokenizer.h"
#include "utils/math_utils.h"
#include "utils/panic.h"
#include "utils/zero.h"
#include <stddef.h>

#define PEEK(p, t)                                                             \
    do {                                                                       \
        t = tk_peek(&(p)->tk);                                                 \
        if (!t) return PE_TK_ERROR;                                            \
    } while (false)

bool body_supports_exprs(ast_node_type pt);
parse_error parse_statement(parser* p, stmt** tgt);
parse_error parse_scope_body(parser* p, scope* s);
parse_error parse_open_scope_body(parser* p, open_scope* s, mdg_node* n);
parse_error parse_body(parser* p, body* b);
parse_error parse_expression(parser* p, expr** ex);
parse_error parse_expression_of_prec(parser* p, expr** ex, ureg prec);
parse_error parse_brace_delimited_body(parser* p, body* b);

static const unsigned char op_precedence[] = {
        [OP_POST_INCREMENT] = 15,
        [OP_POST_DECREMENT] = 15,
        [OP_CALL] = 15,
        [OP_ACCESS] = 15,
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

static inline bool is_left_associative(ast_node_type t)
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

static inline op_type token_to_binary_op(token* t)
{
    switch (t->type) {
        case TT_PLUS: return OP_ADD;
        case TT_PLUS_EQUALS: return OP_ADD_ASSIGN;

        case TT_MINUS: return OP_SUB;
        case TT_MINUS_EQUALS: return OP_SUB_ASSIGN;

        case TT_STAR: return OP_MUL;
        case TT_STAR_EQUALS: return OP_MUL_ASSIGN;

        case TT_SLASH: return OP_DIV;
        case TT_SLASH_EQUALS: return OP_DIV_ASSIGN;

        case TT_PERCENT: return OP_MOD;
        case TT_PERCENT_EQUALS: return OP_MOD_ASSIGN;

        case TT_DOUBLE_LESS_THAN: return OP_LSHIFT;
        case TT_DOUBLE_LESS_THAN_EQUALS: return OP_LSHIFT_ASSIGN;

        case TT_DOUBLE_GREATER_THAN: return OP_RSHIFT;
        case TT_DOUBLE_GREATER_THAN_EQUALS: return OP_RSHIFT_ASSIGN;

        case TT_LESS_THAN: return OP_LESS_THAN;
        case TT_LESS_THAN_EQUALS: return OP_LESS_THAN_OR_EQUAL;

        case TT_GREATER_THAN: return OP_GREATER_THAN;
        case TT_GREATER_THAN_EQUALS: return OP_GREATER_THAN_OR_EQUAL;

        case TT_EQUALS: return OP_ASSIGN;
        case TT_DOUBLE_EQUALS: return OP_EQUAL;
        case TT_EXCLAMATION_MARK_EQUALS: return OP_UNEQAL;

        case TT_AND: return OP_BITWISE_AND;
        case TT_AND_EQUALS: return OP_BITWISE_AND_ASSIGN;
        case TT_DOUBLE_AND: return OP_AND;

        case TT_CARET: return OP_BITWISE_XOR;
        case TT_CARET_EQUALS: return OP_BITWISE_XOR_ASSIGN;
        case TT_DOUBLE_CARET: return OP_XOR;

        case TT_PIPE: return OP_BITWISE_OR;
        case TT_PIPE_EQUALS: return OP_BITWISE_OR_ASSIGN;
        case TT_DOUBLE_PIPE: return OP_OR;

        case TT_TILDE_EQUALS: return OP_BITWISE_NOT_ASSIGN;
        case TT_DOT: return OP_MEMBER_ACCESS;
        default: return OP_NOOP;
    }
}
bool expr_allowed_to_drop_semicolon(expr* e)
{
    switch (e->type) {
        case EXPR_FOR:
        case EXPR_FOR_EACH:
        case EXPR_WHILE:
        case EXPR_LOOP:
        case EXPR_MATCH:
        case EXPR_IF:
        case EXPR_BLOCK: return true;
        case EXPR_OP_UNARY: {
            switch (e->op_type) {
                case OP_PP:
                    return expr_allowed_to_drop_semicolon(((expr_pp*)e)->child);
                default: return false;
            }
        }
        default: return false;
    }
}
bool stmt_allowed_to_drop_semicolon(stmt* s)
{
    switch (s->type) {
        case SC_FUNC:
        case OSC_MODULE:
        case OSC_MODULE_GENERIC:
        case OSC_EXTEND:
        case OSC_EXTEND_GENERIC:
        case SC_STRUCT:
        case SC_STRUCT_GENERIC:
        case SC_TRAIT:
        case SC_TRAIT_GENERIC: return true;
        case STMT_EXPRESSION:
            return expr_allowed_to_drop_semicolon(((stmt_expr*)s)->expr);
        default: return false;
    }
}
static inline op_type token_to_prefix_unary_op(token* t)
{
    switch (t->type) {
        case TT_MINUS: return OP_UNARY_MINUS;
        case TT_PLUS: return OP_UNARY_PLUS;
        case TT_TILDE: return OP_BITWISE_NOT;
        case TT_EXCLAMATION_MARK: return OP_NOT;
        case TT_STAR: return OP_DEREF;
        case TT_AND: return OP_REF_OF;
        case TT_PERCENT: return OP_POINTER_OF;
        case TT_CARET: return OP_CLOSURE_BY_VALUE;
        case TT_DOLLAR: return OP_RREF_OF;
        case TT_DOUBLE_PLUS: return OP_PRE_INCREMENT;
        case TT_DOUBLE_MINUS: return OP_PRE_DECREMENT;
        case TT_KW_CONST: return OP_CONST;
        case TT_HASH: return OP_PP;
        default: return OP_NOOP;
    }
}
static inline op_type token_to_postfix_unary_op(token* t)
{
    switch (t->type) {
        case TT_DOUBLE_PLUS: return OP_POST_INCREMENT;
        case TT_DOUBLE_MINUS: return OP_POST_DECREMENT;
        case TT_PAREN_OPEN: return OP_CALL;
        case TT_BRACKET_OPEN: return OP_ACCESS;
        default: return OP_NOOP;
    }
}
static inline bool is_kw_valid_label(token_type t)
{
    switch (t) {
        case TT_KW_FOR:
        case TT_KW_LOOP:
        case TT_KW_WHILE:
        case TT_KW_MATCH:
        case TT_KW_DO: return true;
        default: return false;
    }
}
static inline void* alloc_ppool(parser* p, ureg size, pool* pool)
{
    void* mem = pool_alloc(pool, size);
    if (!mem) error_log_report_allocation_failiure(&p->tk.tc->error_log);
    return mem;
}
static inline void* alloc_temp(parser* p, ureg size)
{
    return alloc_ppool(p, size, &p->tk.tc->tempmem);
}
static inline void* alloc_perm(parser* p, ureg size)
{
    return alloc_ppool(p, size, &p->tk.tc->permmem);
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
    return alloc_string_ppool(p, s, &p->tk.tc->tempmem);
}
static inline char* alloc_string_perm(parser* p, string s)
{
    return alloc_string_ppool(p, s, &p->tk.tc->permmem);
}
static inline void
parser_error_1a(parser* p, char* msg, ureg start, ureg end, char* annot)
{
    error_log_report_annotated(
        &p->tk.tc->error_log, ES_PARSER, false, msg, p->tk.file, start, end,
        annot);
}
static inline void parser_error_2a(
    parser* p, char* msg, ureg start, ureg end, char* annot, ureg start2,
    ureg end2, char* annot2)
{
    error_log_report_annotated_twice(
        &p->tk.tc->error_log, ES_PARSER, false, msg, p->tk.file, start, end,
        annot, start2, end2, annot2);
}
static inline void parser_error_3a(
    parser* p, char* msg, ureg start, ureg end, char* annot, ureg start2,
    ureg end2, char* annot2, ureg start3, ureg end3, char* annot3)
{
    error_log_report_annotated_thrice(
        &p->tk.tc->error_log, ES_PARSER, false, msg, p->tk.file, start, end,
        annot, start2, end2, annot2, start3, end3, annot3);
}
char* get_parent_context_msg(parser* p)
{
    if (p->curr_scope == &p->root.oscope.scope) return NULL;
    switch (p->curr_scope->symbol.stmt.type) {
        case SC_FUNC: return "in this function's body";
        case SC_FUNC_GENERIC: return "in this generic function's body";
        case SC_STRUCT: return "in this struct's body";
        case SC_STRUCT_GENERIC: return "in this generic struct's body";
        case SC_TRAIT: return "in this struct's body";
        case SC_TRAIT_GENERIC: return "in this generic struct's body";
        case OSC_MODULE: return "in this module's body";
        case OSC_MODULE_GENERIC: return "in this generic module's body";
        case OSC_EXTEND: return "in this extend statement's body";
        case EXPR_WHILE:
        case EXPR_FOR:
        case EXPR_FOR_EACH:
        case EXPR_LOOP: return "in this loop's body";
        case EXPR_IF: return "in this if expressions's body";
        case OSC_EXTEND_GENERIC:
            return "in this generic extend statement's body";
        case EXPR_LAMBDA: return "in this lambda's body";
        default: panic("unexpected parent context");
    }
    return NULL;
}
static inline void
parser_error_1a_pc(parser* p, char* msg, ureg start, ureg end, char* annot)
{
    char* bpmmsg = get_parent_context_msg(p);
    if (bpmmsg != NULL) {
        src_range_large sr;
        src_range_unpack(p->curr_scope->symbol.stmt.srange, &sr);
        parser_error_2a(p, msg, start, end, annot, sr.start, sr.end, bpmmsg);
    }
    else {
        parser_error_1a(p, msg, start, end, annot);
    }
}
static inline void parser_error_2a_pc(
    parser* p, char* msg, ureg start, ureg end, char* annot, ureg start2,
    ureg end2, char* annot2)
{
    char* bpmmsg = get_parent_context_msg(p);
    if (bpmmsg != NULL) {
        src_range_large sr;
        src_range_unpack(p->curr_scope->symbol.stmt.srange, &sr);
        parser_error_3a(
            p, msg, start, end, annot, start2, end2, annot2, sr.start, sr.end,
            bpmmsg);
    }
    else {
        parser_error_2a(p, msg, start, end, annot, start2, end2, annot2);
    }
}

static inline void parser_error_unexpected_token(
    parser* p, token* t, token_type exp_tt, char* msg, char* ctx,
    ureg ctx_start, ureg ctx_end)
{
    char* expstr = "expected ";
    ureg explen = strlen(expstr);
    ureg toklen = strlen(token_strings[exp_tt]);
    char* ann =
        (char*)error_log_alloc(&p->tk.tc->error_log, explen + toklen + 3);
    if (!ann) return;
    memcpy(ann, expstr, explen);
    ann[explen] = '\'';
    memcpy(ann + explen + 1, token_strings[exp_tt], toklen);
    ann[explen + 1 + toklen] = '\'';
    ann[explen + 1 + toklen + 1] = '\0';
    parser_error_2a(p, msg, t->start, t->end, ann, ctx_start, ctx_end, ctx);
}
static inline void stmt_init(stmt* s, ast_node_type type)
{
    s->eflags = ERR_FLAGS_DEFAULT;
    s->flags = STMT_FLAGS_DEFAULT;
    s->type = type;
}
int parser_init(parser* p, thread_context* tc)
{
    int r = tk_init(&p->tk, tc);
    if (r) return r;
    r = list_builder_init(&p->list_builder, &p->tk.tc->tempmem, 64);
    if (r) {
        tk_fin(&p->tk);
        return r;
    }
    p->root.oscope.scope.symbol.name = NULL;
    p->root.oscope.scope.symbol.stmt.type = OSC_MODULE;
    p->root.oscope.scope.body.children = NULL;
    p->root.oscope.scope.symbol.stmt.next = NULL;
    p->curr_scope = (scope*)&p->root;
    return OK;
}
void parser_fin(parser* p)
{
    tk_fin(&p->tk);
}
static inline parse_error
expr_fill_srange(parser* p, expr* ex, ureg start, ureg end)
{
    ex->srange = src_range_pack_lines(p->tk.tc, start, end);
    if (ex->srange == SRC_RANGE_INVALID) return PE_FATAL;
    return PE_OK;
}

static inline parse_error
stmt_fill_srange(parser* p, stmt* s, ureg start, ureg end)
{
    s->srange = src_range_pack_lines(p->tk.tc, start, end);
    if (s->srange == SRC_RANGE_INVALID) return PE_FATAL;
    return PE_OK;
}
static inline expr* parse_str_value(parser* p, token* t)
{
    ast_node_type ent;
    switch (t->type) {
        case TT_BINARY_LITERAL: ent = EXPR_BINARY_LITERAL; break;
        case TT_LITERAL: ent = EXPR_STRING_LITERAL; break;
        case TT_NUMBER: ent = EXPR_NUMBER; break;
        case TT_STRING: ent = EXPR_IDENTIFIER; break;
        default: return NULL;
    }
    expr_str_value* sv = (expr_str_value*)alloc_perm(p, sizeof(expr_str_value));
    if (!sv) return NULL;
    sv->expr.type = ent;
    sv->value = alloc_string_temp(p, t->str);
    if (!sv->value) return NULL;
    if (expr_fill_srange(p, &sv->expr, t->start, t->end)) return NULL;
    return (expr*)sv;
}
parse_error expr_to_stmt(parser* p, stmt** tgt, expr* e, ureg start, ureg end)
{
    stmt_expr* s = alloc_perm(p, sizeof(stmt_expr));
    if (!s) return PE_FATAL;
    s->stmt.type = STMT_EXPRESSION;
    s->expr = e;
    if (stmt_fill_srange(p, (stmt*)s, start, end)) return PE_FATAL;
    *tgt = &s->stmt;
    return PE_OK;
}
parse_error parse_param_decl(
    parser* p, sym_param** tgt, ureg ctx_start, ureg ctx_end, char* msg_context)
{
    parse_error pe;
    token* t;
    PEEK(p, t);
    if (t->type != TT_STRING) {
        parser_error_2a(
            p, "invalid parameter syntax", t->start, t->end,
            "expected parameter identifier", ctx_start, ctx_end, msg_context);
        return PE_UNEXPECTED_TOKEN;
    }
    sym_param* d = alloc_perm(p, sizeof(sym_param));
    if (!d) return PE_FATAL;
    d->symbol.name = alloc_string_perm(p, t->str);
    if (!d->symbol.name) return PE_FATAL;
    d->symbol.stmt.type = SYM_PARAM;
    // TODO: flags parsing
    d->symbol.stmt.flags = STMT_FLAGS_DEFAULT;
    tk_void(&p->tk);
    PEEK(p, t);
    if (t->type != TT_COLON) {
        parser_error_2a(
            p, "invalid parameter syntax", t->start, t->end,
            "expected ':' after identifier", ctx_start, ctx_end, msg_context);
        return PE_HANDLED;
    }
    tk_void(&p->tk);
    PEEK(p, t);
    if (t->type == TT_EQUALS) {
        tk_void(&p->tk);
        d->type = NULL;
        pe = parse_expression(p, &d->default_value);
        if (pe == PE_EOEX) {
            PEEK(p, t);
            parser_error_2a(
                p, "invalid parameter declaration syntax", t->start, t->end,
                "expected expression after '='", ctx_start, ctx_end,
                msg_context);
            return PE_HANDLED;
        }
        if (pe) return pe;
    }
    else {
        pe = parse_expression_of_prec(p, &d->type, op_precedence[OP_EQUAL] + 1);
        if (pe == PE_EOEX) {
            PEEK(p, t);
            parser_error_2a(
                p, "invalid parameter declaration syntax", t->start, t->end,
                "expected type expression after ':'", ctx_start, ctx_end,
                msg_context);
            return PE_HANDLED;
        }
        if (pe) return pe;
        PEEK(p, t);
        if (t->type == TT_EQUALS) {
            tk_void(&p->tk);
            pe = parse_expression(p, &d->default_value);
            if (pe == PE_EOEX) {
                PEEK(p, t);
                parser_error_2a(
                    p, "invalid parameter declaration syntax", t->start, t->end,
                    "expected expression after '='", ctx_start, ctx_end,
                    msg_context);
                return PE_HANDLED;
            }
            if (pe) return pe;
        }
        else {
            d->default_value = NULL;
        }
    }
    *tgt = d;
    return PE_OK;
}
parse_error parse_expr_node_list(
    parser* p, expr* prefetch, expr*** tgt, char* type,
    token_type expected_trailer)
{
    token* t;
    PEEK(p, t);
    if (t->type == expected_trailer) {
        if (!prefetch) {
            *tgt = NULL;
            return PE_OK;
        }
        else {
            *tgt = alloc_perm(p, sizeof(expr*) * 2);
            if (!*tgt) return PE_FATAL;
            **tgt = prefetch;
            *(*tgt + 1) = NULL;
            return PE_OK;
        }
    }
    void** list_start = list_builder_start(&p->list_builder);
    if (prefetch) list_builder_add(&p->list_builder, prefetch);
    while (true) {
        expr* ex;
        parse_error pe = parse_expression(p, &ex);
        if (pe == PE_EOEX) {
            PEEK(p, t);
            char* msg = error_log_cat_strings_3(
                &p->tk.tc->error_log, "invalid ", type, " syntax");
            if (!msg) return PE_FATAL;
            parser_error_1a(
                p, msg, t->start, t->end, "expected expression after ','");
            return PE_HANDLED;
        }
        if (pe != PE_OK) return pe;
        int r = list_builder_add(&p->list_builder, (void*)ex);
        if (r) return PE_FATAL;
        PEEK(p, t);
        if (t->type == TT_COMMA) {
            tk_void(&p->tk);
        }
        else if (t->type == expected_trailer) {
            break;
        }
        else {
            return PE_UNEXPECTED_TOKEN;
        }
    }
    *tgt = (expr**)list_builder_pop_list_zt(
        &p->list_builder, list_start, &p->tk.tc->permmem);
    if (!*tgt) return PE_FATAL;
    return PE_OK;
}
static inline parse_error parse_array(parser* p, token* t, expr** ex)
{
    ureg t_start = t->start;
    ureg t_end = t->end;
    tk_void(&p->tk);
    expr_array* arr = alloc_perm(p, sizeof(expr_array));
    if (!arr) return PE_FATAL;
    arr->expr.type = EXPR_ARRAY;
    parse_error pe = parse_expr_node_list(
        p, NULL, &arr->elements, "array", TT_BRACKET_CLOSE);
    // TODO: EMSG: suboptimal e.g. for case [,,]
    if (pe == PE_UNEXPECTED_TOKEN) {
        PEEK(p, t);
        parser_error_2a(
            p, "array brackets missmatch", t->start, t->end,
            "reached end of expression due to unexpected token", t_start, t_end,
            "didn't find a matching bracket for this array");
        return PE_HANDLED;
    }
    if (pe != PE_OK) return pe;
    PEEK(p, t);
    if (expr_fill_srange(p, &arr->expr, t_start, t->end)) return PE_FATAL;
    tk_void(&p->tk);
    *ex = (expr*)arr;
    return PE_OK;
}
static inline parse_error
parse_tuple_after_first_comma(parser* p, ureg t_start, ureg t_end, expr** ex)
{
    expr_tuple* tp = alloc_perm(p, sizeof(expr_tuple));
    tp->expr.type = EXPR_TUPLE;
    void** list = list_builder_start(&p->list_builder);
    list_builder_add(&p->list_builder, *ex);
    token* t;
    PEEK(p, t);
    bool err = false;
    while (true) {
        if (t->type == TT_PAREN_CLOSE) break;
        parse_error pe = parse_expression(p, ex);
        if (pe) {
            if (pe == PE_EOEX) {
                err = true;
                break;
            }
            return pe;
        }
        list_builder_add(&p->list_builder, *ex);
        PEEK(p, t);
        if (t->type == TT_COMMA) {
            tk_void(&p->tk);
            PEEK(p, t);
            continue;
        }
        if (t->type == TT_PAREN_CLOSE) break;
        err = true;
        break;
    }
    if (err) {
        parser_error_2a(
            p, "tuple parenthesis missmatch", t->start, t->end,
            "reached end of expression due to unexpected token", t_start, t_end,
            "didn't find a matching parenthesis for this tuple");
        return PE_HANDLED;
    }
    tp->elements = (expr**)list_builder_pop_list_zt(
        &p->list_builder, list, &p->tk.tc->permmem);
    if (!tp->elements) return PE_FATAL;
    if (expr_fill_srange(p, (expr*)tp, t_start, t->end)) return PE_FATAL;
    tk_void(&p->tk);
    *ex = (expr*)tp;
    return PE_OK;
}
static inline parse_error
build_expr_parentheses(parser* p, ureg t_start, ureg t_end, expr** ex)
{
    token* t;
    PEEK(p, t);
    if (t->type != TT_PAREN_CLOSE) {
        parser_error_2a(
            p, "parenthesis missmatch", t->start, t->end,
            "reached end of expression", t_start, t_end,
            "didn't find a match for this parenthesis");
        return PE_HANDLED;
    }
    tk_void(&p->tk);
    expr_parentheses* pr =
        (expr_parentheses*)alloc_perm(p, sizeof(expr_parentheses));
    if (!pr) return PE_FATAL;
    pr->expr.type = EXPR_OP_PARENTHESES;
    pr->child = *ex;
    if (expr_fill_srange(p, &pr->expr, t_start, t->end)) return PE_FATAL;
    *ex = (expr*)pr;
    return PE_OK;
}
static inline parse_error
build_empty_tuple(parser* p, ureg t_start, ureg t_end, expr** ex)
{
    expr_tuple* tp = alloc_perm(p, sizeof(expr_tuple));
    if (!tp) return PE_FATAL;
    tp->expr.type = EXPR_TUPLE;
    if (expr_fill_srange(p, (expr*)tp, t_start, t_end)) return PE_FATAL;
    tp->elements = NULL;
    *ex = (expr*)tp;
    return PE_OK;
}
static inline parse_error require_default_flags(
    parser* p, token* t, stmt_flags flags, ureg start, ureg end)
{
    if (flags == STMT_FLAGS_DEFAULT) return PE_OK;
    char* loc_msg = error_log_cat_strings_2(
        &p->tk.tc->error_log, token_strings[t->type],
        " does not accept any modifiers");
    if (!loc_msg) return PE_FATAL;
    parser_error_2a(
        p, loc_msg, start, end, "invalid modifier(s)", t->start, t->end,
        "before this statement");
    return PE_HANDLED;
}
static inline parse_error
parse_paren_group_or_tuple(parser* p, token* t, expr** ex)
{
    ureg t_start = t->start;
    ureg t_end = t->end;
    tk_void(&p->tk);
    PEEK(p, t);
    if (t->type == TT_PAREN_CLOSE) {
        tk_void(&p->tk);
        return build_empty_tuple(p, t_start, t->end, ex);
    }
    parse_error pe = parse_expression_of_prec(p, ex, PREC_BASELINE);
    if (pe != PE_OK && pe != PE_EOEX) return pe;
    PEEK(p, t);
    if (t->type == TT_COMMA) {
        tk_void(&p->tk);
        return parse_tuple_after_first_comma(p, t_start, t_end, ex);
    }
    else if (t->type == TT_PAREN_CLOSE) {
        return build_expr_parentheses(p, t_start, t_end, ex);
    }
    else {
        parser_error_2a(
            p, "unexpected token after expression", t->start, t->end,
            "expected comma or closing parenthesis", t_start, t_end,
            "in parenthesized expression starting here");
        return PE_HANDLED;
    }
}
typedef union tuple_ident_node {
    sym_var_uninitialized var;
    expr_identifier ident;
} tuple_ident_node;

static inline parse_error
parse_uninitialized_var_in_tuple(parser* p, token* t, expr** ex)
{
    sym_var_uninitialized* v = alloc_perm(p, sizeof(tuple_ident_node));
    v->symbol.stmt.type = SYM_VAR_UNINITIALIZED;
    v->symbol.stmt.eflags = ERR_FLAGS_DEFAULT;
    v->symbol.stmt.flags = STMT_FLAGS_DEFAULT;
    stmt_flags_set_compound_decl(&v->symbol.stmt.flags);
    v->symbol.name = alloc_string_perm(p, t->str);
    if (!v->symbol.name) return PE_FATAL;
    if (stmt_fill_srange(p, (stmt*)v, t->start, t->end)) return PE_FATAL;
    tk_void_n(&p->tk, 2);
    PEEK(p, t);
    if (t->type == TT_COMMA || t->type == TT_PAREN_CLOSE) {
        v->type = NULL;
        *ex = (expr*)v;
        return PE_OK;
    }
    else {
        ureg t_start = t->start;
        parse_error pe = parse_expression(p, &v->type);
        if (!pe) {
            *ex = (expr*)v;
            return PE_OK;
        }
        if (pe == PE_EOEX) {
            PEEK(p, t);
            parser_error_1a(
                p, "invalid var declaration syntax", t_start, t->end,
                "expected a type expression or a comma");
            return PE_HANDLED;
        }
        return pe;
    }
}

static inline parse_error
build_ident_node_in_tuple(parser* p, token* t, expr** ex)
{
    tuple_ident_node* tin = alloc_perm(p, sizeof(tuple_ident_node));
    sym_var_uninitialized* v = &tin->var;
    v->symbol.stmt.type = SYM_VAR_UNINITIALIZED;
    v->symbol.stmt.eflags = ERR_FLAGS_DEFAULT;
    v->symbol.stmt.flags = STMT_FLAGS_DEFAULT;
    v->symbol.name = alloc_string_perm(p, t->str);
    if (!v->symbol.name) return PE_FATAL;
    if (stmt_fill_srange(p, (stmt*)v, t->start, t->end)) return PE_FATAL;
    v->type = NULL;
    *ex = (expr*)tin;
    tk_void(&p->tk);
    return PE_OK;
}
static inline void turn_ident_nodes_to_exprs(expr** elems)
{
    if (!elems) return;
    while (*elems) {
        if ((**elems).type == SYM_VAR_UNINITIALIZED) {
            tuple_ident_node* tin = (tuple_ident_node*)*elems;
            if (tin->var.type == NULL &&
                !stmt_flags_get_compound_decl(tin->var.symbol.stmt.flags)) {
                ureg srange = tin->var.symbol.stmt.srange;
                tin->ident.value = tin->var.symbol.name;
                tin->ident.expr.srange = srange;
                tin->ident.expr.type = EXPR_IDENTIFIER;
            }
        }
        else if ((**elems).type == EXPR_TUPLE) {
            turn_ident_nodes_to_exprs((**((expr_tuple**)elems)).elements);
        }
        elems++;
    }
}
static inline parse_error parse_paren_group_or_tuple_or_compound_decl(
    parser* p, token* t, expr** ex, expr*** elem_list, bool* contains_decls,
    ureg* list_end)
{
    parse_error pe;
    ureg t_start = t->start;
    ureg t_end = t->end;
    tk_void(&p->tk);
    PEEK(p, t);
    if (t->type == TT_PAREN_CLOSE) {
        tk_void(&p->tk);
        return build_empty_tuple(p, t_start, t->end, ex);
    }
    void** element_list = NULL;
    if (t->type != TT_STRING) {
        pe = parse_expression(p, ex);
        if (pe == PE_EOEX) {
            PEEK(p, t);
            parser_error_2a(
                p, "unexpected token after opening parenthesis", t->start,
                t->end, "expected an expression, a declaration or a closing "
                        "parenthesis",
                t_start, t_end, "opening parenthesis here");
            return PE_HANDLED;
        }
        if (pe) return pe;
        PEEK(p, t);
        if (t->type == TT_COMMA) {
            element_list = list_builder_start(&p->list_builder);
            if (list_builder_add(&p->list_builder, *ex)) return PE_FATAL;
        }
        else {
            return build_expr_parentheses(p, t_start, t_end, ex);
        }
    }
    else {
        token* t2;
        t2 = tk_peek_2nd(&p->tk);
        if (!t2) return PE_TK_ERROR;
        if (t2->type == TT_COLON) {
            *contains_decls = true;
            pe = parse_uninitialized_var_in_tuple(p, t, ex);
            if (pe) return pe;
            PEEK(p, t);
        }
        else if (t2->type != TT_COMMA) {
            pe = parse_expression(p, ex);
            if (pe) return pe;
            PEEK(p, t);
            if (t->type == TT_PAREN_CLOSE) {
                return build_expr_parentheses(p, t_start, t_end, ex);
            }
            PEEK(p, t);
        }
        else {
            pe = build_ident_node_in_tuple(p, t, ex);
            if (pe) return pe;
            t = t2;
        }
        element_list = list_builder_start(&p->list_builder);
        if (list_builder_add(&p->list_builder, *ex)) return PE_FATAL;
    }
    while (true) {
        if (t->type == TT_COMMA) {
            tk_void(&p->tk);
            PEEK(p, t);
        }
        else if (t->type != TT_PAREN_CLOSE) {
            parser_error_2a(
                p, "unexpected token in tuple", t->start, t->end,
                "expected a comma or a closing parenthesis", t_start, t_end,
                "tuple starts here");
            return PE_HANDLED;
        }
        if (t->type == TT_PAREN_CLOSE) {
            tk_void(&p->tk);
            expr** res_elem_list = (expr**)list_builder_pop_list_zt(
                &p->list_builder, element_list, &p->tk.tc->permmem);
            if (!res_elem_list) return PE_FATAL;
            if (elem_list) {
                *elem_list = res_elem_list;
                *list_end = t->end;
            }
            else {
                expr_tuple* tp = alloc_perm(p, sizeof(expr_tuple));
                if (!tp) return PE_FATAL;
                tp->expr.type = EXPR_TUPLE;
                if (expr_fill_srange(p, (expr*)tp, t_start, t->end))
                    return PE_FATAL;
                tp->elements = res_elem_list;
                *ex = (expr*)tp;
            }
            return PE_OK;
        }
        if (t->type == TT_STRING) {
            token* t2;
            t2 = tk_peek_2nd(&p->tk);
            if (!t2) return PE_TK_ERROR;
            if (t2->type == TT_COLON) {
                *contains_decls = true;
                pe = parse_uninitialized_var_in_tuple(p, t, ex);
                if (pe) return pe;
            }
            else if (t2->type == TT_COMMA || t2->type == TT_PAREN_CLOSE) {
                pe = build_ident_node_in_tuple(p, t, ex);
                if (pe) return pe;
            }
            else {
                pe = parse_expression(p, ex);
                if (pe) return pe;
            }
        }
        else if (t->type == TT_PAREN_OPEN) {
            pe = parse_paren_group_or_tuple_or_compound_decl(
                p, t, ex, NULL, contains_decls, NULL);
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
                return PE_HANDLED;
            }
            if (pe) return pe;
        }
        if (list_builder_add(&p->list_builder, *ex)) return PE_FATAL;
        PEEK(p, t);
    }
}
static inline parse_error
parse_prefix_unary_op(parser* p, ast_node_type op, expr** ex)
{
    token* t = tk_aquire(&p->tk);
    expr_op_unary* ou = (expr_op_unary*)alloc_perm(p, sizeof(expr_op_unary));
    if (!ou) return PE_FATAL;
    if (expr_fill_srange(p, &ou->expr, t->start, t->end)) return PE_FATAL;
    tk_void(&p->tk);
    ou->expr.type = EXPR_OP_UNARY;
    ou->expr.op_type = op;
    parse_error pe = parse_expression_of_prec(
        p, &ou->child, op_precedence[op] + is_left_associative(op));
    if (pe) {
        PEEK(p, t);
        if (pe == PE_EOEX) {
            src_range_large s;
            src_range_unpack(ou->expr.srange, &s);
            parser_error_2a(
                p, "missing operand for unary operator", t->start, t->end,
                "reached end of expression due to unexpected token", s.start,
                s.end, "missing operand for this operator");
        }
        return PE_HANDLED;
    }
    *ex = (expr*)ou;
    return PE_OK;
}
parse_error parse_continue_stmt(
    parser* p, stmt_flags flags, ureg start, ureg flags_end, stmt** tgt)
{
    token* t = tk_aquire(&p->tk);
    ureg end = t->end;
    parse_error pe = require_default_flags(p, t, flags, start, flags_end);
    if (pe) return pe;
    tk_void(&p->tk);
    PEEK(p, t);
    const char* target;
    if (t->type == TT_STRING) {
        target = alloc_string_perm(p, t->str);
        if (!target) return PE_FATAL;
    }
    else if (is_kw_valid_label(t->type)) {
        target = token_strings[t->type];
    }
    else {
        target = NULL;
    }
    if (target) {
        end = t->end;
        tk_void(&p->tk);
    }
    stmt_continue* c = alloc_perm(p, sizeof(stmt_continue));
    if (!c) return PE_FATAL;
    stmt_init((stmt*)c, STMT_BREAK);
    c->target.name = target;
    if (stmt_fill_srange(p, (stmt*)c, start, end)) return PE_FATAL;
    *tgt = (stmt*)c;
    return PE_OK;
}
parse_error parse_return_stmt(
    parser* p, stmt_flags flags, ureg start, ureg flags_end, stmt** tgt)
{
    token* t = tk_aquire(&p->tk);
    parse_error pe = require_default_flags(p, t, flags, start, flags_end);
    if (pe) return pe;
    ureg end = t->end;
    tk_void(&p->tk);
    stmt_return* r = alloc_perm(p, sizeof(stmt_return));
    if (!r) return PE_FATAL;
    r->stmt.type = STMT_RETURN;
    PEEK(p, t);
    if (t->type == TT_SEMICOLON) {
        r->value = NULL;
        if (stmt_fill_srange(p, (stmt*)r, start, t->end)) return PE_FATAL;
    }
    else {
        parse_error pe = parse_expression(p, &r->value);
        if (pe == PE_EOEX) {
            PEEK(p, t);
            parser_error_2a(
                p, "unexpected token in return expression", t->start, t->end,
                "expected expression or ';'", start, end,
                "in this return expression");
            return PE_HANDLED;
        }
        if (pe) return pe;
        if (stmt_fill_srange(
                p, (stmt*)r, start, src_range_get_end(r->value->srange)))
            return PE_FATAL;
    }
    if (r->stmt.srange == SRC_RANGE_INVALID) return PE_FATAL;
    *tgt = (stmt*)r;
    return PE_OK;
}
parse_error parse_goto_stmt(
    parser* p, stmt_flags flags, ureg start, ureg flags_end, stmt** tgt)
{
    token* t = tk_aquire(&p->tk);
    parse_error pe = require_default_flags(p, t, flags, start, flags_end);
    if (pe) return pe;
    ureg end = t->end;
    tk_void(&p->tk);
    PEEK(p, t);
    if (t->type != TT_STRING) {
        parser_error_2a(
            p, "missing label in goto expression", t->start, t->end,
            "expected label name", start, end, "");
    }
    stmt_goto* g = alloc_perm(p, sizeof(stmt_goto));
    g->stmt.type = STMT_GOTO;
    g->target.name = alloc_string_temp(p, t->str);
    if (!g->target.name) return PE_FATAL;
    tk_void(&p->tk);
    if (stmt_fill_srange(p, (stmt*)g, start, t->end)) return PE_FATAL;
    *tgt = (stmt*)g;
    return PE_OK;
}
parse_error parse_give_stmt(
    parser* p, stmt_flags flags, ureg start, ureg flags_end, stmt** tgt)
{
    token* t1 = tk_aquire(&p->tk);
    parse_error pe = require_default_flags(p, t1, flags, start, flags_end);
    if (pe) return pe;
    ureg end = t1->end;
    tk_void(&p->tk);
    stmt_give* g = alloc_perm(p, sizeof(stmt_give));
    if (!g) return PE_FATAL;
    g->stmt.type = STMT_GIVE;
    g->target.name = NULL;
    pe = parse_expression(p, &g->value);
    if (pe == PE_EOEX) {
        PEEK(p, t1);
        parser_error_2a(
            p, "unexpected token in give statement", t1->start, t1->end,
            "expected expression", start, end, "in this give statement");
    }
    if (pe) return pe;
    if (stmt_fill_srange(
            p, (stmt*)g, start, src_range_get_end(g->value->srange)))
        return PE_FATAL;
    *tgt = (stmt*)g;
    return PE_OK;
}
parse_error parse_break_stmt(
    parser* p, stmt_flags flags, ureg start, ureg flags_end, stmt** tgt)
{
    token* t = tk_aquire(&p->tk);
    ureg end = t->end;
    parse_error pe = require_default_flags(p, t, flags, start, flags_end);
    if (pe) return pe;
    tk_void(&p->tk);
    PEEK(p, t);
    const char* target;
    if (t->type == TT_STRING) {
        target = alloc_string_perm(p, t->str);
        if (!target) return PE_FATAL;
    }
    else if (is_kw_valid_label(t->type)) {
        target = token_strings[t->type];
    }
    else {
        target = NULL;
    }
    if (target) {
        end = t->end;
        tk_void(&p->tk);
        PEEK(p, t);
        if (t->type == TT_KW_GIVE) {
            pe =
                parse_give_stmt(p, STMT_FLAGS_DEFAULT, t->start, t->start, tgt);
            if (pe) return pe;
            ((stmt_give*)*tgt)->target.name = target;
            return PE_OK;
        }
    }
    stmt_break* g = alloc_perm(p, sizeof(stmt_give));
    if (!g) return PE_FATAL;
    g->stmt.type = STMT_BREAK;
    g->target.name = target;
    if (stmt_fill_srange(p, (stmt*)g, start, end)) return PE_FATAL;
    *tgt = (stmt*)g;
    return PE_OK;
}
static inline parse_error parse_expr_block(parser* p, expr** ex)
{
    expr_block* b = alloc_perm(p, sizeof(expr_block));
    b->expr.type = EXPR_BLOCK;
    *ex = (expr*)b;
    return parse_brace_delimited_body(p, &b->body);
}
parse_error parse_loop(parser* p, expr** tgt, ureg start, char* label)
{
    token* t = tk_aquire(&p->tk);
    tk_void(&p->tk);
    expr_loop* l = alloc_perm(p, sizeof(expr_loop));
    if (!l) return PE_FATAL;
    if (expr_fill_srange(p, (expr*)l, start, t->end)) return PE_FATAL;
    l->expr_named.name = label;
    l->expr_named.expr.type = EXPR_LOOP;
    *tgt = (expr*)l;
    return parse_body(p, &l->body);
}
parse_error parse_match(parser* p, expr** tgt, ureg start, char* label)
{
    token* t = tk_aquire(&p->tk);
    ureg t_end = t->end;
    tk_void(&p->tk);
    expr_match* em = alloc_perm(p, sizeof(expr_match));
    em->expr_named.expr.type = EXPR_MATCH;
    em->expr_named.name = label;
    parse_error pe = parse_expression(p, &em->match_expr);
    if (pe == PE_EOEX) {
        PEEK(p, t);
        parser_error_2a(
            p, "invalid match syntax", t->start, t->end,
            "expected match expression", start, t_end, "in this match");
    }
    if (pe) return pe;
    PEEK(p, t);
    if (t->type != TT_BRACE_OPEN) {
        ureg e_end;
        get_expr_bounds(&em->expr_named.expr, NULL, &e_end);
        parser_error_2a(
            p, "invalid match syntax", t->start, t->end,
            "expected match expression", start, e_end, "in this match");
    }
    tk_void(&p->tk);
    void** list = list_builder_start(&p->list_builder);
    while (true) {
        PEEK(p, t);
        if (t->type == TT_BRACE_CLOSE) {
            em->match_arms = (match_arm**)list_builder_pop_list_zt(
                &p->list_builder, list, &p->tk.tc->permmem);
            if (!em->match_arms) return PE_FATAL;
            *tgt = (expr*)em;
            tk_void(&p->tk);
            em->body_end = t->end;
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
                return PE_HANDLED;
            }
            if (pe) return pe;
            PEEK(p, t);
            if (t->type != TT_FAT_ARROW) {
                ureg exp_start, exp_end;
                get_expr_bounds(ma->condition, &exp_start, &exp_end);
                parser_error_2a(
                    p, "invalid match syntax", t->start, t->end,
                    "expected '=>'", exp_start, exp_end,
                    "after this match condition");
                return PE_HANDLED;
            }
            tk_void(&p->tk);
            pe = parse_body(p, &ma->body);
            if (pe) return pe;
            PEEK(p, t);
            if (t->type == TT_SEMICOLON) {
                tk_void(&p->tk);
            }
            else if (!body_is_braced(&ma->body)) {
                ureg arm_start, arm_end;
                get_expr_bounds(ma->condition, &arm_start, NULL);
                arm_end = src_range_get_end(ma->body.srange);
                parser_error_2a(
                    p, "invalid match syntax", t->start, t->end,
                    "expected semicolon", arm_start, arm_end,
                    "after this match arm statement");
                return PE_HANDLED;
            }
            list_builder_add(&p->list_builder, ma);
        }
    }
}
parse_error parse_do(parser* p, expr** tgt, ureg start, char* label)
{
    token* t = tk_aquire(&p->tk);
    ureg end = t->end;
    tk_void(&p->tk);
    PEEK(p, t);
    expr* e = NULL;
    body b;
    parse_error pe;
    if (t->type == TT_BRACE_OPEN) {
        pe = parse_brace_delimited_body(p, &b);
        if (pe) return pe;
    }
    else {
        pe = parse_expression(p, &e);
        if (pe == PE_EOEX) {
            parser_error_2a(
                p, "unexpected token in do expression", t->start, t->end,
                "expected expression", start, end, "do expression starts here");
        }
        if (pe) return pe;
    }
    PEEK(p, t);
    if (t->type == TT_KW_WHILE) {
        tk_void(&p->tk);
        if (e) {
            expr_to_stmt(
                p, &b.children, e, start, src_range_get_end(e->srange));
            b.children->next = NULL;
            b.srange = e->srange;
        }
        expr_do_while* edw = alloc_perm(p, sizeof(expr_do_while));
        edw->expr_named.name = label;
        edw->expr_named.expr.type = EXPR_DO_WHILE;
        if (expr_fill_srange(p, (expr*)edw, start, end)) return PE_FATAL;
        *tgt = (expr*)edw;
        edw->do_body = b;
        pe = parse_expression(p, &edw->condition);
        if (pe == PE_EOEX) {
            parser_error_2a(
                p, "invalid do while loop syntax", t->start, t->end,
                "expected while condition expression", start, end,
                "do while starts here");
        }
        PEEK(p, t);
        if (t->type == TT_KW_FINALLY) {
            tk_void(&p->tk);
            pe = parse_body(p, &edw->finally_body);
        }
        else {
            edw->finally_body.children = NULL;
        }
        return pe;
    }
    else {
        if (label) {
            parser_error_2a(
                p, "labeled do only allowed with do ... while", t->start,
                t->end, "expected while keyword", start, end,
                "labeled do starts here");
            return PE_HANDLED;
        }
        if (!e) {
            expr_block* eb = alloc_perm(p, sizeof(expr_block));
            eb->body = b;
            eb->expr.srange = b.srange;
            eb->expr.type = EXPR_BLOCK;
            e = (expr*)eb;
        }
    }
    expr_do* ed = alloc_perm(p, sizeof(expr_do));
    ed->expr.type = EXPR_DO;
    ed->expr_body = e;
    *tgt = (expr*)ed;
    PEEK(p, t);
    switch (t->type) {
        case TT_KW_CONTINUE: {
            pe = parse_continue_stmt(
                p, STMT_FLAGS_DEFAULT, t->start, t->start, &ed->tail_stmt);
        } break;
        case TT_KW_BREAK: {
            pe = parse_give_stmt(
                p, STMT_FLAGS_DEFAULT, t->start, t->start, &ed->tail_stmt);
        } break;
        default: {
            ed->tail_stmt = NULL;
        } break;
    }
    return pe;
}
parse_error parse_while(parser* p, expr** tgt, ureg start, char* label)
{
    token* t = tk_aquire(&p->tk);
    ureg end = t->end;
    tk_void(&p->tk);
    expr_while* w = alloc_perm(p, sizeof(expr_while));
    if (!w) return PE_FATAL;
    parse_error pe = parse_expression(p, &w->condition);
    if (pe == PE_EOEX) {
        PEEK(p, t);
        parser_error_2a(
            p, "invalid while loop syntax", t->start, t->end,
            "expected while condition expression", start, end,
            "in this while loop");
        return PE_HANDLED;
    }
    if (pe) return pe;
    if (expr_fill_srange(p, (expr*)w, start, end)) return PE_FATAL;
    w->expr_named.name = label;
    w->expr_named.expr.type = EXPR_WHILE;
    *tgt = (expr*)w;
    pe = parse_body(p, &w->while_body);
    if (pe) return pe;
    PEEK(p, t);
    if (t->type == TT_KW_FINALLY) {
        tk_void(&p->tk);
        pe = parse_body(p, &w->finally_body);
    }
    else {
        w->finally_body.children = NULL;
    }
    return pe;
}
parse_error parse_if(parser* p, expr** tgt)
{
    token* t = tk_aquire(&p->tk);
    ureg start = t->start;
    ureg end = t->end;
    tk_void(&p->tk);
    expr_if* i = alloc_perm(p, sizeof(expr_if));
    if (!i) return PE_FATAL;
    PEEK(p, t);
    if (t->type != TT_PAREN_OPEN) {
        parser_error_1a(
            p, "invalid if expression syntax", t->start, t->end,
            "expected opening parenthesis");
    }
    tk_void(&p->tk);
    parse_error pe = parse_expression(p, &i->condition);
    if (pe) return pe;
    if (pe == PE_EOEX) {
        PEEK(p, t);
        parser_error_2a(
            p, "invalid if expression syntax", t->start, t->end,
            "expected if condition expression", start, end,
            "in this if condition");
        return PE_HANDLED;
    }
    PEEK(p, t);
    if (t->type != TT_PAREN_CLOSE) {
        panic("TODO: error");
    }
    tk_void(&p->tk);
    if (expr_fill_srange(p, (expr*)i, start, end)) return PE_FATAL;
    i->expr.type = EXPR_IF;
    *tgt = (expr*)i;
    pe = parse_expression(p, &i->if_body);
    if (pe) return pe;
    PEEK(p, t);
    if (t->type == TT_KW_ELSE) {
        tk_void(&p->tk);
        pe = parse_expression(p, &i->else_body);
    }
    else {
        i->else_body = NULL;
    }
    return pe;
}
static inline void check_unepected_expr_after_label(
    parser* p, char* label, ureg label_start, ureg label_end)
{
    if (!label) return;
    token* t = tk_aquire(&p->tk);
    parser_error_2a(
        p, "unexpected expression after label", t->start, t->end,
        "expected control flow statement or semicolon", label_start, label_end,
        "after this label");
}
static inline parse_error parse_labeled_value_expr(
    parser* p, expr** ex, ureg label_start, ureg label_end, char* label)
{
    token* t;
    PEEK(p, t);
    switch (t->type) {
        case TT_PAREN_OPEN: {
            check_unepected_expr_after_label(p, label, label_start, label_end);
            return parse_paren_group_or_tuple(p, t, ex);
        }
        case TT_BRACKET_OPEN: {
            check_unepected_expr_after_label(p, label, label_start, label_end);
            return parse_array(p, t, ex);
        }
        case TT_BRACE_OPEN: {
            check_unepected_expr_after_label(p, label, label_start, label_end);
            return parse_expr_block(p, ex);
        }
        case TT_KW_LOOP: {
            return parse_loop(p, ex, label_start, label);
        }
        case TT_KW_LABEL: {
            check_unepected_expr_after_label(p, label, label_start, label_end);
            token* t2 = tk_peek_2nd(&p->tk);
            if (t2->type != TT_STRING) {
                parser_error_2a(
                    p, "unexpected token in label expression", t2->start,
                    t2->end, "expected label name", t->start, t->end,
                    "in this label expression");
                return PE_HANDLED;
            }
            tk_void_n(&p->tk, 2);
            char* label = alloc_string_perm(p, t2->str);
            if (!label) return PE_FATAL;
            return parse_labeled_value_expr(p, ex, t->start, t2->end, label);
        }
        case TT_KW_FOR: {
            // TODO: for loop
        }
        case TT_KW_WHILE: {
            return parse_while(p, ex, label_start, label);
        }
        case TT_KW_DO: {
            return parse_do(p, ex, label_start, label);
        }
        case TT_KW_MATCH: {
            return parse_match(p, ex, label_start, label);
        }
        case TT_KW_IF: {
            check_unepected_expr_after_label(p, label, label_start, label_end);
            return parse_if(p, ex);
        }
        case TT_STRING:
        case TT_NUMBER:
        case TT_LITERAL:
        case TT_BINARY_LITERAL: {
            *ex = parse_str_value(p, t);
            if (!*ex) return PE_FATAL;
            tk_void(&p->tk);
            return PE_OK;
        } break;
        default: {
            return PE_EOEX; // investigate: shouldn't this be unexp. tok?
        } break;
    }
}
static inline parse_error parse_value_expr(parser* p, expr** ex)
{
    return parse_labeled_value_expr(p, ex, 0, 0, NULL);
}
static inline parse_error parse_call(parser* p, expr** ex, expr* lhs)
{
    token* t = tk_aquire(&p->tk);
    ureg t_start = t->start;
    tk_void(&p->tk);
    expr_call* call = alloc_perm(p, sizeof(expr_call));
    if (!call) return PE_FATAL;
    parse_error pe =
        parse_expr_node_list(p, NULL, &call->args, "call", TT_PAREN_CLOSE);
    // EMSG: suboptimal e.g. for case {,,}
    if (pe == PE_UNEXPECTED_TOKEN) {
        PEEK(p, t);
        parser_error_2a(
            p, "unclosed function call", t->start, t->end,
            "reached end of expression due to unexpected token", t_start,
            t_start + 1, "didn't find a matching parenthesis for this");
        return PE_HANDLED;
    }
    if (pe != PE_OK) return pe;
    PEEK(p, t);
    if (expr_fill_srange(p, &call->expr, t_start, t->end)) return PE_FATAL;
    tk_void(&p->tk);
    call->expr.type = EXPR_OP_CALL;
    call->expr.op_type = OP_CALL;
    call->lhs = lhs;
    *ex = (expr*)call;
    return PE_OK;
}
static inline parse_error parse_access(parser* p, expr** ex, expr* lhs)
{
    token* t = tk_aquire(&p->tk);
    ureg t_start = t->start;
    tk_void(&p->tk);
    expr_access* acc = alloc_perm(p, sizeof(expr_access));
    if (!acc) return PE_FATAL;
    parse_error pe = parse_expr_node_list(
        p, NULL, &acc->args, "access operator", TT_BRACKET_CLOSE);
    // EMSG: suboptimal e.g. for case {,,}
    if (pe == PE_UNEXPECTED_TOKEN) {
        PEEK(p, t);
        parser_error_2a(
            p, "unclosed access operator", t->start, t->end,
            "reached end of expression due to unexpected token", t_start,
            t_start + 1, "didn't find a matching bracket for this");
        return PE_HANDLED;
    }
    if (pe != PE_OK) return pe;
    PEEK(p, t);
    if (expr_fill_srange(p, &acc->expr, t_start, t->end)) return PE_FATAL;
    tk_void(&p->tk);
    acc->expr.type = EXPR_OP_ACCESS;
    acc->expr.op_type = OP_ACCESS;
    acc->lhs = lhs;
    *ex = (expr*)acc;
    return PE_OK;
}
static inline parse_error
parse_postfix_unary_op(parser* p, op_type op, expr** ex, expr* lhs)
{
    if (op == OP_CALL) {
        return parse_call(p, ex, lhs);
    }
    else if (op == OP_ACCESS) {
        return parse_access(p, ex, lhs);
    }
    else {
        token* t = tk_aquire(&p->tk);
        tk_void(&p->tk);
        expr_op_unary* ou =
            (expr_op_unary*)alloc_perm(p, sizeof(expr_op_unary));
        if (!ou) return PE_FATAL;
        ou->expr.type = EXPR_OP_UNARY;
        ou->expr.op_type = op;
        ou->child = lhs;
        if (expr_fill_srange(p, &ou->expr, t->start, t->end)) return PE_FATAL;
        *ex = (expr*)ou;
        return PE_OK;
    }
}
static inline parse_error
parse_binary_op(parser* p, op_type op, expr** ex, expr* lhs)
{
    token* t = tk_aquire(&p->tk);
    tk_void(&p->tk);
    expr_op_binary* ob = (expr_op_binary*)alloc_perm(p, sizeof(expr_op_binary));
    if (!ob) return PE_FATAL;
    if (expr_fill_srange(p, &ob->expr, t->start, t->end)) return PE_FATAL;
    ob->expr.type = EXPR_OP_BINARY;
    ob->expr.op_type = op;
    parse_error pe = parse_expression_of_prec(
        p, &ob->rhs, op_precedence[op] + is_left_associative(op));
    if (pe) {
        if (pe == PE_EOEX) {
            PEEK(p, t);
            src_range_large sr;
            src_range_unpack(ob->expr.srange, &sr);
            parser_error_2a(
                p, "missing operand for infix operator", t->start, t->end,
                "reached end of expression", sr.start, sr.end,
                "missing operand for this operator");
            return PE_HANDLED;
        }
        return pe;
    }
    ob->lhs = *ex;
    *ex = (expr*)ob;
    return PE_OK;
}
parse_error parse_expression_of_prec_post_value(parser* p, expr** ex, ureg prec)
{
    token* t;
    parse_error pe;
    op_type op;
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
parse_error parse_expression_of_prec(parser* p, expr** ex, ureg prec)
{
    token* t;
    PEEK(p, t);
    *ex = NULL;
    parse_error pe;
    // parse one prefix op(recursive) or a plain value
    op_type op = token_to_prefix_unary_op(t);
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
parse_error parse_expression(parser* p, expr** ex)
{
    return parse_expression_of_prec(p, ex, PREC_BASELINE);
}
void report_missing_semicolon(parser* p, ureg start, ureg end)
{
    token* t = tk_aquire(&p->tk);
    parser_error_2a(
        p, "missing semicolon ", t->start, t->end,
        "expected ';' to terminate the statement", start, end,
        "statement started here");
}
parse_error handle_semicolon_after_statement(parser* p, stmt* s)
{
    token* t;
    PEEK(p, t);
    if (t->type != TT_SEMICOLON) {
        if (!stmt_allowed_to_drop_semicolon(s)) {
            ureg start = src_range_get_start(s->srange);
            ureg end = src_range_get_end(s->srange);
            report_missing_semicolon(p, start, end);
            return PE_HANDLED;
        }
    }
    else {
        src_range_set_end(p->tk.tc, &s->srange, t->end);
        if (s->srange == SRC_RANGE_INVALID) return PE_FATAL;
        tk_void(&p->tk);
    }
    return PE_OK;
}
static inline parse_error parse_delimited_open_scope(
    parser* p, open_scope* osc, token_type delimiter_1, token_type delimiter_2)
{
    scope* parent = p->curr_scope;
    p->curr_scope = (scope*)osc;
    osc->requires =
        (file_require*)list_builder_start_blocklist(&p->list_builder);
    stmt** head = &osc->scope.body.children;
    *head = NULL; // so extend can check if it comes first
    token* t;
    PEEK(p, t);
    parse_error pe;
    while (t->type != delimiter_1 && t->type != delimiter_2) {
        pe = parse_statement(p, head);
        if (pe) break;
        pe = handle_semicolon_after_statement(p, *head);
        if (pe) break;
        head = &(*head)->next;
        t = tk_peek(&p->tk);
        if (!t) {
            pe = PE_TK_ERROR;
            break;
        }
    }
    *head = NULL;
    osc->requires = list_builder_pop_block_list_zt(
        &p->list_builder, osc->requires, &p->tk.tc->permmem);
    p->curr_scope = parent;
    if (!osc->requires) return PE_FATAL;
    return pe;
}
parse_error parse_eof_delimited_open_scope(parser* p, open_scope* osc)
{
    return parse_delimited_open_scope(p, osc, TT_EOF, TT_EOF);
}
parse_error parser_parse_file(parser* p, job_parse* j)
{
    // This is test code. it sucks
    int r = tk_open_file(&p->tk, j->file);
    if (r) {
        if (j->requiring_file != NULL) {
            src_range_large srl;
            src_range_unpack(j->requiring_srange, &srl);
            error_log_report_annotated(
                &p->tk.tc->error_log, ES_TOKENIZER, false,
                "required file doesn't exist", j->requiring_file, srl.start,
                srl.end, "required here");
        }
        else {
            char* file_path = tmalloc(src_file_get_path_len(j->file) + 1);
            if (!file_path) return PE_FATAL;
            src_file_write_path(j->file, file_path);
            char* msg = error_log_cat_strings_3(
                &p->tk.tc->error_log, "the requirested file \"", file_path,
                "\" doesn't exist");
            tfree(file_path);
            error_log_report_critical_failiure(&p->tk.tc->error_log, msg);
        }
        return PE_TK_ERROR;
    }
    p->current_module = TAUC.mdg.root_node;
    parse_error pe = parse_eof_delimited_open_scope(p, &p->root.oscope);
    // DBUG:
    print_astn_nl((stmt*)&p->root, 0);
    tk_close_file(&p->tk);
    if (src_file_done_parsing(j->file, p->tk.tc)) pe = PE_FATAL;
    return pe;
}
static inline const char* access_modifier_string(access_modifier am)
{
    switch (am) {
        case AM_PRIVATE: return token_strings[TT_KW_PRIVATE];
        case AM_PROTECTED: return token_strings[TT_KW_PROTECTED];
        case AM_PUBLIC: return token_strings[TT_KW_PUBLIC];
        default: return NULL;
    }
}
static inline int
report_redundant_specifier(parser* p, const char* spec, ureg start, ureg end)
{
    char* msg = error_log_cat_strings_3(
        &p->tk.tc->error_log, "redundant ", spec, " specifier");
    if (!msg) return ERR;
    parser_error_1a(p, "redundant access modifiers specified", start, end, msg);
    return OK;
}
static inline parse_error stmt_flags_from_kw_set_access_mod(
    parser* p, stmt_flags* f, access_modifier am, ureg start, ureg end)
{
    access_modifier old_am = stmt_flags_get_access_mod(*f);
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
            char* msg = error_log_cat_strings(&p->tk.tc->error_log, 4, msgstrs);
            if (!msg) return PE_FATAL;
            error_log_report_annotated(
                &p->tk.tc->error_log, ES_PARSER, false,
                "conflicting access modifiers specified", p->tk.file, start,
                end, msg);
        }
        return PE_UNEXPECTED_TOKEN;
    }
    return PE_OK;
}
parse_error parse_var_decl(
    parser* p, stmt_flags flags, ureg start, ureg flags_end, stmt** n)
{
    token* t = tk_aquire(&p->tk);
    string ident = t->str;
    tk_void(&p->tk);
    t = tk_aquire(&p->tk);
    ureg col_end = t->end;
    tk_void(&p->tk);
    parse_error pe;
    sym_var* vd = alloc_perm(p, sizeof(sym_var));
    if (!vd) return PE_FATAL;
    vd->symbol.name = alloc_string_perm(p, ident);
    if (!vd->symbol.name) return PE_FATAL;
    vd->symbol.stmt.type = SYM_VAR;
    vd->symbol.stmt.flags = flags;
    PEEK(p, t);
    if (t->type == TT_EQUALS) {
        ureg eq_end = t->end;
        tk_void(&p->tk);
        vd->type = NULL;
        pe = parse_expression(p, &vd->value);
        if (pe == PE_EOEX) {
            PEEK(p, t);
            parser_error_2a(
                p, "invalid declaration syntax", t->start, t->end,
                "expected expression after '='", start, eq_end,
                "begin of declaration");
            return PE_HANDLED;
        }
        if (pe) return pe;
    }
    else {
        pe =
            parse_expression_of_prec(p, &vd->type, op_precedence[OP_EQUAL] + 1);
        if (pe == PE_EOEX) {
            PEEK(p, t);
            parser_error_2a(
                p, "invalid declaration syntax", t->start, t->end,
                "expected type or '='", start, col_end, "begin of declaration");
            return PE_HANDLED;
        }
        if (pe) return pe;
        PEEK(p, t);
        if (t->type == TT_EQUALS) {
            ureg eq_end = t->end;
            tk_void(&p->tk);
            pe = parse_expression(p, &vd->value);
            if (pe == PE_EOEX) {
                parser_error_2a(
                    p, "unexpeted token in declaration", t->start, t->end,
                    "expected expression", start, eq_end,
                    "in this declaration");
                return PE_HANDLED;
            }
            if (pe) return pe;
            PEEK(p, t);
        }
        else {
            vd->value = NULL;
        }
    }
    stmt_fill_srange(p, (stmt*)vd, start, t->end);
    *n = (stmt*)vd;
    return PE_OK;
}
parse_error parse_param_list(
    parser* p, sym_param** tgt, bool generic, ureg ctx_start, ureg ctx_end,
    char* msg)
{
    token* t;
    token_type end_tok = generic ? TT_BRACKET_CLOSE : TT_PAREN_CLOSE;
    PEEK(p, t);
    if (t->type == end_tok) {
        tk_void(&p->tk);
        *tgt = NULL;
        return PE_OK;
    }
    do {
        parse_error pe = parse_param_decl(p, tgt, ctx_start, ctx_end, msg);
        if (pe) {
            *tgt = NULL;
            return pe;
        }
        tgt = (sym_param**)&(*tgt)->symbol.stmt.next;
        PEEK(p, t);
        if (t->type == TT_COMMA) {
            tk_void(&p->tk);
        }
        else if (t->type != end_tok) {
            char* e1 = generic ? "invalid generic parameter list syntax"
                               : "invalid parameter list syntax";
            char* e2 = generic ? "expected ',' or ']' in generic parameter list"
                               : "expected ',' or ')' in parameter list";
            error_log_report_annotated_twice(
                &p->tk.tc->error_log, ES_PARSER, false, e1, p->tk.file,
                t->start, t->end, e2, ctx_start, ctx_end, msg);
            return PE_HANDLED;
        }
    } while (t->type != end_tok);
    tk_void(&p->tk);
    *tgt = NULL;
    return PE_OK;
}
parse_error parse_func_decl(
    parser* p, stmt_flags flags, ureg start, ureg flags_end, stmt** n)
{
    tk_void(&p->tk);
    token* t;
    parse_error pe;
    PEEK(p, t);
    if (t->type != TT_STRING) {
        parser_error_2a(
            p, "invalid function declaration syntax", t->start, t->end,
            "expected function identifier", start, t->end,
            "in this function declaration");
        return PE_HANDLED;
    }
    ureg decl_end = t->end;
    char* name = alloc_string_perm(p, t->str);
    if (!name) return PE_FATAL;
    tk_void(&p->tk);
    PEEK(p, t);
    scope* sc;
    bool generic;
    if (t->type == TT_BRACKET_OPEN) {
        generic = true;
        sc = alloc_perm(p, sizeof(sc_func_generic));
        if (!sc) return PE_FATAL;
        tk_void(&p->tk);
        pe = parse_param_list(
            p, &((sc_func_generic*)sc)->generic_params, true, start, decl_end,
            "in this function declaration");
        if (pe) return pe;
        PEEK(p, t);
    }
    else {
        generic = false;
        sc = alloc_perm(p, sizeof(sc_func));
        if (!sc) return PE_FATAL;
    }
    sc->symbol.name = name;
    sc->parent = p->curr_scope;
    pe = stmt_fill_srange(p, (stmt*)sc, start, decl_end);
    if (pe) return pe;
    if (t->type != TT_PAREN_OPEN) {
        parser_error_2a(
            p, "invalid function declaration syntax", t->start, t->end,
            "expected '(' to start parameter list", start, decl_end,
            "in this function declaration");
        return PE_HANDLED;
    }
    tk_void(&p->tk);
    sym_param** pd =
        generic ? &((sc_func_generic*)sc)->params : &((sc_func*)sc)->params;
    pe = parse_param_list(
        p, pd, false, start, decl_end, "in this function declaration");
    if (pe) return pe;
    sc->symbol.stmt.type = generic ? SC_FUNC_GENERIC : SC_FUNC;
    sc->symbol.stmt.flags = flags;
    *n = (stmt*)sc;
    return parse_scope_body(p, sc);
}
parse_error parse_struct_decl(
    parser* p, stmt_flags flags, ureg start, ureg flags_end, stmt** n)
{
    tk_void(&p->tk);
    token* t;
    parse_error pe;
    PEEK(p, t);
    if (t->type != TT_STRING) {
        parser_error_2a(
            p, "invalid struct declaration syntax", t->start, t->end,
            "expected struct identifier", start, t->end,
            "in this struct declaration");
        return PE_HANDLED;
    }
    ureg decl_end = t->end;
    char* name = alloc_string_perm(p, t->str);
    if (!name) return PE_FATAL;
    tk_void(&p->tk);
    PEEK(p, t);
    scope* st;
    bool generic;
    if (t->type == TT_BRACKET_OPEN) {
        generic = true;
        st = alloc_perm(p, sizeof(sc_struct_generic));
        if (!st) return PE_FATAL;
        tk_void(&p->tk);
        pe = parse_param_list(
            p, &((sc_struct_generic*)st)->generic_params, true, start, decl_end,
            "in this struct declaration");
        if (pe) return pe;
        PEEK(p, t);
    }
    else {
        generic = false;
        st = alloc_perm(p, sizeof(sc_struct));
        if (!st) return PE_FATAL;
    }
    st->symbol.name = name;
    pe = stmt_fill_srange(p, (stmt*)st, start, decl_end);
    if (pe) return pe;
    st->symbol.stmt.type = generic ? SC_STRUCT_GENERIC : SC_STRUCT;
    st->symbol.stmt.flags = flags;
    *n = (stmt*)st;
    return parse_body(p, &st->body);
}
parse_error parse_module_decl(
    parser* p, stmt_flags flags, ureg start, ureg flags_end, stmt** n)
{
    tk_void(&p->tk);
    token *t, *t2;
    parse_error pe;
    PEEK(p, t);
    if (t->type != TT_STRING) {
        parser_error_2a(
            p, "invalid module declaration syntax", t->start, t->end,
            "expected module identifier", start, t->end,
            "in this module declaration");
        return PE_HANDLED;
    }
    ureg decl_end = t->end;

    t2 = tk_peek_2nd(&p->tk);
    if (!t2) return PE_FATAL;
    open_scope* md;
    bool generic;
    if (t2->type == TT_BRACKET_OPEN) {
        generic = true;
        md = alloc_perm(p, sizeof(osc_module_generic));
        if (!md) return PE_FATAL;
        tk_void_n(&p->tk, 2);
        pe = parse_param_list(
            p, &((osc_module_generic*)md)->generic_params, true, start,
            decl_end, "in this module declaration");
        if (pe) return pe;
        PEEK(p, t);
    }
    else {
        generic = false;
        md = alloc_perm(p, sizeof(osc_module));
        if (!md) return PE_FATAL;
        tk_void(&p->tk);
    }
    mdg_node* mdgn =
        mdg_add_open_scope(&TAUC.mdg, p->current_module, md, t->str);
    if (mdgn == NULL) return PE_FATAL;
    pe = stmt_fill_srange(p, (stmt*)md, start, decl_end);
    if (pe) return pe;
    md->scope.symbol.stmt.type = generic ? OSC_MODULE_GENERIC : OSC_MODULE;
    md->scope.symbol.stmt.flags = flags;
    PEEK(p, t);
    mdg_node* parent = p->current_module;
    p->current_module = mdgn;
    if (t->type == TT_SEMICOLON) {
        *n = NULL;
        if (p->curr_scope->body.children == NULL) {
            tk_consume(&p->tk);
            pe = parse_delimited_open_scope(p, md, TT_EOF, TT_BRACE_CLOSE);
        }
        else {
            stmt* head = p->curr_scope->body.children;
            while (head->next != NULL) head = head->next;
            ureg hs = src_range_get_start(head->srange);
            ureg he = src_range_get_end(head->srange);
            parser_error_2a_pc(
                p, "non leading module statement", start, t->end, "", hs, he,
                "preceeded by this statement");
            p->current_module = parent;
            return PE_HANDLED;
        }
    }
    else {
        pe = parse_open_scope_body(p, md, mdgn);
    }
    p->current_module = parent;
    if (*(void**)md->requires == NULL) {
        if (mdg_node_parsed(&TAUC.mdg, mdgn, p->tk.tc)) return PE_FATAL;
    }
    src_range_large srl;
    src_range_unpack(md->scope.symbol.stmt.srange, &srl);
    srl.file = p->tk.file;
    md->scope.symbol.stmt.srange = src_range_pack(p->tk.tc, &srl);
    if (md->scope.symbol.stmt.srange == SRC_RANGE_INVALID) return PE_FATAL;
    *n = (stmt*)md;
    return pe;
}
parse_error parse_extend_decl(
    parser* p, stmt_flags flags, ureg start, ureg flags_end, stmt** n)
{
    tk_void(&p->tk);
    token *t, *t2;
    parse_error pe;
    PEEK(p, t);
    if (t->type != TT_STRING) {
        parser_error_2a(
            p, "invalid extend declaration syntax", t->start, t->end,
            "expected extend identifier", start, t->end,
            "in this extend declaration");
        return PE_HANDLED;
    }
    ureg decl_end = t->end;
    t2 = tk_peek_2nd(&p->tk);
    if (!t2) return PE_TK_ERROR;
    open_scope* ex;
    bool generic;
    if (t2->type == TT_BRACKET_OPEN) {
        generic = true;
        ex = alloc_perm(p, sizeof(osc_extend_generic));
        if (!ex) return PE_FATAL;
        tk_void_n(&p->tk, 2);
        pe = parse_param_list(
            p, &((osc_extend_generic*)ex)->generic_params, true, start,
            decl_end, "in this extend declaration");
        if (pe) return pe;
    }
    else {
        generic = false;
        ex = alloc_perm(p, sizeof(osc_extend));
        if (!ex) return PE_FATAL;
        tk_void(&p->tk);
    }
    mdg_node* mdgn =
        mdg_add_open_scope(&TAUC.mdg, p->current_module, ex, t->str);
    if (mdgn == NULL) return PE_FATAL;
    pe = stmt_fill_srange(p, (stmt*)ex, start, decl_end);
    if (pe) return pe;
    ex->scope.symbol.stmt.type = generic ? OSC_EXTEND_GENERIC : OSC_EXTEND;
    ex->scope.symbol.stmt.flags = flags;
    ex->scope.parent = p->curr_scope;
    PEEK(p, t);
    mdg_node* parent = p->current_module;
    p->current_module = mdgn;
    if (t->type == TT_SEMICOLON) {
        *n = NULL;
        if (p->curr_scope->body.children == NULL) {
            tk_consume(&p->tk);
            pe = parse_delimited_open_scope(p, ex, TT_EOF, TT_BRACE_CLOSE);
        }
        else {
            stmt* head = p->curr_scope->body.children;
            while (head->next != NULL) head = head->next;
            ureg hs = src_range_get_start(head->srange);
            ureg he = src_range_get_end(head->srange);
            parser_error_2a_pc(
                p, "non leading extend statement", start, t->end, "", hs, he,
                "preceeded by this statement");
            p->current_module = parent;
            return PE_HANDLED;
        }
    }
    else {
        pe = parse_open_scope_body(p, ex, mdgn);
    }
    src_range_large srl;
    src_range_unpack(ex->scope.symbol.stmt.srange, &srl);
    srl.file = p->tk.file;
    ex->scope.symbol.stmt.srange = src_range_pack(p->tk.tc, &srl);
    if (ex->scope.symbol.stmt.srange == SRC_RANGE_INVALID) return PE_FATAL;
    p->current_module = parent;
    *n = (stmt*)ex;
    return pe;
}
parse_error parse_trait_decl(
    parser* p, stmt_flags flags, ureg start, ureg flags_end, stmt** n)
{
    tk_void(&p->tk);
    token* t;
    parse_error pe;
    PEEK(p, t);
    if (t->type != TT_STRING) {
        parser_error_2a(
            p, "invalid trait declaration syntax", t->start, t->end,
            "expected trait identifier", start, t->end,
            "in this trait declaration");
        return PE_HANDLED;
    }
    ureg decl_end = t->end;
    char* name = alloc_string_perm(p, t->str);
    if (!name) return PE_FATAL;
    tk_void(&p->tk);
    PEEK(p, t);
    scope* tr;
    bool generic;
    if (t->type == TT_BRACKET_OPEN) {
        generic = true;
        tr = alloc_perm(p, sizeof(sc_trait_generic));
        if (!tr) return PE_FATAL;
        tk_void(&p->tk);
        pe = parse_param_list(
            p, &((sc_trait_generic*)tr)->generic_params, true, start, decl_end,
            "in this trait declaration");
        if (pe) return pe;
        PEEK(p, t);
    }
    else {
        generic = false;
        tr = alloc_perm(p, sizeof(sc_trait));
        if (!tr) return PE_FATAL;
    }
    tr->symbol.name = name;
    pe = stmt_fill_srange(p, (stmt*)tr, start, decl_end);
    if (pe) return pe;
    tr->symbol.stmt.type = generic ? SC_TRAIT_GENERIC : SC_TRAIT;
    tr->symbol.stmt.flags = flags;
    *n = (stmt*)tr;
    return parse_body(p, &tr->body);
}
bool body_supports_exprs(ast_node_type pt)
{
    switch (pt) {
        case SC_FUNC:
        case SC_FUNC_GENERIC:
        case EXPR_LOOP:
        case EXPR_WHILE:
        case EXPR_FOR:
        case EXPR_FOR_EACH:
        case EXPR_IF:
        case EXPR_BLOCK:
        case EXPR_LAMBDA: {
            return true;
        }
        default: {
            return false;
        }
    }
}
bool curr_scope_supports_exprs(parser* p)
{
    return body_supports_exprs(p->curr_scope->symbol.stmt.type);
}
bool body_customizes_exprs(ast_node_type pt)
{
    return pt == EXPR_ARRAY;
}
static inline parse_error parse_compound_assignment_after_equals(
    parser* p, ureg t_start, ureg t_end, expr** elements, stmt** tgt,
    bool had_colon)
{
    stmt_compound_assignment* ca =
        alloc_perm(p, sizeof(stmt_compound_assignment));
    if (!ca) return PE_FATAL;
    ca->elements = elements;
    ca->stmt.type = STMT_COMPOUND_ASSIGN;
    ca->stmt.flags = STMT_FLAGS_DEFAULT;
    ca->stmt.eflags = ERR_FLAGS_DEFAULT;
    if (had_colon) stmt_flags_set_compound_decl(&ca->stmt.flags);
    parse_error pe = parse_expression(p, &ca->value);
    token* t;
    if (pe == PE_EOEX) {
        PEEK(p, t);
        parser_error_2a(
            p, "unexpected token", t->start, t->end, "expected expression",
            t_start, t_end, "in this compound assignment statement");
    }
    if (pe) return pe;
    *tgt = (stmt*)ca;
    return PE_OK;
}
parse_error parse_expr_stmt(parser* p, stmt** tgt)
{
    expr* ex;
    parse_error pe;
    token* t;
    PEEK(p, t);
    ureg t_start = t->start;
    if (t->type == TT_PAREN_OPEN) {
        bool contains_decls = false;
        ureg t_end;
        expr** elems = NULL;
        pe = parse_paren_group_or_tuple_or_compound_decl(
            p, t, &ex, &elems, &contains_decls, &t_end);
        if (pe) return pe;
        if (elems) {
            PEEK(p, t);
            if (t->type == TT_COLON) {
                t = tk_peek_2nd(&p->tk);
                if (!t) return PE_TK_ERROR;
                if (t->type == TT_EQUALS) {
                    tk_void_n(&p->tk, 2);
                    return parse_compound_assignment_after_equals(
                        p, t_start, t->end, elems, tgt, true);
                }
                contains_decls = true;
            }
            if (t->type == TT_EQUALS) {
                tk_void(&p->tk);
                turn_ident_nodes_to_exprs(elems);
                return parse_compound_assignment_after_equals(
                    p, t_start, t->end, elems, tgt, false);
            }
            else {
                if (contains_decls) {
                    parser_error_2a(
                        p, "unexpected token", t->start, t->end,
                        "expected '=' or ':=' to parse compound assignment",
                        t_start, t_end,
                        "after this compound which contains declarations");
                }
                else {
                    turn_ident_nodes_to_exprs(elems);
                    expr_tuple* tp = alloc_perm(p, sizeof(expr_tuple));
                    if (!tp) return PE_FATAL;
                    tp->expr.type = EXPR_TUPLE;
                    if (expr_fill_srange(p, (expr*)tp, t_start, t->end))
                        return PE_FATAL;
                    tp->elements = elems;
                    ex = (expr*)tp;
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
        op_type op = token_to_prefix_unary_op(t);
        if (op != OP_NOOP) {
            pe = parse_prefix_unary_op(p, op, &ex);
            if (pe) return pe;
            pe = parse_expression_of_prec_post_value(p, &ex, PREC_BASELINE);
        }
        else {
            pe = parse_value_expr(p, &ex);
            if (pe) return pe;
            if (!expr_allowed_to_drop_semicolon(ex)) {
                pe = parse_expression_of_prec_post_value(p, &ex, PREC_BASELINE);
            }
        }
    }
    if (pe == PE_EOEX) {
        PEEK(p, t);
        parser_error_1a_pc(
            p, "unexpected token in expression statement", t->start, t->end,
            "");
        return PE_HANDLED;
    }
    if (pe) return pe;
    return expr_to_stmt(p, tgt, ex, t_start, t->end);
}
parse_error
parse_using(parser* p, stmt_flags flags, ureg start, ureg flags_end, stmt** tgt)
{
    token* t = tk_aquire(&p->tk);
    ureg end = t->end;
    tk_void(&p->tk);
    PEEK(p, t);
    if (t->type == TT_STRING) {
        token* t2 = tk_peek_2nd(&p->tk);
        if (!t2) return PE_TK_ERROR;
        if (t2->type == TT_EQUALS) {
            sym_named_using* nu = alloc_perm(p, sizeof(sym_named_using));
            if (!nu) return PE_FATAL;
            nu->symbol.stmt.type = SYM_NAMED_USING;
            nu->symbol.stmt.flags = flags;
            nu->symbol.name = alloc_string_perm(p, t->str);
            if (!nu->symbol.name) return PE_FATAL;
            tk_void_n(&p->tk, 2);
            parse_error pe = parse_expression(p, &nu->target);
            if (pe == PE_EOEX) {
                parser_error_2a(
                    p, "unexpected token", t->start, t->end,
                    "expected expression", start, end,
                    "in this named using statement");
                return PE_HANDLED;
            }
            if (pe) return pe;
            if (stmt_fill_srange(
                    p, (stmt*)nu, start, src_range_get_end(nu->target->srange)))
                return PE_FATAL;
            *tgt = (stmt*)nu;
            return PE_OK;
        }
    }
    stmt_using* u = alloc_perm(p, sizeof(stmt_using));
    if (!u) return PE_FATAL;
    u->stmt.type = STMT_USING;
    u->stmt.flags = flags;
    parse_error pe = parse_expression(p, &u->target);
    if (pe == PE_EOEX) {
        parser_error_2a(
            p, "unexpected token", t->start, t->end, "expected expression",
            start, end, "in this using statement");
        return PE_HANDLED;
    }
    if (stmt_fill_srange(
            p, (stmt*)u, start, src_range_get_end(u->target->srange)))
        return PE_FATAL;
    *tgt = (stmt*)u;
    return pe;
}
parse_error parse_symbol_imports(parser* p, module_import* mi)
{
    mi->selected_symbols =
        (symbol_import*)list_builder_start_blocklist(&p->list_builder);
    token *t, *t2;
    t = tk_aquire(&p->tk);
    ureg start = t->start;
    ureg end;
    tk_void(&p->tk);
    t = tk_peek(&p->tk);
    if (!t) {
        list_builder_drop_list(&p->list_builder, mi->selected_symbols);
        return PE_TK_ERROR;
    }
    if (t->type == TT_PAREN_CLOSE) {
        parser_error_1a(
            p, "empty parenthesized import list not allowed", start, t->end,
            "at least one item required");
        list_builder_drop_list(&p->list_builder, mi->selected_symbols);
        return PE_HANDLED;
    }
    while (true) {
        t2 = tk_peek_2nd(&p->tk);
        if (!t2) return PE_TK_ERROR;
        symbol_import si;
        if (t2->type == TT_EQUALS) {
            si.alias = alloc_string_perm(p, t->str);
            if (!si.alias) {
                list_builder_drop_list(&p->list_builder, mi->selected_symbols);
                return PE_FATAL;
            }
            tk_void_n(&p->tk, 2);
            t = tk_peek(&p->tk);
            if (!t) {
                list_builder_drop_list(&p->list_builder, mi->selected_symbols);
                return PE_TK_ERROR;
            }
        }
        else {
            si.alias = NULL;
        }
        if (t->type != TT_STRING) {
            parser_error_2a(
                p, "invalid identifier for named import", t->start, t->end,
                "identifier must be a string", t2->start, t2->end,
                "named import begins here");
            list_builder_drop_list(&p->list_builder, mi->selected_symbols);
            return PE_HANDLED;
        }
        si.symbol_name = alloc_string_perm(p, t->str);
        if (!si.symbol_name) return PE_FATAL;
        list_builder_add_block(&p->list_builder, &si, sizeof(si));
        end = t->end;
        tk_void(&p->tk);
        t = tk_peek(&p->tk);
        if (!t) {
            list_builder_drop_list(&p->list_builder, mi->selected_symbols);
            return PE_TK_ERROR;
        }
        if (t->type == TT_PAREN_CLOSE) break;
        if (t->type != TT_COMMA) {
            parser_error_2a(
                p, "unexpected token in parenthesized import list", t->start,
                t->end, "expected ',' or ')'", start, end,
                "in this parenthesized import list");
            list_builder_drop_list(&p->list_builder, mi->selected_symbols);
            return PE_HANDLED;
        }
        tk_void(&p->tk);
        t = tk_peek(&p->tk);
        if (!t) {
            list_builder_drop_list(&p->list_builder, mi->selected_symbols);
            return PE_TK_ERROR;
        }
        if (t->type == TT_PAREN_CLOSE) break;
    }
    tk_void(&p->tk);
    mi->selected_symbols = (symbol_import*)list_builder_pop_block_list_zt(
        &p->list_builder, mi->selected_symbols, &p->tk.tc->permmem);
    if (!mi->selected_symbols) return PE_FATAL;
    if (mdg_node_add_dependency(p->current_module, mi->tgt, p->tk.tc)) {
        return PE_FATAL;
    }
    return PE_OK;
}
parse_error parse_single_import(
    parser* p, mdg_node* parent, stmt_import* stmt, module_import* mi);
parse_error parse_braced_imports(
    parser* p, stmt_import* stmt, module_import* mi, ureg start)
{
    mi->nested_imports =
        (module_import*)list_builder_start_blocklist(&p->list_builder);
    parse_error pe;
    tk_void(&p->tk);
    token* t = tk_peek(&p->tk);
    if (!t) {
        list_builder_drop_list(&p->list_builder, mi->nested_imports);
        return PE_TK_ERROR;
    }
    if (t->type == TT_BRACE_CLOSE) {
        parser_error_1a(
            p, "empty braced import list not allowed", start, t->end,
            "at least one item required");
        list_builder_drop_list(&p->list_builder, mi->nested_imports);
        return PE_HANDLED;
    }
    while (true) {
        module_import m;
        pe = parse_single_import(p, mi->tgt, stmt, &m);
        if (pe) return pe;
        list_builder_add_block(&p->list_builder, &m, sizeof(m));
        t = tk_peek(&p->tk);
        if (!t) {
            list_builder_drop_list(&p->list_builder, mi->nested_imports);
            return PE_TK_ERROR;
        }
        if (t->type == TT_BRACE_CLOSE) break;
        if (t->type != TT_COMMA) {
            parser_error_1a(
                p, "unexpected token in braced import list", start, t->end,
                "expected ',' or '}'");
            list_builder_drop_list(&p->list_builder, mi->nested_imports);
            return PE_HANDLED;
        }
        tk_void(&p->tk);
        t = tk_peek(&p->tk);
        if (!t) {
            list_builder_drop_list(&p->list_builder, mi->nested_imports);
            return PE_TK_ERROR;
        }
        if (t->type == TT_BRACE_CLOSE) break;
    }
    mi->nested_imports = (module_import*)list_builder_pop_block_list_zt(
        &p->list_builder, mi->nested_imports, &p->tk.tc->permmem);
    if (!mi->nested_imports) return PE_FATAL;
    mi->srange = src_range_pack_lines(p->tk.tc, start, t->end);
    if (mi->srange == SRC_RANGE_INVALID) return PE_FATAL;
    tk_void(&p->tk);
    /*if (mdg_node_add_dependency(p->current_module, mi->tgt, p->tk.tc)) {
        return PE_FATAL;
    }
    */
    return PE_OK;
}
parse_error parse_single_import(
    parser* p, mdg_node* parent, stmt_import* stmt, module_import* mi)
{
    token *t, *t2;
    mi->statement = stmt;
    mi->selected_symbols = NULL;
    mi->nested_imports = NULL;
    PEEK(p, t);
    ureg start = t->start;
    ureg end = t->end;
    t2 = tk_peek_2nd(&p->tk);
    if (!t2) return PE_TK_ERROR;
    if (t2->type == TT_EQUALS) {
        if (t->type != TT_STRING) {
            parser_error_2a(
                p, "invalid import syntax", t->start, t->end,
                "expected identifier", start, end, "in this import statement");
            return PE_HANDLED;
        }
        mi->name = alloc_string_perm(p, t->str);
        if (!mi->name) return PE_FATAL;
        tk_void_n(&p->tk, 2);
        PEEK(p, t);
    }
    else {
        mi->name = NULL;
    }
    mi->tgt = parent;
    while (true) {
        if (t->type == TT_BRACE_OPEN) {
            if (mi->name != NULL) {
                parser_error_1a(
                    p, "invalid import syntax", start, t->end,
                    "named import can't be braced import");
                return PE_HANDLED;
            }
            return parse_braced_imports(p, stmt, mi, start);
        }
        else if (t->type == TT_PAREN_OPEN) {
            return parse_symbol_imports(p, mi);
        }
        else if (t->type == TT_STAR) {
            mi->selected_symbols = (symbol_import*)NULL_PTR_PTR;
            end = t->end;
            tk_void(&p->tk);
            break;
        }
        else if (t->type == TT_STRING) {
            mi->tgt = mdg_get_node(&TAUC.mdg, mi->tgt, t->str);
            if (!mi->tgt) return PE_FATAL;
            end = t->end;
            tk_void(&p->tk);
            PEEK(p, t);
            if (t->type != TT_DOT) break;
            tk_void(&p->tk);
            PEEK(p, t);
        }
        else {
            char* expected =
                mi->name ? "expected module identifier"
                         : " expected module identifier or '{' or '(' or '*' ";
            parser_error_2a(
                p, "invalid import syntax", t->start, t->end, expected, start,
                end, "in this import statement");
            return PE_HANDLED;
        }
    }
    t = tk_peek(&p->tk);
    mi->srange = src_range_pack_lines(p->tk.tc, start, end);
    if (mi->srange == SRC_RANGE_INVALID) return PE_FATAL;
    if (mdg_node_add_dependency(p->current_module, mi->tgt, p->tk.tc)) {
        return PE_FATAL;
    }
    return PE_OK;
}
parse_error parse_import(
    parser* p, stmt_flags flags, ureg start, ureg flags_end, stmt** tgt)
{
    tk_void(&p->tk);
    stmt_import* si = alloc_perm(p, sizeof(stmt_import));
    if (!si) return PE_FATAL;
    stmt_init((stmt*)si, STMT_IMPORT);
    parse_error pe =
        parse_single_import(p, TAUC.mdg.root_node, si, &si->module_import);
    if (pe) return pe;
    ureg end = src_range_get_end(si->module_import.srange);
    pe = stmt_fill_srange(p, (stmt*)si, start, end);
    if (pe) return PE_FATAL;
    *tgt = (stmt*)si;
    return PE_OK;
}
parse_error parse_require(parser* p)
{
    token* t = tk_aquire(&p->tk);
    ureg start = t->start;
    ureg end = t->end;
    tk_void(&p->tk);
    if (!scope_is_open(p->curr_scope)) {
        parser_error_1a_pc(
            p, "invalid scope for require statement", t->start, t->end,
            "require statement only allowed at module scope");
        return PE_HANDLED;
    }
    PEEK(p, t);
    if (t->type != TT_LITERAL) {
        parser_error_2a(
            p, "unexpected token", t->start, t->end,
            "expected path pattern as string literal", start, end,
            "in this require statement");
        return PE_HANDLED;
    }
    src_file* f = file_map_get_file_from_relative_path(
        &TAUC.file_map, p->tk.file->head.parent, t->str);
    tk_void(&p->tk);
    PEEK(p, t);
    if (t->type != TT_SEMICOLON) {
        report_missing_semicolon(p, start, end);
        return PE_HANDLED;
    }
    file_require rq;
    rq.file = f;
    rq.srange = src_range_pack_lines(p->tk.tc, start, t->end);
    if (rq.srange == SRC_RANGE_INVALID) return PE_FATAL;
    end = t->end;
    tk_void(&p->tk);
    rwslock_read(&p->current_module->stage_lock);
    bool needed = (p->current_module->stage != MS_UNNEEDED);
    int r = list_builder_add_block(&p->list_builder, &rq, sizeof(rq));
    rwslock_end_read(&p->current_module->stage_lock);
    if (r) return PE_FATAL;
    if (needed) {
        int r = src_file_require(f, p->tk.file, rq.srange, p->current_module);
        if (r == ERR) return PE_FATAL;
        if (r != SF_ALREADY_PARSED) {
            atomic_ureg_inc(&p->current_module->unparsed_files);
        }
    }
    return PE_OK;
}
parse_error
parse_label(parser* p, stmt_flags flags, ureg start, ureg flags_end, stmt** tgt)
{
    token* t = tk_aquire(&p->tk);
    parse_error pe = require_default_flags(p, t, flags, start, flags_end);
    if (pe) return pe;
    token* t3 = tk_peek_3rd(&p->tk);
    if (t3->type == TT_SEMICOLON) {
        token* t2 = tk_peek_2nd(&p->tk);
        if (t2->type != TT_STRING) {
            parser_error_2a(
                p, "missing label name", t2->start, t2->end,
                "expected label name", t->start, t->end,
                "in this label statement");
            return PE_HANDLED;
        }
        char* label_name = alloc_string_temp(p, t2->str);
        if (!label_name) return PE_FATAL;
        tk_void_n(&p->tk, 2);
        sym_label* g = alloc_perm(p, sizeof(sym_label));
        g->symbol.stmt.type = SYM_LABEL;
        g->symbol.name = label_name;
        if (stmt_fill_srange(p, (stmt*)g, start, t2->end)) return PE_FATAL;
        *tgt = (stmt*)g;
        return PE_OK;
    }
    else {
        return parse_expr_stmt(p, tgt);
    }
}
static inline parse_error stmt_flags_from_kw(
    parser* p, stmt_flags* f, token_type kw, ureg start, ureg end)
{
    // TODO: enforce order
    switch (kw) {
        case TT_KW_PRIVATE:
            return stmt_flags_from_kw_set_access_mod(
                p, f, AM_PRIVATE, start, end);
        case TT_KW_PROTECTED:
            return stmt_flags_from_kw_set_access_mod(
                p, f, AM_PROTECTED, start, end);
        case TT_KW_PUBLIC:
            return stmt_flags_from_kw_set_access_mod(
                p, f, AM_PUBLIC, start, end);
        case TT_KW_CONST: {
            if (stmt_flags_get_const(*f) != false) {
                report_redundant_specifier(
                    p, token_strings[TT_KW_CONST], start, end);
                return PE_UNEXPECTED_TOKEN;
            }
            stmt_flags_set_const(f);
        } break;
        case TT_KW_SEALED: {
            if (stmt_flags_get_sealed(*f) != false) {
                report_redundant_specifier(
                    p, token_strings[TT_KW_SEALED], start, end);
                return PE_UNEXPECTED_TOKEN;
            }
            stmt_flags_set_sealed(f);
        } break;
        case TT_KW_VIRTUAL: {
            if (stmt_flags_get_virtual(*f) != false) {
                report_redundant_specifier(
                    p, token_strings[TT_KW_VIRTUAL], start, end);
                return PE_UNEXPECTED_TOKEN;
            }
            stmt_flags_set_virtual(f);
        } break;
        case TT_KW_STATIC: {
            if (stmt_flags_get_static(*f) != false) {
                report_redundant_specifier(
                    p, token_strings[TT_KW_STATIC], start, end);
                return PE_UNEXPECTED_TOKEN;
            }
            stmt_flags_set_static(f);
        } break;
        default: {
            return PE_EOEX;
        } break;
    }
    return PE_OK;
}
parse_error parse_statement(parser* p, stmt** tgt)
{
    parse_error pe;
    stmt_flags flags = STMT_FLAGS_DEFAULT;
    token* t;
    PEEK(p, t);
    ureg start = t->start;
    ureg flags_end = t->start;

    while (true) {
        pe = stmt_flags_from_kw(p, &flags, t->type, start, t->end);
        if (pe == PE_OK) {
            flags_end = t->end;
            tk_void(&p->tk);
            PEEK(p, t);
            continue;
        }
        if (pe != PE_EOEX) return pe;
        switch (t->type) {
            case TT_KW_FUNC:
                return parse_func_decl(p, flags, start, flags_end, tgt);
            case TT_KW_STRUCT:
                return parse_struct_decl(p, flags, start, flags_end, tgt);
            case TT_KW_TRAIT:
                return parse_trait_decl(p, flags, start, flags_end, tgt);
            case TT_KW_MODULE:
                return parse_module_decl(p, flags, start, flags_end, tgt);
            case TT_KW_EXTEND:
                return parse_extend_decl(p, flags, start, flags_end, tgt);
            case TT_KW_USING:
                return parse_using(p, flags, start, flags_end, tgt);
            case TT_KW_REQUIRE:
                pe = require_default_flags(p, t, flags, start, flags_end);
                if (pe) return pe;
                pe = parse_require(p);
                if (pe) return pe;
                return parse_statement(p, tgt);
            case TT_KW_IMPORT:
                return parse_import(p, flags, start, flags_end, tgt);
            case TT_KW_LABEL:
                return parse_label(p, flags, start, flags_end, tgt);
            case TT_KW_GIVE:
                return parse_give_stmt(p, flags, start, flags_end, tgt);
            case TT_KW_BREAK:
                return parse_break_stmt(p, flags, start, flags_end, tgt);
            case TT_KW_CONTINUE:
                return parse_continue_stmt(p, flags, start, flags_end, tgt);
            case TT_KW_RETURN:
                return parse_return_stmt(p, flags, start, flags_end, tgt);
            case TT_KW_GOTO:
                return parse_goto_stmt(p, flags, start, flags_end, tgt);
            case TT_STRING: {
                token* t2 = tk_peek_2nd(&p->tk);
                if (!t2) return PE_TK_ERROR;
                if (t2->type == TT_COLON) {
                    return parse_var_decl(p, flags, start, flags_end, tgt);
                }
                if (curr_scope_supports_exprs(p)) {
                    pe = require_default_flags(p, t, flags, start, flags_end);
                    if (pe) return pe;
                    return parse_expr_stmt(p, tgt);
                }
                t = tk_peek_2nd(&p->tk);
                parser_error_1a(
                    p, "unexpected token in statement", t->start, t->end,
                    "expected ':' to initiate a declaration");
                return PE_UNEXPECTED_TOKEN;
            }
            default: {
                if (curr_scope_supports_exprs(p)) {
                    pe = require_default_flags(p, t, flags, start, flags_end);
                    if (pe) return pe;
                    pe = parse_expr_stmt(p, tgt);
                    if (pe == PE_EOEX) {
                        PEEK(p, t);
                        parser_error_1a(
                            p, "unexpected token while expecting a statement",
                            t->start, t->end, "expected begin of statement");
                        return PE_HANDLED;
                    }
                    return pe;
                }
                parser_error_1a_pc(
                    p, "unexpected token", t->start, t->end,
                    "expected a declaration");
                return PE_HANDLED;
            }
        }
    }
}
parse_error parse_brace_delimited_body(parser* p, body* b)
{
    token* t = tk_aquire(&p->tk);
    ureg bstart = t->start;
    ureg bend = t->end;
    tk_void(&p->tk);
    parse_error pe = PE_OK;
    PEEK(p, t);
    stmt** head = &b->children;
    if (pe) return pe;
    while (t->type != TT_BRACE_CLOSE) {
        if (t->type != TT_EOF) {
            pe = parse_statement(p, head);
            if (pe) break;
            pe = handle_semicolon_after_statement(p, *head);
            if (pe) break;
            head = &(*head)->next;
            t = tk_peek(&p->tk);
            if (!t) {
                pe = PE_TK_ERROR;
                break;
            }
        }
        else {
            parser_error_2a_pc(
                p, "unterminated scope", t->start, t->end,
                "reached EOF before scope was closed", bstart, bend,
                "scope starts here");
            return PE_UNEXPECTED_TOKEN;
        }
    }
    if (!pe) {
        tk_consume(&p->tk);
        b->srange = src_range_pack_lines(p->tk.tc, bstart, t->end);
        if (b->srange == SRC_RANGE_INVALID) return PE_FATAL;
    }
    *head = NULL;
    return pe;
}

parse_error parse_scope_body(parser* p, scope* s)
{
    scope* old_scope = p->curr_scope;
    p->curr_scope = s;
    parse_error pe = parse_body(p, &s->body);
    p->curr_scope = old_scope;
    return pe;
}
parse_error parse_open_scope_body(parser* p, open_scope* s, mdg_node* m)
{
    mdg_node* parent = p->current_module;
    p->current_module = m;
    s->requires = (file_require*)list_builder_start_blocklist(&p->list_builder);
    parse_error pe = parse_scope_body(p, (scope*)s);
    s->requires = (file_require*)list_builder_pop_block_list_zt(
        &p->list_builder, s->requires, &p->tk.tc->permmem);
    p->current_module = parent;
    return pe;
}
parse_error parse_body(parser* p, body* b)
{
    parse_error pe;
    token* t;
    PEEK(p, t);
    if (t->type != TT_BRACE_OPEN) {
        pe = parse_statement(p, &b->children);
        if (!pe) {
            b->children->next = NULL;
            b->srange = b->children->srange;
        }
    }
    else {
        pe = parse_brace_delimited_body(p, b);
    }
    return pe;
}
