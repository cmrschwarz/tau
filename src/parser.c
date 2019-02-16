#include "parser.h"
#include "error_log.h"
#include "keywords.h"
#include "print_ast.h"
#include "tauc.h"
#include "tokenizer.h"
#include "utils/math_utils.h"
#include "utils/panic.h"
#include <stddef.h>

#define PEEK(p, t)                                                             \
    do {                                                                       \
        t = tk_peek(&(p)->tk);                                                 \
        if (!t) return PE_TK_ERROR;                                            \
    } while (false)

typedef enum body_parser_mode {
    BPM_FILE,
    BPM_MODULE,
    BPM_GENERIC_MODULE,
    BPM_EXTEND,
    BPM_GENERIC_EXTEND,
    BPM_STRUCT,
    BPM_GENERIC_STRUCT,
    BPM_TRAIT,
    BPM_GENERIC_TRAIT,
    BPM_FUNCTION,
    BPM_GENERIC_FUNCTION,
    BPM_LAMBDA,
    BPM_LOOP,
} body_parser_mode;

parse_error parse_statement(parser* p, stmt** tgt, body_parser_mode bpm);
parse_error parse_body(
    parser* p, named_stmt* parent, stmt** tgt, body_parser_mode bpm, ureg* end);
parse_error parse_expression(parser* p, astn** ex);
parse_error parse_expression_of_prec(parser* p, astn** ex, ureg prec);

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

static inline bool is_left_associative(astnt t)
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
        case TT_DOUBLE_COLON: return OP_SCOPE_ACCESS;
        default: return OP_NOOP;
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
        case TT_STRING:
            switch (kw_match(t->str)) {
                case KW_CONST: return OP_CONST;
                default: return OP_NOOP;
            }
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

static inline void* alloc_ppool(parser* p, ureg size, pool* pool)
{
    void* mem = pool_alloc(pool, size);
    if (!mem) error_log_report_allocation_failiure(&p->tk.tc->error_log);
    return mem;
}
static inline void* alloc_stage(parser* p, ureg size)
{
    return alloc_ppool(p, size, &p->tk.tc->stagemem);
}
static inline void* alloc_perm(parser* p, ureg size)
{
    return alloc_ppool(p, size, &p->tk.tc->permmem);
}
static inline char* alloc_string_ppool(parser* p, string s, pool* pool)
{
    ureg len = string_len(s);
    char* mem = (char*)alloc_ppool(p, align_size(len + 1, sizeof(void*)), pool);
    if (!mem) return NULL;
    memcpy(mem, s.start, len);
    mem[len] = '\0';
    return mem;
}
static inline char* alloc_string_stage(parser* p, string s)
{
    return alloc_string_ppool(p, s, &p->tk.tc->stagemem);
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
char* bpm_to_context_msg(body_parser_mode bpm)
{
    switch (bpm) {
        case BPM_FILE: return NULL;
        case BPM_FUNCTION: return "in this function's body";
        case BPM_GENERIC_FUNCTION: return "in this generic function's body";
        case BPM_STRUCT: return "in this struct's body";
        case BPM_GENERIC_STRUCT: return "in this generic struct's body";
        case BPM_TRAIT: return "in this struct's body";
        case BPM_GENERIC_TRAIT: return "in this generic struct's body";
        case BPM_MODULE: return "in this module's body";
        case BPM_GENERIC_MODULE: return "in this generic module's body";
        case BPM_EXTEND: return "in this extend statement's body";
        case BPM_LOOP: return "in this loop's body";
        case BPM_GENERIC_EXTEND:
            return "in this generic extend statement's body";
        case BPM_LAMBDA: return "in this lambda's body";
        default: panic("unexpected bpm");
    }
    return NULL;
}
static inline void parser_error_1a_bpm(
    parser* p, char* msg, ureg start, ureg end, char* annot,
    body_parser_mode bpm)
{
    char* bpmmsg = bpm_to_context_msg(bpm);
    if (bpmmsg != NULL) {
        src_range_large sr;
        src_range_unpack(p->curr_parent->decl_range, &sr);
        parser_error_2a(p, msg, start, end, annot, sr.start, sr.end, bpmmsg);
    }
    else {
        parser_error_1a(p, msg, start, end, annot);
    }
}
static inline void parser_error_2a_bpm(
    parser* p, char* msg, ureg start, ureg end, char* annot, ureg start2,
    ureg end2, char* annot2, body_parser_mode bpm)
{
    char* bpmmsg = bpm_to_context_msg(bpm);
    if (bpmmsg != NULL) {
        src_range_large sr;
        src_range_unpack(p->curr_parent->decl_range, &sr);
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
int parser_init(parser* p, thread_context* tc)
{
    int r = tk_init(&p->tk, tc);
    if (r) return r;
    r = iht_init_with_capacity(
        &p->iht,
        allocator_get_segment_size(), //*8-->32KiB
        &p->tk.tc->tal);
    if (r) {
        tk_fin(&p->tk);
        return r;
    }
    r = list_builder_init(&p->lb, &p->tk.tc->stagemem, 64);
    if (r) {
        iht_fin(&p->iht);
        tk_fin(&p->tk);
        return r;
    }
    p->root.nstmt.name = NULL;
    p->root.nstmt.parent = NULL;
    p->root.nstmt.stmt.type = ASTNT_MODULE;
    p->root.body = NULL;
    p->root.nstmt.stmt.next = NULL;
    p->curr_parent = (named_stmt*)&p->root;
    p->root_head = &p->root.body;
    return OK;
}
void parser_fin(parser* p)
{
    tk_fin(&p->tk);
}
static inline parse_error
nstmt_fill_srange(parser* p, named_stmt* nstmt, ureg start, ureg end)
{
    nstmt->decl_range = src_range_pack_lines(p->tk.tc, start, end);
    if (nstmt->decl_range == SRC_RANGE_INVALID) return PE_INSANE;
    return PE_OK;
}
static inline parse_error
expr_fill_srange(parser* p, expr* ex, ureg start, ureg end)
{
    ex->srange = src_range_pack_lines(p->tk.tc, start, end);
    if (ex->srange == SRC_RANGE_INVALID) return PE_INSANE;
    return PE_OK;
}
static inline astn* parse_str_value(parser* p, token* t)
{
    astnt ent;
    switch (t->type) {
        case TT_BINARY_LITERAL: ent = ENT_BINARY_LITERAL; break;
        case TT_LITERAL: ent = ENT_STRING_LITERAL; break;
        case TT_NUMBER: ent = ENT_NUMBER; break;
        case TT_STRING: ent = ENT_IDENTIFIER; break;
        default: return NULL;
    }
    expr_str_value* sv = (expr_str_value*)alloc_perm(p, sizeof(expr_str_value));
    if (!sv) return NULL;
    sv->ex.type = ent;
    sv->value = alloc_string_stage(p, t->str);
    if (!sv->value) return NULL;
    if (expr_fill_srange(p, &sv->ex, t->start, t->end)) return NULL;
    return (astn*)sv;
}
parse_error parse_param_decl(
    parser* p, named_stmt* parent, stmt_param_decl** tgt, ureg ctx_start,
    ureg ctx_end, char* msg_context)
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
    stmt_param_decl* d = alloc_perm(p, sizeof(stmt_param_decl));
    if (!d) return PE_INSANE;
    d->nstmt.name = alloc_string_perm(p, t->str);
    if (!d->nstmt.name) return PE_INSANE;
    d->nstmt.stmt.type = ASTNT_PARAM_DECL;
    d->nstmt.parent = parent;
    // TODO: flags parsing
    d->nstmt.stmt.flags = ASTN_FLAGS_DEFAULT;
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
    iht_insert(&p->iht, (named_stmt*)d);
    *tgt = d;
    return PE_OK;
}
parse_error parse_expression_node_list(
    parser* p, expr_list* tgt, char* type, token_type expected_trailer)
{
    token* t;
    PEEK(p, t);
    if (t->type == expected_trailer) {
        tgt->end_ptr = NULL;
        return PE_OK;
    }
    void** list_start = list_builder_start(&p->lb);
    while (true) {
        astn* ex;
        parse_error pe = parse_expression(p, &ex);
        if (pe == PE_EOEX) {
            PEEK(p, t);
            char* msg = error_log_cat_strings_3(
                &p->tk.tc->error_log, "invalid ", type, " syntax");
            if (!msg) return PE_INSANE;
            parser_error_1a(
                p, msg, t->start, t->end, "expected expression after ','");
            return PE_HANDLED;
        }
        if (pe != PE_OK) return pe;
        int r = list_builder_add(&p->lb, (void*)ex);
        if (r) return PE_INSANE;
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
    ureg count;
    void** start = list_builder_pop_list(
        &p->lb, list_start, (void***)&tgt->end_ptr, &count, &p->tk.tc->permmem,
        sizeof(astn**), 0);
    if (!start) return PE_INSANE;
    *tgt->end_ptr = ptradd(start, count * sizeof(void*));
    return PE_OK;
}
static inline parse_error parse_tuple(parser* p, token* t, astn** ex)
{
    ureg t_start = t->start;
    ureg t_end = t->end;
    tk_void(&p->tk);
    expr_tuple* tp = alloc_perm(p, sizeof(expr_tuple));
    if (!tp) return PE_INSANE;
    tp->ex.type = ENT_TUPLE;
    parse_error pe =
        parse_expression_node_list(p, &tp->elements, "tuple", TT_BRACKET_CLOSE);
    // EMSG: suboptimal e.g. for case [,,]
    if (pe == PE_UNEXPECTED_TOKEN) {
        PEEK(p, t);
        parser_error_2a(
            p, "unclosed tuple", t->start, t->end,
            "reached end of expression due to unexpected token", t_start, t_end,
            "didn't find a matching bracket for this tuple");
        return PE_HANDLED;
    }
    if (pe != PE_OK) return pe;
    PEEK(p, t);
    if (expr_fill_srange(p, &tp->ex, t_start, t->end)) return PE_INSANE;
    tk_void(&p->tk);
    *ex = (astn*)tp;
    return PE_OK;
}
static inline parse_error parse_array(parser* p, token* t, astn** ex)
{
    ureg t_start = t->start;
    ureg t_end = t->end;
    tk_void(&p->tk);
    expr_array* arr = alloc_perm(p, sizeof(expr_array));
    if (!arr) return PE_INSANE;
    arr->ex.type = ENT_ARRAY;
    parse_error pe =
        parse_expression_node_list(p, &arr->elements, "array", TT_BRACE_CLOSE);
    // EMSG: suboptimal e.g. for case {,,}
    if (pe == PE_UNEXPECTED_TOKEN) {
        PEEK(p, t);
        parser_error_2a(
            p, "unclosed array", t->start, t->end,
            "reached end of expression due to unexpected token", t_start, t_end,
            "didn't find a matching brace for this array");
        return PE_HANDLED;
    }
    if (pe != PE_OK) return pe;
    PEEK(p, t);
    if (expr_fill_srange(p, &arr->ex, t_start, t->end)) return PE_INSANE;
    tk_void(&p->tk);
    *ex = (astn*)arr;
    return PE_OK;
}
static inline parse_error
parse_parenthesis_group(parser* p, token* t, astn** ex)
{
    ureg t_start = t->start;
    ureg t_end = t->end;
    tk_void(&p->tk);
    parse_error pe = parse_expression_of_prec(p, ex, PREC_BASELINE);
    if (pe != PE_OK && pe != PE_EOEX) return pe;
    PEEK(p, t);
    if (t->type != TT_PAREN_CLOSE) {
        parser_error_2a(
            p, "parenthesis missmatch", t->start, t->end,
            "reached end of expression", t_start, t_end,
            "didn't find a match for this parenthesis");
        return PE_HANDLED;
    }
    if (pe == PE_EOEX) {
        parser_error_2a(
            p, "empty parenthesis pair", t->start, t->end,
            "found closing parenthesis", t_start, t_end,
            "expected an evaluable expression");
        return PE_HANDLED;
    }
    expr_parentheses* pr =
        (expr_parentheses*)alloc_perm(p, sizeof(expr_parentheses));
    if (!pr) return PE_INSANE;
    pr->ex.type = ENT_OP_PARENTHESES;
    pr->child = *ex;
    // TODO fixme
    if (expr_fill_srange(p, &pr->ex, t_start, t->end)) return PE_INSANE;
    tk_void(&p->tk);
    *ex = (astn*)pr;
    return PE_OK;
}
static inline parse_error
parse_prefix_unary_op(parser* p, token* t, astnt op, astn** ex)
{
    expr_op_unary* ou = (expr_op_unary*)alloc_perm(p, sizeof(expr_op_unary));
    if (!ou) return PE_INSANE;
    if (expr_fill_srange(p, &ou->ex, t->start, t->end)) return PE_INSANE;
    tk_void(&p->tk);
    ou->ex.type = ENT_OP_UNARY;
    ou->ex.op_type = op;
    parse_error pe = parse_expression_of_prec(
        p, &ou->child, op_precedence[op] + is_left_associative(op));
    if (pe) {
        PEEK(p, t);
        if (pe == PE_EOEX) {
            src_range_large s;
            src_range_unpack(ou->ex.srange, &s);
            parser_error_2a(
                p, "missing operand for unary operator", t->start, t->end,
                "reached end of expression due to unexpected token", s.start,
                s.end, "missing operand for this operator");
        }
        return PE_HANDLED;
    }
    *ex = (astn*)ou;
    return PE_OK;
}
parse_error parse_loop(parser* p, astn** tgt, ureg start, ureg end, char* label)
{
    expr_loop* l = alloc_perm(p, sizeof(expr_loop));
    if (!l) return PE_INSANE;
    l->nstmt.decl_range = src_range_pack_lines(p->tk.tc, start, end);
    l->nstmt.name = label;
    l->nstmt.parent = p->curr_parent;
    l->nstmt.stmt.type = ASTNT_LOOP;
    *tgt = (astn*)l;
    return parse_body(
        p, &l->nstmt, &l->nstmt.stmt.next, BPM_LOOP, &l->block_end);
}
static inline parse_error parse_single_value(parser* p, token* t, astn** ex)
{
    switch (t->type) {
        case TT_PAREN_OPEN: {
            return parse_parenthesis_group(p, t, ex);
        }
        case TT_BRACKET_OPEN: {
            return parse_tuple(p, t, ex);
        }
        case TT_BRACE_OPEN: {
            return parse_array(p, t, ex);
        }
        case TT_STRING: {
            keyword_id kw = kw_match(t->str);
            switch (kw) {
                case KW_LOOP: {
                    tk_void(&p->tk);
                    return parse_loop(p, ex, t->start, t->end, NULL);
                }
                case KW_INVALID_KEYWORD: // fallthrough
                default:;                // noop to please C
            }
        } // fallthrough for str_value
        case TT_NUMBER:
        case TT_LITERAL:
        case TT_BINARY_LITERAL: {
            *ex = parse_str_value(p, t);
            if (!*ex) return PE_INSANE;
            tk_void(&p->tk);
            return PE_OK;
        } break;
        default: {
            return PE_EOEX; // investigate: shouldn't this be unexp. tok?
        } break;
    }
}
static inline parse_error parse_call(parser* p, token* t, astn** ex, astn* lhs)
{
    ureg t_start = t->start;
    tk_void(&p->tk);
    expr_call* call = alloc_perm(p, sizeof(expr_call));
    if (!call) return PE_INSANE;
    parse_error pe =
        parse_expression_node_list(p, &call->args, "call", TT_PAREN_CLOSE);
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
    if (expr_fill_srange(p, &call->ex, t_start, t->end)) return PE_INSANE;
    tk_void(&p->tk);
    call->ex.type = ENT_OP_CALL;
    call->ex.op_type = OP_CALL;
    call->lhs = lhs;
    *ex = (astn*)call;
    return PE_OK;
}
static inline parse_error
parse_access(parser* p, token* t, astn** ex, astn* lhs)
{
    ureg t_start = t->start;
    tk_void(&p->tk);
    expr_access* acc = alloc_perm(p, sizeof(expr_access));
    if (!acc) return PE_INSANE;
    parse_error pe = parse_expression_node_list(
        p, &acc->args, "access operator", TT_BRACKET_CLOSE);
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
    if (expr_fill_srange(p, &acc->ex, t_start, t->end)) return PE_INSANE;
    tk_void(&p->tk);
    acc->ex.type = ENT_OP_ACCESS;
    acc->ex.op_type = OP_ACCESS;
    acc->lhs = lhs;
    *ex = (astn*)acc;
    return PE_OK;
}
static inline parse_error
parse_postfix_unary_op(parser* p, token* t, op_type op, astn** ex, astn* lhs)
{
    if (op == OP_CALL) {
        return parse_call(p, t, ex, lhs);
    }
    else if (op == OP_ACCESS) {
        return parse_access(p, t, ex, lhs);
    }
    else {
        tk_void(&p->tk);
        expr_op_unary* ou =
            (expr_op_unary*)alloc_perm(p, sizeof(expr_op_unary));
        if (!ou) return PE_INSANE;
        ou->ex.type = ENT_OP_UNARY;
        ou->ex.op_type = op;
        ou->child = lhs;
        if (expr_fill_srange(p, &ou->ex, t->start, t->end)) return PE_INSANE;
        *ex = (astn*)ou;
        return PE_OK;
    }
}
static inline parse_error
parse_binary_op(parser* p, token* t, op_type op, astn** ex, astn* lhs)
{
    tk_void(&p->tk);
    expr_op_binary* ob = (expr_op_binary*)alloc_perm(p, sizeof(expr_op_binary));
    if (!ob) return PE_INSANE;
    if (expr_fill_srange(p, &ob->ex, t->start, t->end)) return PE_INSANE;
    ob->ex.type = ENT_OP_BINARY;
    ob->ex.op_type = op;
    parse_error pe = parse_expression_of_prec(
        p, &ob->rhs, op_precedence[op] + is_left_associative(op));
    if (pe) {
        if (pe == PE_EOEX) {
            PEEK(p, t);
            src_range_large sr;
            src_range_unpack(ob->ex.srange, &sr);
            parser_error_2a(
                p, "missing operand for infix operator", t->start, t->end,
                "reached end of expression", sr.start, sr.end,
                "missing operand for this operator");
            return PE_HANDLED;
        }
        return pe;
    }
    ob->lhs = *ex;
    *ex = (astn*)ob;
    return PE_OK;
}
parse_error parse_expression_of_prec(parser* p, astn** ex, ureg prec)
{
    token* t;
    PEEK(p, t);
    *ex = NULL;
    parse_error pe;
    // parse one prefix op(recursive) or a plain value
    op_type op = token_to_prefix_unary_op(t);
    if (op != OP_NOOP) {
        pe = parse_prefix_unary_op(p, t, op, ex);
        if (pe) return pe;
    }
    else {
        pe = parse_single_value(p, t, ex);
        if (pe) return pe;
    }
    // parse arbitrarily many postfix operators
    while (true) {
        PEEK(p, t);
        op = token_to_postfix_unary_op(t);
        if (op == OP_NOOP) break;
        if (op_precedence[op] < prec) return PE_OK;
        pe = parse_postfix_unary_op(p, t, op, ex, *ex);
        if (pe) return pe;
    }
    // parse arbitrarily many binary operators
    while (true) {
        op = token_to_binary_op(t);
        if (op == OP_NOOP) break;
        if (op_precedence[op] < prec) return PE_OK;
        pe = parse_binary_op(p, t, op, ex, *ex);
        if (pe) return pe;
        PEEK(p, t);
    }
    return PE_OK;
}
parse_error parse_expression(parser* p, astn** ex)
{
    return parse_expression_of_prec(p, ex, PREC_BASELINE);
}
parse_error
parse_eof_delimited_body(parser* p, stmt** tgt, body_parser_mode bpm)
{
    *tgt = NULL; // fist element must be zero for extend to check
    token* t;
    PEEK(p, t);
    while (t->type != TT_EOF) {
        parse_error pe = parse_statement(p, tgt, bpm);
        if (!pe) {
            tgt = &(*tgt)->next;
        }
        else {
            *tgt = NULL;
            return pe;
        }
        PEEK(p, t);
    }
    return PE_OK;
}
parse_error parser_parse_file(parser* p, file* f)
{
    int r = tk_open_file(&p->tk, f);
    if (r) return PE_TK_ERROR;
    parse_error pe = parse_eof_delimited_body(p, p->root_head, BPM_FILE);
    if (!pe) {
        while ((*p->root_head)->next != NULL) {
            p->root_head = &(*p->root_head)->next;
        }
        p->root_head = &(*p->root_head)->next;
    }
    tk_close_file(&p->tk);
    return pe;
}
static inline char* access_modifier_string(access_modifier am)
{
    switch (am) {
        case AM_PRIVATE: return keyword_strings[KW_PRIVATE];
        case AM_PROTECTED: return keyword_strings[KW_PROTECTED];
        case AM_PUBLIC: return keyword_strings[KW_PUBLIC];
        default: return NULL;
    }
}
static inline int
report_redundant_specifier(parser* p, char* spec, ureg start, ureg end)
{
    char* msg = error_log_cat_strings_3(
        &p->tk.tc->error_log, "redundant ", spec, " specifier");
    if (!msg) return ERR;
    parser_error_1a(p, "redundant access modifiers specified", start, end, msg);
    return OK;
}
static inline int stmt_flags_from_kw_set_access_mod(
    parser* p, stmt_flags* f, access_modifier am, ureg start, ureg end)
{
    access_modifier old_am = stmt_flags_get_access_mod(*f);
    if (old_am != AM_UNSPECIFIED) {
        if (old_am == am) {
            report_redundant_specifier(
                p, access_modifier_string(am), start, end);
        }
        else {
            char* msgstrs[5];
            msgstrs[0] = "'";
            msgstrs[1] = access_modifier_string(am);
            msgstrs[2] = "' conflicts with previous '";
            msgstrs[3] = access_modifier_string(old_am);
            msgstrs[4] = "'";
            char* msg = error_log_cat_strings(&p->tk.tc->error_log, 4, msgstrs);
            if (!msg) return ERR;
            error_log_report_annotated(
                &p->tk.tc->error_log, ES_PARSER, false,
                "conflicting access modifiers specified", p->tk.file, start,
                end, msg);
        }
        return ERR;
    }
    return OK;
}
int stmt_flags_from_kw(
    parser* p, stmt_flags* f, keyword_id kw, ureg start, ureg end, bool* action)
{
    // TODO: enforce order
    *action = true;
    switch (kw) {
        case KW_PRIVATE:
            return stmt_flags_from_kw_set_access_mod(
                p, f, AM_PRIVATE, start, end);
        case KW_PROTECTED:
            return stmt_flags_from_kw_set_access_mod(
                p, f, AM_PROTECTED, start, end);
        case KW_PUBLIC:
            return stmt_flags_from_kw_set_access_mod(
                p, f, AM_PUBLIC, start, end);
        case KW_CONST: {
            if (stmt_flags_get_const(*f) != false) {
                report_redundant_specifier(
                    p, keyword_strings[KW_CONST], start, end);
                return ERR;
            }
            stmt_flags_set_const(f, true);
        } break;
        case KW_SEALED: {
            if (stmt_flags_get_sealed(*f) != false) {
                report_redundant_specifier(
                    p, keyword_strings[KW_SEALED], start, end);
                return ERR;
            }
            stmt_flags_set_sealed(f, true);
        } break;
        case KW_VIRTUAL: {
            if (stmt_flags_get_virtual(*f) != false) {
                report_redundant_specifier(
                    p, keyword_strings[KW_VIRTUAL], start, end);
                return ERR;
            }
            stmt_flags_set_virtual(f, true);
        } break;
        case KW_STATIC: {
            if (stmt_flags_get_static(*f) != false) {
                report_redundant_specifier(
                    p, keyword_strings[KW_STATIC], start, end);
                return ERR;
            }
            stmt_flags_set_static(f, true);
        } break;
        default: {
            *action = false;
        } break;
    }
    return OK;
}
parse_error parse_var_decl(
    parser* p, ureg start, ureg col_end, stmt_flags flags, string ident,
    stmt** n)
{
    parse_error pe;
    stmt_var_decl* vd = alloc_perm(p, sizeof(stmt_var_decl));
    if (!vd) return PE_INSANE;
    vd->nstmt.name = alloc_string_perm(p, ident);
    if (!vd->nstmt.name) return PE_INSANE;
    vd->nstmt.stmt.type = ASTNT_VAR_DECL;
    vd->nstmt.stmt.flags = flags;
    vd->nstmt.parent = p->curr_parent;
    token* t;
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
        PEEK(p, t);
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
        else if (t->type == TT_SEMICOLON) {
            vd->value = NULL;
        }
        else {
            ureg end;
            get_expr_bounds(vd->type, NULL, &end);
            parser_error_2a(
                p, "invalid declaration syntax", t->start, t->end,
                "expected '=' or ';'", start, end, "begin of declaration");
            return PE_HANDLED;
        }
    }
    if (t->type != TT_SEMICOLON) {
        parser_error_1a(
            p, "missing semicolon", t->start, t->end,
            "expected ';' to terminate the declaration");
        return PE_HANDLED;
    }
    nstmt_fill_srange(p, &vd->nstmt, start, t->end);
    tk_consume(&p->tk);
    iht_insert(&p->iht, (named_stmt*)vd);
    *n = (stmt*)vd;
    return PE_OK;
}
parse_error parse_param_list(
    parser* p, named_stmt* parent, stmt_param_decl** tgt, bool generic,
    ureg ctx_start, ureg ctx_end, char* msg)
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
        parse_error pe =
            parse_param_decl(p, parent, tgt, ctx_start, ctx_end, msg);
        if (pe) {
            *tgt = NULL;
            return pe;
        }
        tgt = (stmt_param_decl**)&(*tgt)->nstmt.stmt.next;
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
parse_error parse_func_decl(parser* p, ureg start, stmt_flags flags, stmt** n)
{
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
    if (!name) return PE_INSANE;
    tk_void(&p->tk);
    PEEK(p, t);
    named_stmt* fn;
    bool generic;
    if (t->type == TT_BRACKET_OPEN) {
        generic = true;
        fn = alloc_perm(p, sizeof(stmt_generic_function));
        if (!fn) return PE_INSANE;
        tk_void(&p->tk);
        pe = parse_param_list(
            p, fn, &((stmt_generic_function*)fn)->generic_params, true, start,
            decl_end, "in this function declaration");
        if (pe) return pe;
        PEEK(p, t);
    }
    else {
        generic = false;
        fn = alloc_perm(p, sizeof(stmt_function));
        if (!fn) return PE_INSANE;
    }
    fn->name = name;
    pe = nstmt_fill_srange(p, fn, start, decl_end);
    if (pe) return pe;
    if (t->type != TT_PAREN_OPEN) {
        parser_error_2a(
            p, "invalid function declaration syntax", t->start, t->end,
            "expected '(' to start parameter list", start, decl_end,
            "in this function declaration");
        return PE_HANDLED;
    }
    tk_void(&p->tk);
    stmt_param_decl** pd = generic ? &((stmt_generic_function*)fn)->params
                                   : &((stmt_function*)fn)->params;
    pe = parse_param_list(
        p, fn, pd, false, start, decl_end, "in this function declaration");
    if (pe) return pe;
    fn->parent = p->curr_parent;
    fn->stmt.type = generic ? ASTNT_GENERIC_FUNCTION : ASTNT_FUNCTION;
    fn->stmt.flags = flags;
    *n = (stmt*)fn;
    if (generic) {
        stmt_generic_function* gf = (stmt_generic_function*)fn;
        return parse_body(
            p, fn, &gf->body, BPM_GENERIC_FUNCTION, &gf->body_end);
    }
    else {
        stmt_function* f = (stmt_function*)fn;
        return parse_body(p, fn, &f->body, BPM_FUNCTION, &f->body_end);
    }
}
parse_error parse_struct_decl(parser* p, ureg start, stmt_flags flags, stmt** n)
{
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
    if (!name) return PE_INSANE;
    tk_void(&p->tk);
    PEEK(p, t);
    named_stmt* st;
    bool generic;
    if (t->type == TT_BRACKET_OPEN) {
        generic = true;
        st = alloc_perm(p, sizeof(stmt_generic_struct));
        if (!st) return PE_INSANE;
        tk_void(&p->tk);
        pe = parse_param_list(
            p, st, &((stmt_generic_struct*)st)->generic_params, true, start,
            decl_end, "in this struct declaration");
        if (pe) return pe;
        PEEK(p, t);
    }
    else {
        generic = false;
        st = alloc_perm(p, sizeof(stmt_struct));
        if (!st) return PE_INSANE;
    }
    st->name = name;
    pe = nstmt_fill_srange(p, st, start, decl_end);
    if (pe) return pe;
    st->parent = p->curr_parent;
    st->stmt.type = generic ? ASTNT_GENERIC_STRUCT : ASTNT_STRUCT;
    st->stmt.flags = flags;
    *n = (stmt*)st;
    if (generic) {
        stmt_generic_struct* gs = (stmt_generic_struct*)st;
        return parse_body(p, st, &gs->body, BPM_GENERIC_STRUCT, &gs->body_end);
    }
    else {
        stmt_struct* s = (stmt_struct*)st;
        return parse_body(p, st, &s->body, BPM_STRUCT, &s->body_end);
    }
}
parse_error parse_module_decl(parser* p, ureg start, stmt_flags flags, stmt** n)
{
    token* t;
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
    char* name = alloc_string_perm(p, t->str);
    if (!name) return PE_INSANE;
    tk_void(&p->tk);
    PEEK(p, t);
    named_stmt* md;
    bool generic;
    if (t->type == TT_BRACKET_OPEN) {
        generic = true;
        md = alloc_perm(p, sizeof(stmt_generic_module));
        if (!md) return PE_INSANE;
        tk_void(&p->tk);
        pe = parse_param_list(
            p, md, &((stmt_generic_module*)md)->generic_params, true, start,
            decl_end, "in this module declaration");
        if (pe) return pe;
        PEEK(p, t);
    }
    else {
        generic = false;
        md = alloc_perm(p, sizeof(stmt_module));
        if (!md) return PE_INSANE;
    }
    md->name = name;
    pe = nstmt_fill_srange(p, md, start, decl_end);
    if (pe) return pe;
    md->parent = p->curr_parent;
    md->stmt.type = generic ? ASTNT_GENERIC_MODULE : ASTNT_MODULE;
    md->stmt.flags = flags;
    *n = (stmt*)md;
    if (generic) {
        stmt_generic_module* gm = (stmt_generic_module*)md;
        return parse_body(p, md, &gm->body, BPM_GENERIC_MODULE, &gm->body_end);
    }
    else {
        stmt_module* m = (stmt_module*)md;
        return parse_body(p, md, &m->body, BPM_MODULE, &m->body_end);
    }
}
parse_error parse_extend_decl(
    parser* p, ureg start, stmt_flags flags, stmt** n, body_parser_mode bpm)
{
    token* t;
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
    char* name = alloc_string_perm(p, t->str);
    if (!name) return PE_INSANE;
    tk_void(&p->tk);
    PEEK(p, t);
    named_stmt* ex;
    bool generic;
    if (t->type == TT_BRACKET_OPEN) {
        generic = true;
        ex = alloc_perm(p, sizeof(stmt_generic_extend));
        if (!ex) return PE_INSANE;
        tk_void(&p->tk);
        pe = parse_param_list(
            p, ex, &((stmt_generic_extend*)ex)->generic_params, true, start,
            decl_end, "in this extend declaration");
        if (pe) return pe;
        PEEK(p, t);
    }
    else {
        generic = false;
        ex = alloc_perm(p, sizeof(stmt_extend));
        if (!ex) return PE_INSANE;
    }
    ex->name = name;
    pe = nstmt_fill_srange(p, ex, start, decl_end);
    if (pe) return pe;
    ex->parent = p->curr_parent;
    ex->stmt.type = generic ? ASTNT_GENERIC_EXTEND : ASTNT_EXTEND;
    ex->stmt.flags = flags;
    PEEK(p, t);
    if (t->type == TT_SEMICOLON) {
        named_stmt* parent = p->curr_parent;
        stmt* body = get_parent_body(parent);
        if (body == NULL) {
            tk_consume(&p->tk);
            p->curr_parent = ex;
            if (generic) {
                pe = parse_eof_delimited_body(
                    p, &((stmt_generic_extend*)ex)->body, BPM_GENERIC_EXTEND);
            }
            else {
                pe = parse_eof_delimited_body(
                    p, &((stmt_extend*)ex)->body, BPM_EXTEND);
            }
            p->curr_parent = parent;
            *n = (stmt*)ex;
            return pe;
        }
        else {
            *n = NULL;
            while (body->next != NULL) body = body->next;
            ureg hs, he;
            stmt_get_highlight_bounds(body, &hs, &he);
            parser_error_2a_bpm(
                p, "non leading extend statement", start, t->end, "", hs, he,
                "preceeded by this statement", bpm);
            return PE_HANDLED;
        }
    }
    *n = (stmt*)ex;
    if (generic) {
        stmt_generic_extend* ge = (stmt_generic_extend*)ex;
        return parse_body(p, ex, &ge->body, BPM_GENERIC_EXTEND, &ge->body_end);
    }
    else {
        stmt_extend* e = (stmt_extend*)ex;
        return parse_body(p, ex, &e->body, BPM_EXTEND, &e->body_end);
    }
}
parse_error parse_trait_decl(parser* p, ureg start, stmt_flags flags, stmt** n)
{
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
    if (!name) return PE_INSANE;
    tk_void(&p->tk);
    PEEK(p, t);
    named_stmt* un;
    bool generic;
    if (t->type == TT_BRACKET_OPEN) {
        generic = true;
        un = alloc_perm(p, sizeof(stmt_generic_trait));
        if (!un) return PE_INSANE;
        tk_void(&p->tk);
        pe = parse_param_list(
            p, un, &((stmt_generic_trait*)un)->generic_params, true, start,
            decl_end, "in this trait declaration");
        if (pe) return pe;
        PEEK(p, t);
    }
    else {
        generic = false;
        un = alloc_perm(p, sizeof(stmt_trait));
        if (!un) return PE_INSANE;
    }
    un->name = name;
    pe = nstmt_fill_srange(p, un, start, decl_end);
    if (pe) return pe;
    un->parent = p->curr_parent;
    un->stmt.type = generic ? ASTNT_GENERIC_TRAIT : ASTNT_TRAIT;
    un->stmt.flags = flags;
    *n = (stmt*)un;
    if (generic) {
        stmt_generic_extend* gt = (stmt_generic_extend*)un;
        return parse_body(p, un, &gt->body, BPM_GENERIC_TRAIT, &gt->body_end);
    }
    else {
        stmt_extend* t = (stmt_extend*)un;
        return parse_body(p, un, &t->body, BPM_TRAIT, &t->body_end);
    }
}
bool bpm_supports_exprs(body_parser_mode bpm)
{
    switch (bpm) {
        case BPM_FUNCTION:
        case BPM_GENERIC_FUNCTION:
        case BPM_LOOP:
        case BPM_LAMBDA: {
            return true;
        }
        default: {
            return false;
        }
    }
}
parse_error parse_expr_stmt(parser* p, stmt** tgt, body_parser_mode bpm)
{
    astn* expr;
    token* t;
    PEEK(p, t);
    ureg start = t->start;
    parse_error pe = parse_expression(p, &expr);
    PEEK(p, t);
    if (pe == PE_EOEX) {
        parser_error_1a_bpm(
            p, "unexpected token in statement", t->start, t->end, "", bpm);
        return PE_HANDLED;
    }
    if (pe) return pe;
    if (t->type != TT_SEMICOLON) {
        src_range_large sr;
        src_range_unpack(p->curr_parent->decl_range, &sr);
        ureg end;
        get_expr_bounds(expr, NULL, &end); // TODO improve for loops etc.
        parser_error_2a_bpm(
            p, "missing semicolon", start, end, "in this expression", t->start,
            t->end, "expected ';' to terminate the declaration", bpm);
        return PE_HANDLED;
    }
    tk_void(&p->tk);
    if (pe) return pe;
    stmt_expr* e = alloc_perm(p, sizeof(stmt_expr));
    if (!e) return PE_INSANE;
    e->stmt.type = ASTNT_EXPRESSION;
    e->expr = expr;
    e->stmt_range = src_range_pack_lines(p->tk.tc, start, t->end);
    if (e->stmt_range == SRC_RANGE_INVALID) return PE_INSANE;
    *tgt = (stmt*)e;
    return PE_OK;
}
parse_error parse_statement(parser* p, stmt** tgt, body_parser_mode bpm)
{
    parse_error pe;
    stmt_flags flags = ASTN_FLAGS_DEFAULT;
    token* t;
    PEEK(p, t);
    ureg start = t->start;
    while (true) {
        if (t->type != TT_STRING) {
            if (flags == ASTN_FLAGS_DEFAULT && bpm_supports_exprs(bpm)) {
                return parse_expr_stmt(p, tgt, bpm);
            }
            else {
                parser_error_1a_bpm(
                    p, "unexpected token in statement", t->start, t->end,
                    stmt_flags_get_const(flags)
                        ? "expected keyword"
                        : "expected identifier or keyword",
                    bpm);
                return PE_HANDLED;
            }
        }
        keyword_id kw = kw_match(t->str);
        if (kw != KW_INVALID_KEYWORD) {
            bool is_flags_kw;
            int r =
                stmt_flags_from_kw(p, &flags, kw, start, t->end, &is_flags_kw);
            if (r) return PE_UNEXPECTED_TOKEN;
            if (is_flags_kw) {
                tk_void(&p->tk);
                PEEK(p, t);
                continue;
            }
        }
        switch (kw) {
            case KW_FUNC: {
                tk_void(&p->tk);
                pe = parse_func_decl(p, start, flags, tgt);
                return pe;
            } break;
            case KW_STRUCT: {
                tk_void(&p->tk);
                pe = parse_struct_decl(p, start, flags, tgt);
                return pe;
            } break;
            case KW_TRAIT: {
                tk_void(&p->tk);
                pe = parse_trait_decl(p, start, flags, tgt);
                return pe;
            } break;
            case KW_MODULE: {
                tk_void(&p->tk);
                pe = parse_module_decl(p, start, flags, tgt);
                return pe;
            } break;
            case KW_EXTEND: {
                tk_void(&p->tk);
                pe = parse_extend_decl(p, start, flags, tgt, bpm);
                return pe;
            } break;
            // case KW_IMPORT:
            // case KW_INCLUDE:
            default: {
                token* t2 = tk_peek_2nd(&p->tk);
                if (!t2) return PE_TK_ERROR;
                if (t2->type == TT_COLON) {
                    tk_void_n(&p->tk, 2);
                    return parse_var_decl(
                        p, start, t2->end, flags, t->str, tgt);
                }
                else if (
                    flags == ASTN_FLAGS_DEFAULT && bpm_supports_exprs(bpm)) {
                    return parse_expr_stmt(p, tgt, bpm);
                }
                else {
                    tk_void(&p->tk);
                    PEEK(p, t);
                    parser_error_1a_bpm(
                        p, "unexpected token in statement", t->start, t->end,
                        "expected ':' to initiate a declaration", bpm);
                    return PE_UNEXPECTED_TOKEN;
                }

            } break;
        }
        PEEK(p, t);
        start = t->start;
    }
}
parse_error parse_braced_delimited_body(
    parser* p, token* t, stmt** tgt, body_parser_mode bpm, ureg* end)
{
    *tgt = NULL; // fist element must be zero for extend to check
    ureg start = t->start;
    ureg bend = t->end;
    tk_void(&p->tk);
    parse_error pe;
    PEEK(p, t);
    while (t->type != TT_BRACE_CLOSE) {
        if (t->type != TT_EOF) {
            pe = parse_statement(p, tgt, bpm);
            if (!pe) {
                tgt = &(*tgt)->next;
            }
            else {
                *tgt = NULL;
                return pe;
            }
            PEEK(p, t);
        }
        else {
            parser_error_2a_bpm(
                p, "unterminated scope", t->start, t->end,
                "reached EOF before scope was closed", start, bend,
                "scope starts here", bpm);
            return PE_UNEXPECTED_TOKEN;
        }
    }
    tk_consume(&p->tk);
    *end = t->end;
    *tgt = NULL;
    return PE_OK;
}

parse_error parse_body(
    parser* p, named_stmt* parent, stmt** tgt, body_parser_mode bpm, ureg* end)
{
    *tgt = NULL; // fist element must be zero for extend to check
    named_stmt* old_parent = p->curr_parent;
    p->curr_parent = parent;
    parse_error pe;
    token* t;
    PEEK(p, t);
    if (t->type != TT_BRACE_OPEN) {
        pe = parse_statement(p, tgt, bpm);
        if (pe) {
            *tgt = NULL;
        }
        else {
            (*tgt)->next = NULL;
            *end = src_range_get_end(((stmt_expr*)*tgt)->stmt_range);
        }
    }
    else {
        pe = parse_braced_delimited_body(p, t, tgt, bpm, end);
    }
    p->curr_parent = old_parent;
    return pe;
}
