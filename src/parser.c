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
    BPM_STRUCT,
    BPM_FUNCTION,
    BPM_LAMBDA,
} body_parser_mode;

parse_error parse_statement(parser* p, ast_node** head, body_parser_mode bpm);
parse_error parse_body(
    parser* p, named_ast_node* parent, ast_node** head, body_parser_mode bpm);
parse_error parse_expression(parser* p, expr_node** en);
parse_error parse_expression_p(parser* p, ureg prec, expr_node** en);

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
    [OP_VALUE_OF] = 14,
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

static inline bool is_left_associative(expr_node_type t)
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

static inline expr_node_type token_to_binary_op(token* t)
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
static inline expr_node_type token_to_prefix_unary_op(token* t)
{
    switch (t->type) {
    case TT_MINUS: return OP_UNARY_MINUS;
    case TT_PLUS: return OP_UNARY_PLUS;
    case TT_TILDE: return OP_BITWISE_NOT;
    case TT_EXCLAMATION_MARK: return OP_NOT;
    case TT_STAR: return OP_DEREF;
    case TT_AND: return OP_REF_OF;
    case TT_PERCENT: return OP_POINTER_OF;
    case TT_CARET: return OP_VALUE_OF;
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
static inline expr_node_type token_to_postfix_unary_op(token* t)
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

static inline int
parser_error_1a(parser* p, char* msg, ureg start, ureg end, char* annot)
{
    error_log_report_annotated(
        &p->tk.tc->error_log, ES_PARSER, false, msg, p->tk.file, start, end,
        annot);
    return ERR;
}
static inline int parser_error_2a(
    parser* p, char* msg, ureg start, ureg end, char* annot, ureg start2,
    ureg end2, char* annot2)
{
    error_log_report_annotated_twice(
        &p->tk.tc->error_log, ES_PARSER, false, msg, p->tk.file, start, end,
        annot, start2, end2, annot2);
    return ERR;
}
static inline int parser_error_1at(parser* p, char* msg, token* t, char* annot)
{
    return parser_error_1a(p, msg, t->start, t->end, annot);
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
    p->root.nastn.name = NULL;
    p->root.nastn.parent = NULL;
    p->root.nastn.astn.type = ASTNT_MODULE;
    p->root.imports = NULL;
    p->root.body = NULL;
    p->root.nastn.astn.next = NULL;
    p->curr_parent = (named_ast_node*)&p->root;
    p->curr_head = &p->root.body;
    return OK;
}
void parser_fin(parser* p)
{
    tk_fin(&p->tk);
}

parse_error parser_search_extend(parser* p)
{
    token* t;
    PEEK(p, t);
    if (t->type == TT_STRING && kw_equals(KW_EXTEND, t->str)) {
        ureg estart = t->start;
        tk_void(&p->tk);
        t = tk_consume(&p->tk);
        if (!t) return PE_TK_ERROR;
        if (t->type != TT_STRING)
            return parser_error_1at(
                p, "invalid extend statement syntax", t,
                "expected module name");
        t = tk_consume(&p->tk);
        if (t->type != TT_SEMICOLON) {
            parser_error_unexpected_token(
                p, t, TT_SEMICOLON, "invalid extend statement syntax",
                "statement start", estart,
                estart + strlen(keyword_strings[KW_EXTEND]));
            return ERR;
        }
        astn_extend* e = (astn_extend*)alloc_stage(p, sizeof(astn_extend));
        if (!e) return ERR;
        e->nastn.astn.type = ASTNT_EXTEND;
        e->nastn.astn.next = NULL;
        e->nastn.parent = (named_ast_node*)&p->root;
        e->nastn.name = (char*)alloc_string_stage(p, t->str);
        if (e->nastn.name) return ERR;
        e->body = NULL;
        e->imports = NULL;
        return OK;
    }
    return OK;
}
static inline parse_error
nastn_fill_srange(parser* p, named_ast_node* nastn, ureg start, ureg end)
{
    nastn->decl_range = src_range_pack_lines(p->tk.tc, start, end);
    if (nastn->decl_range == SRC_RANGE_INVALID) return PE_INSANE;
    return PE_OK;
}
static inline parse_error
en_fill_srange(parser* p, expr_node* en, ureg start, ureg end)
{
    en->srange = src_range_pack_lines(p->tk.tc, start, end);
    if (en->srange == SRC_RANGE_INVALID) return PE_INSANE;
    return PE_OK;
}
static inline expr_node* parse_str_value(parser* p, token* t)
{
    expr_node_type ent;
    switch (t->type) {
    case TT_BINARY_LITERAL: ent = ENT_BINARY_LITERAL; break;
    case TT_LITERAL: ent = ENT_STRING_LITERAL; break;
    case TT_NUMBER: ent = ENT_NUMBER; break;
    case TT_STRING: ent = ENT_IDENTIFIER; break;
    default: return NULL;
    }
    en_str_value* sv = (en_str_value*)alloc_perm(p, sizeof(en_str_value));
    if (!sv) return NULL;
    sv->en.type = ent;
    sv->value = alloc_string_stage(p, t->str);
    if (!sv->value) return NULL;
    if (en_fill_srange(p, &sv->en, t->start, t->end)) return NULL;
    return (expr_node*)sv;
}
parse_error parse_param_decl(
    parser* p, named_ast_node* parent, astn_param_decl** tgt, ureg ctx_start,
    ureg ctx_end, char* msg_context)
{
    parse_error pe;
    token* t;
    PEEK(p, t);
    if (t->type != TT_STRING) {
        error_log_report_annotated_twice(
            &p->tk.tc->error_log, ES_PARSER, false, "invalid parameter syntax",
            p->tk.file, t->start, t->end, "expected parameter identifier",
            ctx_start, ctx_end, msg_context);
        return PE_UNEXPECTED_TOKEN;
    }
    astn_param_decl* d = alloc_perm(p, sizeof(astn_param_decl));
    if (!d) return PE_INSANE;
    d->nastn.name = alloc_string_perm(p, t->str);
    if (!d->nastn.name) return PE_INSANE;
    d->nastn.astn.type = ASTNT_PARAM_DECL;
    d->nastn.parent = parent;
    // TODO: flags parsing
    d->nastn.astn.flags = ASTN_FLAGS_DEFAULT;
    tk_void(&p->tk);
    PEEK(p, t);
    if (t->type != TT_COLON) {
        error_log_report_annotated_twice(
            &p->tk.tc->error_log, ES_PARSER, false, "invalid parameter syntax",
            p->tk.file, t->start, t->end, "expected ':' after identifier",
            ctx_start, ctx_end, msg_context);
        return PE_UNEXPECTED_TOKEN;
    }
    tk_void(&p->tk);
    PEEK(p, t);
    if (t->type == TT_EQUALS) {
        tk_void(&p->tk);
        d->type = NULL;
        pe = parse_expression(p, &d->default_value);
        if (pe) return pe; // TODO: EMSG
    }
    else {
        pe = parse_expression(p, &d->type);
        if (pe) return pe; // TODO: EMSG
        PEEK(p, t);
        if (t->type == TT_EQUALS) {
            tk_void(&p->tk);
            pe = parse_expression(p, &d->default_value);
            if (pe) return pe; // TODO: EMSG
        }
        else {
            d->default_value = NULL;
        }
    }
    iht_insert(&p->iht, (named_ast_node*)d);
    *tgt = d;
    return PE_OK;
}
parse_error parse_expression_node_list(
    parser* p, expr_node_list* tgt, token_type expected_trailer)
{
    token* t;
    PEEK(p, t);
    if (t->type == expected_trailer) {
        tgt->end_ptr = NULL;
        return PE_OK;
    }
    void** list_start = list_builder_start(&p->lb);
    while (true) {
        expr_node* en;
        parse_error pe = parse_expression(p, &en);
        if (pe != PE_OK) return PE_UNEXPECTED_TOKEN;
        int r = list_builder_add(&p->lb, (void*)en);
        if (r) return PE_INSANE;
        // the way this is programmed right now
        // we allow trailing commas... so be it \(*.*)/
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
        sizeof(expr_node**), 0);
    if (!start) return PE_INSANE;
    *tgt->end_ptr = ptradd(start, count * sizeof(void*));
    return PE_OK;
}
static inline parse_error parse_tuple(parser* p, token* t, expr_node** en)
{
    ureg t_start = t->start;
    ureg t_end = t->end;
    tk_void(&p->tk);
    en_tuple* tp = alloc_perm(p, sizeof(en_tuple));
    if (!tp) return PE_INSANE;
    tp->en.type = ENT_TUPLE;
    parse_error pe =
        parse_expression_node_list(p, &tp->elements, TT_BRACKET_CLOSE);
    // EMSG: suboptimal e.g. for case [,,]
    if (pe == PE_UNEXPECTED_TOKEN) {
        PEEK(p, t);
        error_log_report_annotated_twice(
            &p->tk.tc->error_log, ES_PARSER, false, "unclosed tuple",
            p->tk.file, t->start, t->end,
            "reached end of expression due to unexpected token", t_start, t_end,
            "didn't find a matching bracket for this tuple");
    }
    if (pe != PE_OK) return pe;
    PEEK(p, t);
    if (en_fill_srange(p, &tp->en, t_start, t->end)) return PE_INSANE;
    tk_void(&p->tk);
    *en = (expr_node*)tp;
    return PE_OK;
}
static inline parse_error parse_array(parser* p, token* t, expr_node** en)
{
    ureg t_start = t->start;
    ureg t_end = t->end;
    tk_void(&p->tk);
    en_array* arr = alloc_perm(p, sizeof(en_array));
    if (!arr) return PE_INSANE;
    arr->en.type = ENT_ARRAY;
    parse_error pe =
        parse_expression_node_list(p, &arr->elements, TT_BRACE_CLOSE);
    // EMSG: suboptimal e.g. for case {,,}
    if (pe == PE_UNEXPECTED_TOKEN) {
        PEEK(p, t);
        error_log_report_annotated_twice(
            &p->tk.tc->error_log, ES_PARSER, false, "unclosed array",
            p->tk.file, t->start, t->end,
            "reached end of expression due to unexpected token", t_start, t_end,
            "didn't find a matching brace for this array");
    }
    if (pe != PE_OK) return pe;
    PEEK(p, t);
    if (en_fill_srange(p, &arr->en, t_start, t->end)) return PE_INSANE;
    tk_void(&p->tk);
    *en = (expr_node*)arr;
    return PE_OK;
}
static inline parse_error
parse_parenthesis_group(parser* p, token* t, expr_node** en)
{
    ureg t_start = t->start;
    ureg t_end = t->end;
    tk_void(&p->tk);
    parse_error pe = parse_expression_p(p, PREC_BASELINE, en);
    if (pe != PE_OK && pe != PE_EOEX) return pe;
    PEEK(p, t);
    if (t->type != TT_PAREN_CLOSE) {
        error_log_report_annotated_twice(
            &p->tk.tc->error_log, ES_PARSER, false, "parenthesis missmatch",
            p->tk.file, t->start, t->end, "reached end of expression", t_start,
            t_end, "didn't find a match for this parenthesis");
        return PE_UNEXPECTED_TOKEN;
    }
    if (pe == PE_EOEX) {
        error_log_report_annotated_twice(
            &p->tk.tc->error_log, ES_PARSER, false, "empty parenthesis pair",
            p->tk.file, t->start, t->end, "found closing parenthesis", t_start,
            t_end, "expected an evaluable expression");
        return PE_HANDLED;
    }
    en_parentheses* pr = (en_parentheses*)alloc_perm(p, sizeof(en_parentheses));
    if (!pr) return PE_INSANE;
    pr->en.type = OP_PARENTHESES;
    pr->child = *en;
    // TODO fixme
    if (en_fill_srange(p, &pr->en, t_start, t->end)) return PE_INSANE;
    tk_void(&p->tk);
    *en = (expr_node*)pr;
    return PE_OK;
}
static inline parse_error
parse_prefix_unary_op(parser* p, token* t, expr_node_type op, expr_node** en)
{
    en_op_unary* ou = (en_op_unary*)alloc_perm(p, sizeof(en_op_unary));
    if (!ou) return PE_INSANE;
    if (en_fill_srange(p, &ou->en, t->start, t->end)) return PE_INSANE;
    tk_void(&p->tk);
    ou->en.type = ENT_OP_UNARY;
    ou->en.op_type = op;
    parse_error pe = parse_expression_p(
        p, op_precedence[op] + is_left_associative(op), &ou->child);
    if (pe) {
        PEEK(p, t);
        if (pe == PE_EOEX) {
            src_range s;
            src_range_unpack(ou->en.srange, &s);
            error_log_report_annotated_twice(
                &p->tk.tc->error_log, ES_PARSER, false,
                "missing operand for unary operator", p->tk.file, t->start,
                t->end, "reached end of expression due to unexpected token",
                s.start, s.end, "missing operand for this operator");
        }
        return PE_UNEXPECTED_TOKEN;
    }
    *en = (expr_node*)ou;
    return PE_OK;
}
static inline parse_error
parse_single_value(parser* p, token* t, expr_node** en)
{
    parse_error pe;
    switch (t->type) {
    case TT_PAREN_OPEN: {
        pe = parse_parenthesis_group(p, t, en);
        if (pe) return pe;
    } break;
    case TT_BRACKET_OPEN: {
        pe = parse_tuple(p, t, en);
        if (pe) return pe;
    } break;
    case TT_BRACE_OPEN: {
        pe = parse_array(p, t, en);
        if (pe) return pe;
    } break;
    case TT_STRING:
    case TT_NUMBER:
    case TT_LITERAL:
    case TT_BINARY_LITERAL: {
        *en = parse_str_value(p, t);
        if (!*en) return PE_INSANE;
        tk_void(&p->tk);
        break;
    }
    default: {
        return PE_EOEX;
    } break;
    }
    return PE_OK;
}
static inline parse_error
parse_call(parser* p, token* t, expr_node** en, expr_node* lhs)
{
    ureg t_start = t->start;
    tk_void(&p->tk);
    en_call* call = alloc_perm(p, sizeof(en_call));
    if (!call) return PE_INSANE;
    parse_error pe = parse_expression_node_list(p, &call->args, TT_PAREN_CLOSE);
    // EMSG: suboptimal e.g. for case {,,}
    if (pe == PE_UNEXPECTED_TOKEN) {
        PEEK(p, t);
        error_log_report_annotated_twice(
            &p->tk.tc->error_log, ES_PARSER, false, "unclosed function call",
            p->tk.file, t->start, t->end,
            "reached end of expression due to unexpected token", t_start,
            t_start + 1, "didn't find a matching parenthesis for this");
    }
    if (pe != PE_OK) return pe;
    PEEK(p, t);
    if (en_fill_srange(p, &call->en, t_start, t->end)) return PE_INSANE;
    tk_void(&p->tk);
    call->en.type = ENT_OP_CALL;
    call->en.op_type = OP_CALL;
    call->lhs = lhs;
    *en = (expr_node*)call;
    return PE_OK;
}
static inline parse_error
parse_access(parser* p, token* t, expr_node** en, expr_node* lhs)
{
    ureg t_start = t->start;
    tk_void(&p->tk);
    en_access* acc = alloc_perm(p, sizeof(en_access));
    if (!acc) return PE_INSANE;
    parse_error pe =
        parse_expression_node_list(p, &acc->args, TT_BRACKET_CLOSE);
    // EMSG: suboptimal e.g. for case {,,}
    if (pe == PE_UNEXPECTED_TOKEN) {
        PEEK(p, t);
        error_log_report_annotated_twice(
            &p->tk.tc->error_log, ES_PARSER, false, "unclosed access operator",
            p->tk.file, t->start, t->end,
            "reached end of expression due to unexpected token", t_start,
            t_start + 1, "didn't find a matching bracket for this");
    }
    if (pe != PE_OK) return pe;
    PEEK(p, t);
    if (en_fill_srange(p, &acc->en, t_start, t->end)) return PE_INSANE;
    tk_void(&p->tk);
    acc->en.type = ENT_OP_ACCESS;
    acc->en.op_type = OP_ACCESS;
    acc->lhs = lhs;
    *en = (expr_node*)acc;
    return PE_OK;
}
static inline parse_error parse_postfix_unary_op(
    parser* p, token* t, expr_node_type op, expr_node** en, expr_node* lhs)
{
    if (op == OP_CALL) {
        return parse_call(p, t, en, lhs);
    }
    else if (op == OP_ACCESS) {
        return parse_access(p, t, en, lhs);
    }
    else {
        tk_void(&p->tk);
        en_op_unary* ou = (en_op_unary*)alloc_perm(p, sizeof(en_op_unary));
        if (!ou) return PE_INSANE;
        ou->en.type = ENT_OP_UNARY;
        ou->en.op_type = op;
        ou->child = lhs;
        if (en_fill_srange(p, &ou->en, t->start, t->end)) return PE_INSANE;
        *en = (expr_node*)ou;
        return PE_OK;
    }
}
static inline parse_error parse_binary_op(
    parser* p, token* t, expr_node_type op, expr_node** en, expr_node* lhs)
{
    tk_void(&p->tk);
    en_op_binary* ob = (en_op_binary*)alloc_perm(p, sizeof(en_op_binary));
    if (!ob) return PE_INSANE;
    if (en_fill_srange(p, &ob->en, t->start, t->end)) return PE_INSANE;
    ob->en.type = ENT_OP_BINARY;
    ob->en.op_type = op;
    parse_error pe = parse_expression_p(
        p, op_precedence[op] + is_left_associative(op), &ob->rhs);
    if (pe) {
        if (pe == PE_EOEX) {
            PEEK(p, t);
            src_range sr;
            src_range_unpack(ob->en.srange, &sr);
            error_log_report_annotated_twice(
                &p->tk.tc->error_log, ES_PARSER, false,
                "missing operand for infix operator", p->tk.file, t->start,
                t->end, "reached end of expression", sr.start, sr.end,
                "missing operand for this operator");
            return PE_UNEXPECTED_TOKEN;
        }
        return pe;
    }
    ob->lhs = *en;
    *en = (expr_node*)ob;
    return PE_OK;
}
parse_error parse_expression_p(parser* p, ureg prec, expr_node** en)
{
    token* t;
    PEEK(p, t);
    *en = NULL;
    parse_error pe;
    // parse one prefix op(recursive) or a plain value
    expr_node_type op = token_to_prefix_unary_op(t);
    if (op != OP_NOOP) {
        pe = parse_prefix_unary_op(p, t, op, en);
        if (pe) return pe;
    }
    else {
        pe = parse_single_value(p, t, en);
        if (pe) return pe;
    }
    // parse arbitrarily many postfix operators
    while (true) {
        PEEK(p, t);
        expr_node_type op = token_to_postfix_unary_op(t);
        if (op == OP_NOOP) break;
        if (op_precedence[op] < prec) return PE_OK;
        pe = parse_postfix_unary_op(p, t, op, en, *en);
        if (pe) return pe;
    }
    // parse arbitrarily many binary operators
    while (true) {
        expr_node_type op = token_to_binary_op(t);
        if (op == OP_NOOP) break;
        if (op_precedence[op] < prec) return PE_OK;
        parse_binary_op(p, t, op, en, *en);
        PEEK(p, t);
    }
    return PE_OK;
}
parse_error parse_expression(parser* p, expr_node** en)
{
    return parse_expression_p(p, PREC_BASELINE, en);
}

parse_error parser_parse_file(parser* p, file* f)
{
    int r = tk_open_file(&p->tk, f);
    if (r) return PE_TK_ERROR;
    r = parser_search_extend(p);
    token* t;
    parse_error pe;
    PEEK(p, t);
    while (t->type != TT_EOF) {
        pe = parse_statement(p, p->curr_head, BPM_FILE);
        if (pe) break;
        p->curr_head = &(*p->curr_head)->next;
        PEEK(p, t);
    }
    *p->curr_head = NULL;
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
    error_log_report_annotated(
        &p->tk.tc->error_log, ES_PARSER, false,
        "redundant access modifiers specified", p->tk.file, start, end, msg);
    return OK;
}
static inline int astn_flags_from_kw_set_access_mod(
    parser* p, astn_flags* f, access_modifier am, ureg start, ureg end)
{
    access_modifier old_am = astn_flags_get_access_mod(*f);
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
int astn_flags_from_kw(
    parser* p, astn_flags* f, keyword_id kw, ureg start, ureg end, bool* action)
{
    // TODO: enforce order
    *action = true;
    switch (kw) {
    case KW_PRIVATE:
        return astn_flags_from_kw_set_access_mod(p, f, AM_PRIVATE, start, end);
    case KW_PROTECTED:
        return astn_flags_from_kw_set_access_mod(
            p, f, AM_PROTECTED, start, end);
    case KW_PUBLIC:
        return astn_flags_from_kw_set_access_mod(p, f, AM_PUBLIC, start, end);
    case KW_CONST: {
        if (astn_flags_get_const(*f) != false) {
            report_redundant_specifier(
                p, keyword_strings[KW_CONST], start, end);
            return ERR;
        }
        astn_flags_set_const(f, true);
    } break;
    case KW_SEALED: {
        if (astn_flags_get_sealed(*f) != false) {
            report_redundant_specifier(
                p, keyword_strings[KW_SEALED], start, end);
            return ERR;
        }
        astn_flags_set_sealed(f, true);
    } break;
    case KW_VIRTUAL: {
        if (astn_flags_get_virtual(*f) != false) {
            report_redundant_specifier(
                p, keyword_strings[KW_VIRTUAL], start, end);
            return ERR;
        }
        astn_flags_set_virtual(f, true);
    } break;
    case KW_STATIC: {
        if (astn_flags_get_static(*f) != false) {
            report_redundant_specifier(
                p, keyword_strings[KW_STATIC], start, end);
            return ERR;
        }
        astn_flags_set_static(f, true);
    } break;
    default: {
        *action = false;
    } break;
    }
    return OK;
}
static inline ureg get_expr_end(parser* p, expr_node* n)
{
    switch (n->type) {
    default: {
        src_range r;
        src_range_unpack(n->srange, &r);
        return r.end;
    }
    case ENT_OP_BINARY: return get_expr_end(p, ((en_op_binary*)n)->rhs);
    case ENT_OP_UNARY: {
        en_op_unary* u = (en_op_unary*)n;
        if (is_unary_op_postfix(n->op_type)) {
            src_range r;
            src_range_unpack(n->srange, &r);
            return r.end;
        }
        else {
            return get_expr_end(p, u->child);
        }
    }
    }
}
parse_error parse_var_decl(
    parser* p, ureg start, astn_flags flags, string ident, ast_node** n)
{
    parse_error pe;
    astn_var_decl* vd = alloc_perm(p, sizeof(astn_var_decl));
    if (!vd) return PE_INSANE;
    vd->nastn.name = alloc_string_perm(p, ident);
    if (!vd->nastn.name) return PE_INSANE;
    vd->nastn.astn.type = ASTNT_VAR_DECL;
    vd->nastn.astn.flags = flags;
    vd->nastn.parent = p->curr_parent;
    token* t;
    PEEK(p, t);
    if (t->type == TT_EQUALS) {
        vd->type = NULL;
        pe = parse_expression(p, &vd->value);
        if (pe) return pe;
        PEEK(p, t);
    }
    else {
        pe = parse_expression(p, &vd->type);
        if (pe) return pe;
        PEEK(p, t);
        if (t->type == TT_EQUALS) {
            pe = parse_expression(p, &vd->value);
            if (pe) return pe;
            PEEK(p, t);
        }
        else if (t->type == TT_SEMICOLON) {
            vd->value = NULL;
        }
        else {
            error_log_report_annotated_twice(
                &p->tk.tc->error_log, ES_PARSER, false,
                "invalid declaration syntax", p->tk.file, t->start, t->end,
                "expected '=' or ';'", start, get_expr_end(p, vd->type),
                "begin of declaration");
            return PE_UNEXPECTED_TOKEN;
        }
    }
    if (t->type != TT_SEMICOLON) {
        error_log_report_annotated(
            &p->tk.tc->error_log, ES_PARSER, false, "missing semicolon",
            p->tk.file, t->start, t->end,
            "expected ';' to terminate the declaration");
        return PE_UNEXPECTED_TOKEN;
    }
    tk_consume(&p->tk);
    iht_insert(&p->iht, (named_ast_node*)vd);
    *n = (ast_node*)vd;
    return PE_OK;
}
parse_error
parse_func_decl(parser* p, ureg start, astn_flags flags, ast_node** n)
{
    token* t;
    parse_error pe;
    PEEK(p, t);
    if (t->type != TT_STRING) {
        error_log_report_annotated_twice(
            &p->tk.tc->error_log, ES_PARSER, false,
            "invalid function declaration syntax", p->tk.file, t->start, t->end,
            "expected function identifier", start, t->end,
            "in this function declaration");
        return PE_UNEXPECTED_TOKEN;
    }
    ureg decl_end = t->end;
    astn_function* f = alloc_perm(p, sizeof(astn_function));
    if (!f) return PE_INSANE;
    f->nastn.name = alloc_string_perm(p, t->str);
    if (!f->nastn.name) return PE_INSANE;
    pe = nastn_fill_srange(p, &f->nastn, start, decl_end);
    if (pe) return pe;
    tk_void(&p->tk);
    PEEK(p, t);
    if (t->type == TT_BRACKET_OPEN) {
        // TODO: parse generic parameter list
    }
    if (t->type != TT_PAREN_OPEN) {
        error_log_report_annotated_twice(
            &p->tk.tc->error_log, ES_PARSER, false,
            "invalid function declaration syntax", p->tk.file, t->start, t->end,
            "expected '(' to start parameter list", start, decl_end,
            "in this function declaration");
        return PE_UNEXPECTED_TOKEN;
    }
    tk_void(&p->tk);
    PEEK(p, t);
    astn_param_decl** head = &f->params;
    while (t->type != TT_PAREN_CLOSE) {
        pe = parse_param_decl(
            p, (named_ast_node*)f, head, start, decl_end,
            "in this function declaration");
        if (pe) {
            *head = NULL;
            return pe;
        }
        head = (astn_param_decl**)&(*head)->nastn.astn.next;
        PEEK(p, t);
        if (t->type == TT_COMMA) {
            tk_void(&p->tk);
        }
        else if (t->type != TT_PAREN_CLOSE) {
            error_log_report_annotated_twice(
                &p->tk.tc->error_log, ES_PARSER, false,
                "invalid function declaration syntax", p->tk.file, t->start,
                t->end, "expected ',' or ')'", start, decl_end,
                "in this function declaration");
        }
    }
    *head = NULL;
    tk_void(&p->tk);
    f->nastn.parent = p->curr_parent;
    f->nastn.astn.type = ASTNT_FUNCTION;
    f->nastn.astn.flags = flags;
    *n = (ast_node*)f;
    return parse_body(p, (named_ast_node*)f, &f->body, BPM_FUNCTION);
}
parse_error parse_statement(parser* p, ast_node** head, body_parser_mode bpm)
{
    parse_error pe;
    astn_flags flags = ASTN_FLAGS_DEFAULT;
    token* t;
    PEEK(p, t);
    ureg start = t->start;
    while (true) {
        if (t->type != TT_STRING) {
            if (flags) {
                error_log_report_annotated(
                    &p->tk.tc->error_log, ES_PARSER, false,
                    "unexpected token at module scope", p->tk.file, t->start,
                    t->end,
                    astn_flags_get_const(flags)
                        ? "expected keyword"
                        : "expected identifier or keyword");
                return PE_UNEXPECTED_TOKEN;
            }
            else {
                return PE_EOEX;
            }
        }
        keyword_id kw = kw_match(t->str);
        if (kw != KW_INVALID_KEYWORD) {
            bool is_flags_kw;
            int r =
                astn_flags_from_kw(p, &flags, kw, start, t->end, &is_flags_kw);
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
            pe = parse_func_decl(p, start, flags, head);
            return pe;
        } break;
        /*

        case KW_STRUCT: break;
        case KW_IMPORT: break;
        case KW_INCLUDE: break;
        case KW_MODULE: break;
        case KW_EXTEND: break;
        case KW_TRAIT: break;
        */
        default: {
            token* t2 = tk_peek_2nd(&p->tk);
            if (!t2) return PE_TK_ERROR;
            if (t2->type != TT_COLON) {
                error_log_report_annotated(
                    &p->tk.tc->error_log, ES_PARSER, false,
                    "unexpected token at module scope", p->tk.file, t2->start,
                    t2->end, "expected ':' to initiate a declaration");
                start = t2->start;
                tk_void(&p->tk);
                PEEK(p, t);
                continue;
            }
            tk_void_n(&p->tk, 2);
            return parse_var_decl(p, start, flags, t->str, head);
        } break;
        }
        PEEK(p, t);
        start = t->start;
    }
}

parse_error parse_braced_delimited_body(
    parser* p, token* t, ast_node** head, body_parser_mode bpm)
{
    ureg start = t->start;
    ureg bend = t->end;
    tk_void(&p->tk);
    parse_error pe;
    PEEK(p, t);
    while (t->type != TT_BRACE_CLOSE) {
        if (t->type != TT_EOF) {
            pe = parse_statement(p, head, bpm);
            if (!pe) {
                head = &(*head)->next;
            }
            else {
                *head = NULL;
                return pe;
            }
            PEEK(p, t);
        }
        else {
            char* ctx;
            switch (bpm) {
            case BPM_FUNCTION: ctx = "this function's body"; break;
            case BPM_LAMBDA: ctx = "this lambda's body"; break;
            case BPM_STRUCT: ctx = "this struct's body"; break;
            default: panic("unexpected bpm"); return PE_INSANE;
            }
            src_range sr;
            src_range_unpack(p->curr_parent->decl_range, &sr);
            error_log_report_annotated_thrice(
                &p->tk.tc->error_log, ES_PARSER, false, "unterminated scope",
                p->tk.file, t->start, t->end,
                "reached EOF before scope was closed", sr.start, sr.end, ctx,
                start, bend, "scope starts here");
            return PE_UNEXPECTED_TOKEN;
        }
    }
    tk_consume(&p->tk);
    *head = NULL;
    return PE_OK;
}

parse_error parse_body(
    parser* p, named_ast_node* parent, ast_node** head, body_parser_mode bpm)
{
    parse_error pe;
    token* t;
    PEEK(p, t);
    if (t->type != TT_BRACE_OPEN) {
        pe = parse_statement(p, head, bpm);
        if (pe) {
            *head = NULL;
        }
        else {
            (*head)->next = NULL;
        }
    }
    named_ast_node* old_parent = p->curr_parent;
    ast_node** old_head = p->curr_head;
    p->curr_parent = parent;
    p->curr_head = head;
    pe = parse_braced_delimited_body(p, t, head, bpm);
    p->curr_parent = old_parent;
    p->curr_head = old_head;
    return pe;
}