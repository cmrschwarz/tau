#include "parser.h"
#include "error_log.h"
#include "keywords.h"
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

parse_error parse_statement(parser* p, stmt** tgt);
parse_error parse_scope(parser* p, scope* s);
parse_error parse_body(parser* p, body* b, ast_node_type pt);
parse_error parse_expression(parser* p, expr** ex);
parse_error parse_expression_of_prec(parser* p, expr** ex, ureg prec);
parse_error parse_braced_delimited_body(
    parser* p, ureg bstart, ureg bend, body* b, ast_node_type pt);
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
char* get_parent_context_msg(parser* p)
{
    if (p->curr_scope == &p->root.scope) return NULL;
    switch (p->curr_scope->symbol.stmt.type) {
        case SC_FUNC: return "in this function's body";
        case SC_FUNC_GENERIC: return "in this generic function's body";
        case SC_STRUCT: return "in this struct's body";
        case SC_STRUCT_GENERIC: return "in this generic struct's body";
        case SC_TRAIT: return "in this struct's body";
        case SC_TRAIT_GENERIC: return "in this generic struct's body";
        case SC_MODULE: return "in this module's body";
        case SC_MODULE_GENERIC: return "in this generic module's body";
        case SC_EXTEND: return "in this extend statement's body";
        case EXPR_WHILE:
        case EXPR_FOR:
        case EXPR_FOR_EACH:
        case EXPR_LOOP: return "in this loop's body";
        case EXPR_IF: return "in this if expressions's body";
        case SC_EXTEND_GENERIC:
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
        src_range_unpack(p->curr_scope->symbol.decl_range, &sr);
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
        src_range_unpack(p->curr_scope->symbol.decl_range, &sr);
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
    r = list_builder_init(&p->lb, &p->tk.tc->stagemem, 64);
    if (r) {
        tk_fin(&p->tk);
        return r;
    }
    p->root.scope.symbol.name = NULL;
    p->root.scope.symbol.stmt.type = SC_MODULE;
    p->root.scope.body.children = NULL;
    p->root.scope.symbol.stmt.next = NULL;
    p->curr_scope = (scope*)&p->root;
    return OK;
}
void parser_fin(parser* p)
{
    tk_fin(&p->tk);
}
static inline parse_error
symbol_fill_srange(parser* p, symbol* sym, ureg start, ureg end)
{
    sym->decl_range = src_range_pack_lines(p->tk.tc, start, end);
    if (sym->decl_range == SRC_RANGE_INVALID) return PE_INSANE;
    return PE_OK;
}
static inline parse_error
expr_fill_srange(parser* p, expr* ex, ureg start, ureg end)
{
    ex->srange = src_range_pack_lines(p->tk.tc, start, end);
    if (ex->srange == SRC_RANGE_INVALID) return PE_INSANE;
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
    sv->value = alloc_string_stage(p, t->str);
    if (!sv->value) return NULL;
    if (expr_fill_srange(p, &sv->expr, t->start, t->end)) return NULL;
    return (expr*)sv;
}
parse_error expr_to_stmt(parser* p, stmt** tgt, expr* e, ureg start, ureg end)
{
    stmt_expr* s = alloc_perm(p, sizeof(stmt_expr));
    if (!s) return PE_INSANE;
    s->stmt.type = STMT_EXPRESSION;
    s->expr_range = src_range_pack_lines(p->tk.tc, start, end);
    s->expr = e;
    if (s->expr_range == SRC_RANGE_INVALID) return PE_INSANE;
    *tgt = &s->stmt;
    return PE_OK;
}
parse_error parse_param_decl(
    parser* p, sym_param_decl** tgt, ureg ctx_start, ureg ctx_end,
    char* msg_context)
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
    sym_param_decl* d = alloc_perm(p, sizeof(sym_param_decl));
    if (!d) return PE_INSANE;
    d->symbol.name = alloc_string_perm(p, t->str);
    if (!d->symbol.name) return PE_INSANE;
    d->symbol.stmt.type = SYM_PARAM_DECL;
    // TODO: flags parsing
    d->symbol.stmt.flags = ASTN_FLAGS_DEFAULT;
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
            if (!*tgt) return PE_INSANE;
            **tgt = prefetch;
            *(*tgt + 1) = NULL;
            return PE_OK;
        }
    }
    void** list_start = list_builder_start(&p->lb);
    if (prefetch) list_builder_add(&p->lb, prefetch);
    while (true) {
        expr* ex;
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
    *tgt = (expr**)list_builder_pop_list_zt(
        &p->lb, list_start, &p->tk.tc->permmem, &count);
    if (!*tgt) return PE_INSANE;
    return PE_OK;
}
static inline parse_error parse_tuple(parser* p, token* t, expr** ex)
{
    ureg t_start = t->start;
    ureg t_end = t->end;
    tk_void(&p->tk);
    expr_tuple* tp = alloc_perm(p, sizeof(expr_tuple));
    if (!tp) return PE_INSANE;
    tp->expr.type = EXPR_TUPLE;
    parse_error pe =
        parse_expr_node_list(p, NULL, &tp->elements, "tuple", TT_BRACKET_CLOSE);
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
    if (expr_fill_srange(p, &tp->expr, t_start, t->end)) return PE_INSANE;
    tk_void(&p->tk);
    *ex = (expr*)tp;
    return PE_OK;
}

static inline parse_error
parse_expr_body_or_array(parser* p, token* t, expr** ex)
{
    ureg t_start = t->start;
    ureg t_end = t->end;
    tk_void(&p->tk);
    stmt* s = NULL;
    expr* e = NULL;
    ast_node_type pold = p->parent_type;
    p->parent_type = EXPR_ARRAY;
    parse_error pe = parse_statement(p, &s);
    p->parent_type = pold;
    if (pe == PE_NOT_A_STATEMENT) {
        PEEK(p, t);
        if (t->type != TT_BRACE_CLOSE) {
            pe = parse_expression(p, &e);
            if (pe == PE_EOEX) {
                PEEK(p, t);
                parser_error_1a_pc(
                    p, "invalid expression token", t->start, t->end,
                    "expected expression");
                return PE_HANDLED;
            }
            if (pe) return pe;
            PEEK(p, t);
            if (t->type == TT_SEMICOLON) {
                tk_void(&p->tk);
                pe = expr_to_stmt(
                    p, &s, e, src_range_get_start(e->srange), t->end);
                if (pe) return pe;
            }
            else if (t->type == TT_COMMA) {
                tk_void(&p->tk);
            }
        }
    }
    else if (pe != PE_OK) {
        return pe;
    }
    if (s) {
        expr_block* b = alloc_perm(p, sizeof(expr_block));
        if (!b) return PE_INSANE;
        b->expr.type = EXPR_BLOCK;
        pe = parse_braced_delimited_body(
            p, t_start, t_end, &b->body, EXPR_BLOCK);
        if (pe) return pe;
        s->next = b->body.children;
        b->body.children = s;
        *ex = (expr*)b;
        return PE_OK;
    }
    expr_array* arr = alloc_perm(p, sizeof(expr_array));
    if (!arr) return PE_INSANE;
    arr->expr.type = EXPR_ARRAY;
    if (e) {
        pe =
            parse_expr_node_list(p, e, &arr->elements, "array", TT_BRACE_CLOSE);
        // EMSG: suboptimal e.g. for case {,,}
        if (pe == PE_UNEXPECTED_TOKEN) {
            PEEK(p, t);
            parser_error_2a(
                p, "unclosed array", t->start, t->end,
                "reached end of expression due to unexpected token", t_start,
                t_end, "didn't find a matching brace for this array");
            return PE_HANDLED;
        }
        if (pe != PE_OK) return pe;
        PEEK(p, t);
    }
    else {
        arr->elements = (expr**)&NULL_PTR;
    }
    if (expr_fill_srange(p, &arr->expr, t_start, t->end)) return PE_INSANE;
    tk_void(&p->tk);
    *ex = (expr*)arr;
    return PE_OK;
}
static inline parse_error
parse_parenthesis_group(parser* p, token* t, expr** ex)
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
    pr->expr.type = EXPR_OP_PARENTHESES;
    pr->child = *ex;
    if (expr_fill_srange(p, &pr->expr, t_start, t->end)) return PE_INSANE;
    tk_void(&p->tk);
    *ex = (expr*)pr;
    return PE_OK;
}
static inline parse_error
parse_prefix_unary_op(parser* p, token* t, ast_node_type op, expr** ex)
{
    expr_op_unary* ou = (expr_op_unary*)alloc_perm(p, sizeof(expr_op_unary));
    if (!ou) return PE_INSANE;
    if (expr_fill_srange(p, &ou->expr, t->start, t->end)) return PE_INSANE;
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
parse_error parse_return_expr(parser* p, ureg start, ureg end, expr** tgt)
{
    expr_return* r = alloc_perm(p, sizeof(expr_return));
    if (!r) return PE_INSANE;
    r->expr.type = EXPR_RETURN;
    token* t;
    PEEK(p, t);
    if (t->type == TT_SEMICOLON) {
        r->value = NULL;
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
    }
    *tgt = &r->expr;
    return PE_OK;
}
parse_error parse_goto_expr(parser* p, ureg start, ureg end, expr** tgt)
{
    token* t;
    PEEK(p, t);
    if (t->type != TT_STRING) {
        parser_error_2a(
            p, "missing label in goto expression", t->start, t->end,
            "expected label name", start, end, "");
    }
    expr_goto* g = alloc_perm(p, sizeof(expr_goto));
    g->expr.type = EXPR_GOTO;
    g->target.name = alloc_string_stage(p, t->str);
    if (!g->target.name) return PE_INSANE;
    tk_void(&p->tk);
    *tgt = (expr*)g;
    return PE_OK;
}
parse_error parse_give_expr(parser* p, ureg start, ureg end, expr** tgt)
{
    expr_give* g = alloc_perm(p, sizeof(expr_give));
    if (!g) return PE_INSANE;
    g->expr.type = EXPR_GIVE;
    g->target.name = NULL;
    token* t1;
    PEEK(p, t1);
    if (t1->type == TT_STRING) {
        token* t2 = tk_peek_2nd(&p->tk);
        if (!t2) return PE_TK_ERROR;
        if (t2->type != TT_SEMICOLON && token_to_binary_op(t2) == OP_NOOP &&
            token_to_postfix_unary_op(t2) == OP_NOOP) {
            g->target.name = alloc_string_stage(p, t1->str);
            if (!g->target.name) return PE_INSANE;
            tk_void(&p->tk);
        }
    }
    parse_error pe = parse_expression(p, &g->value);
    if (pe == PE_EOEX) {
        PEEK(p, t1);
        parser_error_2a(
            p, "unexpected token in give expression", t1->start, t1->end,
            "expected expression or ';'", start, end,
            "in this give expression");
    }
    if (pe) return pe;
    *tgt = &g->expr;
    return PE_OK;
}
parse_error parse_expr_body(parser* p, expr** tgt, ast_node body_type)
{
    ast_node_type old_parent_type = p->parent_type;
    p->parent_type = body_type;
    parse_error pe = parse_expression(p, tgt);
    if (pe == PE_EOEX) {
        token* t;
        PEEK(p, t);
        parser_error_1a_pc(
            p, "invalid expression token", t->start, t->end,
            "expected expression");
        pe = PE_HANDLED;
    }
    p->parent_type = old_parent_type;
    return pe;
}
parse_error parse_loop(parser* p, expr** tgt, ureg start, ureg end, char* label)
{
    expr_loop* l = alloc_perm(p, sizeof(expr_loop));
    if (!l) return PE_INSANE;
    l->expr_named.expr.srange = src_range_pack_lines(p->tk.tc, start, end);
    l->expr_named.name = label;
    l->expr_named.expr.type = EXPR_LOOP;
    *tgt = (expr*)l;
    return parse_expr_body(p, &l->body, EXPR_LOOP);
}
parse_error
parse_while(parser* p, expr** tgt, ureg start, ureg end, char* label)
{
    expr_while* w = alloc_perm(p, sizeof(expr_while));
    if (!w) return PE_INSANE;
    parse_error pe = parse_expression(p, &w->condition);
    token* t;
    if (pe == PE_EOEX) {
        PEEK(p, t);
        parser_error_2a(
            p, "invalid while loop syntax", t->start, t->end,
            "expected while condition expression", start, end,
            "in this while loop");
        return PE_HANDLED;
    }
    if (pe) return pe;
    w->expr_named.expr.srange = src_range_pack_lines(p->tk.tc, start, end);
    w->expr_named.name = label;
    w->expr_named.expr.type = EXPR_WHILE;
    *tgt = (expr*)w;
    pe = parse_expr_body(p, &w->while_body, EXPR_WHILE);
    if (pe) return pe;
    PEEK(p, t);
    if (t->type != TT_STRING || !kw_equals(KW_FINALLY, t->str)) {
        parser_error_2a(
            p, "invalid while loop syntax", t->start, t->end,
            "expected finally block", start, end, "for this while loop");
        return PE_HANDLED;
    }
    tk_void(&p->tk);
    pe = parse_expr_body(p, &w->finally_body, EXPR_WHILE);
    return pe;
}
parse_error parse_if(parser* p, expr** tgt, ureg start, ureg end)
{
    expr_if* i = alloc_perm(p, sizeof(expr_if));
    if (!i) return PE_INSANE;
    parse_error pe = parse_expression(p, &i->condition);
    token* t;
    if (pe == PE_EOEX) {
        PEEK(p, t);
        parser_error_2a(
            p, "invalid if expression syntax", t->start, t->end,
            "expected if condition expression", start, end,
            "in this while loop");
        return PE_HANDLED;
    }
    if (pe) return pe;
    i->expr.srange = src_range_pack_lines(p->tk.tc, start, end);
    i->expr.type = EXPR_IF;
    *tgt = (expr*)i;
    pe = parse_expr_body(p, &i->if_body, EXPR_IF);
    if (pe) return pe;
    PEEK(p, t);
    if (t->type != TT_STRING || !kw_equals(KW_ELSE, t->str)) {
        parser_error_2a(
            p, "invalid if expression syntax", t->start, t->end,
            "expected else block", start, end, "for this if expression");
        return PE_HANDLED;
    }
    tk_void(&p->tk);
    pe = parse_expr_body(p, &i->else_body, EXPR_IF);
    return pe;
}
static inline parse_error parse_single_value(parser* p, token* t, expr** ex)
{
    switch (t->type) {
        case TT_PAREN_OPEN: {
            return parse_parenthesis_group(p, t, ex);
        }
        case TT_BRACKET_OPEN: {
            return parse_tuple(p, t, ex);
        }
        case TT_BRACE_OPEN: {
            return parse_expr_body_or_array(p, t, ex);
        }
        case TT_STRING: {
            keyword_id kw = kw_match(t->str);
            switch (kw) {
                case KW_LOOP: {
                    tk_void(&p->tk);
                    return parse_loop(p, ex, t->start, t->end, NULL);
                }
                case KW_LABEL: {
                }
                case KW_FOR: {
                }
                case KW_WHILE: {
                    tk_void(&p->tk);
                    return parse_while(p, ex, t->start, t->end, NULL);
                }
                case KW_IF: {
                    tk_void(&p->tk);
                    return parse_if(p, ex, t->start, t->end);
                }
                case KW_RETURN: {
                    tk_void(&p->tk);
                    return parse_return_expr(p, t->start, t->end, ex);
                }
                case KW_GIVE: {
                    tk_void(&p->tk);
                    return parse_give_expr(p, t->start, t->end, ex);
                }
                case KW_GOTO: {
                    tk_void(&p->tk);
                    return parse_goto_expr(p, t->start, t->end, ex);
                }
                case KW_INVALID_KEYWORD:
                default:; // fallthrough
            }
        } // fallthrough
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
static inline parse_error parse_call(parser* p, token* t, expr** ex, expr* lhs)
{
    ureg t_start = t->start;
    tk_void(&p->tk);
    expr_call* call = alloc_perm(p, sizeof(expr_call));
    if (!call) return PE_INSANE;
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
    if (expr_fill_srange(p, &call->expr, t_start, t->end)) return PE_INSANE;
    tk_void(&p->tk);
    call->expr.type = EXPR_OP_CALL;
    call->expr.op_type = OP_CALL;
    call->lhs = lhs;
    *ex = (expr*)call;
    return PE_OK;
}
static inline parse_error
parse_access(parser* p, token* t, expr** ex, expr* lhs)
{
    ureg t_start = t->start;
    tk_void(&p->tk);
    expr_access* acc = alloc_perm(p, sizeof(expr_access));
    if (!acc) return PE_INSANE;
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
    if (expr_fill_srange(p, &acc->expr, t_start, t->end)) return PE_INSANE;
    tk_void(&p->tk);
    acc->expr.type = EXPR_OP_ACCESS;
    acc->expr.op_type = OP_ACCESS;
    acc->lhs = lhs;
    *ex = (expr*)acc;
    return PE_OK;
}
static inline parse_error
parse_postfix_unary_op(parser* p, token* t, op_type op, expr** ex, expr* lhs)
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
        ou->expr.type = EXPR_OP_UNARY;
        ou->expr.op_type = op;
        ou->child = lhs;
        if (expr_fill_srange(p, &ou->expr, t->start, t->end)) return PE_INSANE;
        *ex = (expr*)ou;
        return PE_OK;
    }
}
static inline parse_error
parse_binary_op(parser* p, token* t, op_type op, expr** ex, expr* lhs)
{
    tk_void(&p->tk);
    expr_op_binary* ob = (expr_op_binary*)alloc_perm(p, sizeof(expr_op_binary));
    if (!ob) return PE_INSANE;
    if (expr_fill_srange(p, &ob->expr, t->start, t->end)) return PE_INSANE;
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
parse_error parse_expression_of_prec(parser* p, expr** ex, ureg prec)
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
    return parse_expression_of_prec_post_value(p, ex, prec);
}
parse_error parse_expression(parser* p, expr** ex)
{
    return parse_expression_of_prec(p, ex, PREC_BASELINE);
}
parse_error parse_eof_delimited_body(parser* p, body* b, ast_node_type pt)
{
    stmt** head = &b->children;
    *head = NULL; // fist element must be zero for extend to check
    token* t;
    PEEK(p, t);
    parse_error pe = PE_OK;
    ast_node_type old_parent_type = p->parent_type;
    p->parent_type = pt;
    while (t->type != TT_EOF) {
        pe = parse_statement(p, head);
        if (!pe) {
            head = &(*head)->next;
        }
        else {
            break;
        }
        t = tk_peek(&p->tk);
        if (!t) {
            pe = PE_TK_ERROR;
            break;
        }
    }
    *head = NULL;
    p->parent_type = old_parent_type;
    return pe;
}
parse_error parser_parse_file(parser* p, file* f)
{
    // This is test code. it sucks
    int r = tk_open_file(&p->tk, f);
    if (r) return PE_TK_ERROR;
    stmt* old_children = p->root.scope.body.children;
    parse_error pe =
        parse_eof_delimited_body(p, &p->root.scope.body, SC_MODULE);
    stmt** old_head = &old_children;
    while (*old_head) old_head = &(*old_head)->next;
    *old_head = p->root.scope.body.children;
    p->root.scope.body.children = old_children;
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
            char* msgstrs[5];
            msgstrs[0] = "'";
            msgstrs[1] = access_modifier_string(am);
            msgstrs[2] = "' conflicts with previous '";
            msgstrs[3] = access_modifier_string(old_am);
            msgstrs[4] = "'";
            char* msg = error_log_cat_strings(&p->tk.tc->error_log, 4, msgstrs);
            if (!msg) return PE_INSANE;
            error_log_report_annotated(
                &p->tk.tc->error_log, ES_PARSER, false,
                "conflicting access modifiers specified", p->tk.file, start,
                end, msg);
        }
        return PE_UNEXPECTED_TOKEN;
    }
    return PE_OK;
}
parse_error stmt_flags_from_kw(
    parser* p, stmt_flags* f, keyword_id kw, ureg start, ureg end)
{
    // TODO: enforce order
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
                return PE_UNEXPECTED_TOKEN;
            }
            stmt_flags_set_const(f);
        } break;
        case KW_SEALED: {
            if (stmt_flags_get_sealed(*f) != false) {
                report_redundant_specifier(
                    p, keyword_strings[KW_SEALED], start, end);
                return PE_UNEXPECTED_TOKEN;
            }
            stmt_flags_set_sealed(f);
        } break;
        case KW_VIRTUAL: {
            if (stmt_flags_get_virtual(*f) != false) {
                report_redundant_specifier(
                    p, keyword_strings[KW_VIRTUAL], start, end);
                return PE_UNEXPECTED_TOKEN;
            }
            stmt_flags_set_virtual(f);
        } break;
        case KW_STATIC: {
            if (stmt_flags_get_static(*f) != false) {
                report_redundant_specifier(
                    p, keyword_strings[KW_STATIC], start, end);
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
parse_error parse_var_decl(
    parser* p, ureg start, ureg col_end, stmt_flags flags, string ident,
    stmt** n)
{
    parse_error pe;
    sym_var* vd = alloc_perm(p, sizeof(sym_var));
    if (!vd) return PE_INSANE;
    vd->symbol.name = alloc_string_perm(p, ident);
    if (!vd->symbol.name) return PE_INSANE;
    vd->symbol.stmt.type = SYM_VAR_DECL;
    vd->symbol.stmt.flags = flags;
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
    symbol_fill_srange(p, &vd->symbol, start, t->end);
    tk_consume(&p->tk);
    *n = (stmt*)vd;
    return PE_OK;
}
parse_error parse_param_list(
    parser* p, sym_param_decl** tgt, bool generic, ureg ctx_start, ureg ctx_end,
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
        tgt = (sym_param_decl**)&(*tgt)->symbol.stmt.next;
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
    scope* sc;
    bool generic;
    if (t->type == TT_BRACKET_OPEN) {
        generic = true;
        sc = alloc_perm(p, sizeof(sc_func_generic));
        if (!sc) return PE_INSANE;
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
        if (!sc) return PE_INSANE;
    }
    sc->symbol.name = name;
    sc->parent = p->curr_scope;
    pe = symbol_fill_srange(p, &sc->symbol, start, decl_end);
    if (pe) return pe;
    if (t->type != TT_PAREN_OPEN) {
        parser_error_2a(
            p, "invalid function declaration syntax", t->start, t->end,
            "expected '(' to start parameter list", start, decl_end,
            "in this function declaration");
        return PE_HANDLED;
    }
    tk_void(&p->tk);
    sym_param_decl** pd =
        generic ? &((sc_func_generic*)sc)->params : &((sc_func*)sc)->params;
    pe = parse_param_list(
        p, pd, false, start, decl_end, "in this function declaration");
    if (pe) return pe;
    sc->symbol.stmt.type = generic ? SC_FUNC_GENERIC : SC_FUNC;
    sc->symbol.stmt.flags = flags;
    *n = (stmt*)sc;
    return parse_scope(p, sc);
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
    scope* st;
    bool generic;
    if (t->type == TT_BRACKET_OPEN) {
        generic = true;
        st = alloc_perm(p, sizeof(sc_struct_generic));
        if (!st) return PE_INSANE;
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
        if (!st) return PE_INSANE;
    }
    st->symbol.name = name;
    pe = symbol_fill_srange(p, &st->symbol, start, decl_end);
    if (pe) return pe;
    st->symbol.stmt.type = generic ? SC_STRUCT_GENERIC : SC_STRUCT;
    st->symbol.stmt.flags = flags;
    *n = (stmt*)st;
    return parse_body(p, &st->body, st->symbol.stmt.type);
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
    scope* md;
    bool generic;
    if (t->type == TT_BRACKET_OPEN) {
        generic = true;
        md = alloc_perm(p, sizeof(sc_module_generic));
        if (!md) return PE_INSANE;
        tk_void(&p->tk);
        pe = parse_param_list(
            p, &((sc_module_generic*)md)->generic_params, true, start, decl_end,
            "in this module declaration");
        if (pe) return pe;
        PEEK(p, t);
    }
    else {
        generic = false;
        md = alloc_perm(p, sizeof(sc_module));
        if (!md) return PE_INSANE;
    }
    md->symbol.name = name;
    pe = symbol_fill_srange(p, &md->symbol, start, decl_end);
    if (pe) return pe;
    md->symbol.stmt.type = generic ? SC_MODULE_GENERIC : SC_MODULE;
    md->symbol.stmt.flags = flags;
    *n = (stmt*)md;
    return parse_eof_delimited_body(p, &md->body, md->symbol.stmt.type);
}
parse_error parse_extend_decl(parser* p, ureg start, stmt_flags flags, stmt** n)
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
    scope* sc;
    bool generic;
    if (t->type == TT_BRACKET_OPEN) {
        generic = true;
        sc = alloc_perm(p, sizeof(sc_extend_generic));
        if (!sc) return PE_INSANE;
        tk_void(&p->tk);
        pe = parse_param_list(
            p, &((sc_extend_generic*)sc)->generic_params, true, start, decl_end,
            "in this extend declaration");
        if (pe) return pe;
        PEEK(p, t);
    }
    else {
        generic = false;
        sc = alloc_perm(p, sizeof(sc_extend));
        if (!sc) return PE_INSANE;
    }
    sc->symbol.name = name;
    pe = symbol_fill_srange(p, &sc->symbol, start, decl_end);
    if (pe) return pe;
    sc->symbol.stmt.type = generic ? SC_EXTEND_GENERIC : SC_EXTEND;
    sc->symbol.stmt.flags = flags;
    sc->parent = p->curr_scope;
    PEEK(p, t);
    if (t->type == TT_SEMICOLON) {
        if (p->curr_scope->body.children == NULL) {
            tk_consume(&p->tk);
            pe = parse_eof_delimited_body(
                p, &sc->body, generic ? SC_EXTEND_GENERIC : SC_EXTEND);
            *n = (stmt*)sc;
            return pe;
        }
        else {
            *n = NULL;
            stmt* head = p->curr_scope->body.children;
            while (head->next != NULL) head = head->next;
            ureg hs, he;
            stmt_get_highlight_bounds(head, &hs, &he);
            parser_error_2a_pc(
                p, "non leading extend statement", start, t->end, "", hs, he,
                "preceeded by this statement");
            return PE_HANDLED;
        }
    }
    *n = (stmt*)sc;
    return parse_body(p, &sc->body, sc->symbol.stmt.type);
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
    scope* tr;
    bool generic;
    if (t->type == TT_BRACKET_OPEN) {
        generic = true;
        tr = alloc_perm(p, sizeof(sc_trait_generic));
        if (!tr) return PE_INSANE;
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
        if (!tr) return PE_INSANE;
    }
    tr->symbol.name = name;
    pe = symbol_fill_srange(p, &tr->symbol, start, decl_end);
    if (pe) return pe;
    tr->symbol.stmt.type = generic ? SC_TRAIT_GENERIC : SC_TRAIT;
    tr->symbol.stmt.flags = flags;
    *n = (stmt*)tr;
    return parse_body(p, &tr->body, tr->symbol.stmt.type);
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
bool body_customizes_exprs(ast_node_type pt)
{
    return pt == EXPR_ARRAY;
}
parse_error parse_expr_stmt(parser* p, stmt** tgt)
{
    expr* expr;
    token* t;
    PEEK(p, t);
    ureg start = t->start;
    parse_error pe = parse_expression(p, &expr);
    if (pe == PE_EOEX) {
        PEEK(p, t);
        parser_error_1a_pc(
            p, "unexpected token in expression statement", t->start, t->end,
            "");
        return PE_HANDLED;
    }
    if (pe) return pe;
    PEEK(p, t);
    if (t->type != TT_SEMICOLON) {
        ureg end;
        get_expr_bounds(expr, NULL, &end); // TODO improve for loops etc.
        parser_error_2a(
            p, "missing semicolon", t->start, t->end,
            "expected ';' to terminate the expression statement", start, end,
            "in this expression");
        return PE_HANDLED;
    }
    tk_void(&p->tk);
    if (pe) return pe;
    return expr_to_stmt(p, tgt, expr, start, t->end);
}
parse_error
parse_alias(parser* p, ureg start, ureg end, stmt_flags flags, stmt** tgt)
{
    sym_alias* a = alloc_perm(p, sizeof(sym_alias));
    if (!a) return PE_INSANE;
    a->symbol.stmt.type = SYM_ALIAS;
    a->symbol.stmt.flags = flags;
    token* t;
    PEEK(p, t);
    if (t->type == TT_STAR) {
        a->symbol.name = NULL;
    }
    else if (t->type == TT_STRING) {
        a->symbol.name = alloc_string_perm(p, t->str);
        if (!a->symbol.name) return PE_INSANE;
    }
    else {
        parser_error_2a(
            p, "invalid alias syntax", t->start, t->end,
            "expected '*' or an identifier", start, t->end, "");
    }
    tk_void(&p->tk);
    PEEK(p, t);
    if (t->type != TT_ARROW) {
        parser_error_2a(
            p, "unexpected token in alias statement", t->start, t->end,
            "expected '->'", start, end, "in this alias statement");
        return PE_HANDLED;
    }
    tk_void(&p->tk);
    parse_error pe = parse_expression(p, &a->target);
    if (pe == PE_EOEX) {
        parser_error_2a(
            p, "unexpected token in alias statement", t->start, t->end,
            "expected expression", start, end, "in this alias statement");
        return PE_HANDLED;
    }
    PEEK(p, t);
    if (t->type != TT_SEMICOLON) {
        parser_error_2a(
            p, "missing semicolon in alias statement", t->start, t->end,
            "expected ';' to terminate alias statement", start, end,
            "alias statement started here");
        return PE_HANDLED;
    }
    tk_void(&p->tk);
    *tgt = &a->symbol.stmt;
    return PE_OK;
}
parse_error parse_label(parser* p, ureg start, ureg end, stmt** tgt)
{
    token* t;
    PEEK(p, t);
    if (t->type != TT_STRING) {
        parser_error_2a(
            p, "missing label name", t->start, t->end, "expected label name",
            start, end, "");
        return PE_HANDLED;
    }
    char* label_name = alloc_string_stage(p, t->str);
    if (!label_name) return PE_INSANE;
    tk_void(&p->tk);
    PEEK(p, t);
    if (t->type == TT_SEMICOLON) {
        tk_void(&p->tk);
        sym_label* g = alloc_perm(p, sizeof(sym_label));
        g->symbol.stmt.type = SYM_LABEL;
        g->symbol.name = label_name;
        *tgt = (stmt*)g;
        return PE_OK;
    }
    else {
        parse_error pe;
        expr* ex;
        keyword_id kw = kw_match(t->str);
        switch (kw) {
            case KW_WHILE:
                tk_void(&p->tk);
                pe = parse_while(p, &ex, t->start, t->end, label_name);
                break;
            case KW_LOOP:
                tk_void(&p->tk);
                pe = parse_loop(p, &ex, t->start, t->end, label_name);
                break;
            case KW_FOR:    // TODO
            case KW_SWITCH: // TODO
            default:
                parser_error_2a(
                    p, "invalid label statement syntax", t->start, t->end,
                    "expected loop statement or semicolon", start, end, "");
                return PE_HANDLED;
        }
        if (pe) return pe;
        pe = parse_expression_of_prec_post_value(p, &ex, PREC_BASELINE);
        if (pe == PE_EOEX) {
            PEEK(p, t);
            parser_error_1a(
                p, "missing semicolon in label expression statement", t->start,
                t->end, "expected ';' to terminate expression statement");
        }
        tk_void(&p->tk);
        return expr_to_stmt(p, tgt, ex, start, t->end);
    }
    return PE_OK;
}

parse_error parse_statement(parser* p, stmt** tgt)
{
    parse_error pe;
    stmt_flags flags = ASTN_FLAGS_DEFAULT;
    token* t;
    PEEK(p, t);
    ureg start = t->start;
    while (true) {
        if (t->type != TT_STRING) {
            if (flags == ASTN_FLAGS_DEFAULT) {
                if (body_supports_exprs(p->parent_type)) {
                    return parse_expr_stmt(p, tgt);
                }
                else if (body_customizes_exprs(p->parent_type)) {
                    return PE_NOT_A_STATEMENT;
                }
            }
            parser_error_1a_pc(
                p, "unexpected token in statement", t->start, t->end,
                stmt_flags_get_const(flags) ? "expected keyword"
                                            : "expected identifier or keyword");
            return PE_HANDLED;
        }
        token* t2 = tk_peek_2nd(&p->tk);
        if (!t2) return PE_TK_ERROR;
        if (t2->type == TT_COLON) {
            tk_void_n(&p->tk, 2);
            return parse_var_decl(p, start, t2->end, flags, t->str, tgt);
        }
        keyword_id kw = kw_match_visibility_or_mutability(t->str);
        if (kw != KW_INVALID_KEYWORD) {
            pe = stmt_flags_from_kw(p, &flags, kw, start, t->end);
            if (pe == PE_OK) {
                tk_void(&p->tk);
                PEEK(p, t);
                continue;
            }
            if (pe != PE_EOEX) return pe;
            // fallthrough on pe == PE_EOEX
        }
        if (token_to_binary_op(t2) == OP_NOOP &&
            token_to_postfix_unary_op(t2) == OP_NOOP) {
            kw = kw_match(t->str);
            switch (kw) {
                case KW_FUNC: {
                    tk_void(&p->tk);
                    return parse_func_decl(p, start, flags, tgt);
                }
                case KW_STRUCT: {
                    tk_void(&p->tk);
                    return parse_struct_decl(p, start, flags, tgt);
                }
                case KW_TRAIT: {
                    tk_void(&p->tk);
                    return parse_trait_decl(p, start, flags, tgt);
                }
                case KW_MODULE: {
                    tk_void(&p->tk);
                    return parse_module_decl(p, start, flags, tgt);
                }
                case KW_EXTEND: {
                    tk_void(&p->tk);
                    return parse_extend_decl(p, start, flags, tgt);
                }
                case KW_ALIAS: {
                    tk_void(&p->tk);
                    return parse_alias(p, start, t->end, flags, tgt);
                }
                default:; // fallthrough
            }
            if (flags == ASTN_FLAGS_DEFAULT) {
                // TODO: require, import, include
                switch (kw) {
                    case KW_LABEL: {
                        tk_void(&p->tk);
                        return parse_label(p, start, t->end, tgt);
                    }
                    default:; // fallthrough
                }
            }
        }
        else if (t2->type == TT_STAR && kw_equals(KW_ALIAS, t->str)) {
            t2 = tk_peek_3rd(&p->tk);
            if (t2->type == TT_ARROW) {
                tk_void(&p->tk);
                return parse_alias(p, start, t->end, flags, tgt);
            }
        }
        if (flags == ASTN_FLAGS_DEFAULT) {
            if (body_supports_exprs(p->parent_type)) {
                return parse_expr_stmt(p, tgt);
            }
            else if (body_customizes_exprs(p->parent_type)) {
                return PE_NOT_A_STATEMENT;
            }
        }
        tk_void(&p->tk);
        PEEK(p, t);
        parser_error_1a_pc(
            p, "unexpected token in statement", t->start, t->end,
            "expected ':' to initiate a declaration");
        return PE_UNEXPECTED_TOKEN;
    }
}
parse_error parse_braced_delimited_body(
    parser* p, ureg bstart, ureg bend, body* b, ast_node_type pt)
{
    parse_error pe;
    token* t;
    PEEK(p, t);
    stmt** head = &b->children;
    *head = NULL; // fist element must be zero for extend to check

    ast_node_type old_parent_type = p->parent_type;
    p->parent_type = pt;

    while (t->type != TT_BRACE_CLOSE) {
        if (t->type != TT_EOF) {
            pe = parse_statement(p, head);
            if (!pe) {
                head = &(*head)->next;
            }
            else {
                break;
            }
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
        b->body_end = t->end;
    }
    *head = NULL;
    p->parent_type = old_parent_type;
    return pe;
}
// TODO: parse eof delimited scope, parse brace delimited scope
parse_error parse_scope(parser* p, scope* s)
{
    scope* old_scope = p->curr_scope;
    p->curr_scope = s;
    parse_error pe = parse_body(p, &s->body, s->symbol.stmt.type);
    p->curr_scope = old_scope;
    return pe;
}
parse_error parse_body(parser* p, body* b, ast_node_type pt)
{
    parse_error pe;
    token* t;
    PEEK(p, t);
    if (t->type != TT_BRACE_OPEN) {
        ast_node_type old_parent_type = p->parent_type;
        p->parent_type = pt;
        pe = parse_statement(p, &b->children);
        if (pe) {
            b->children = NULL;
        }
        else {
            b->children->next = NULL;
            b->body_end =
                src_range_get_end(((stmt_expr*)b->children)->expr_range);
        }
        p->parent_type = old_parent_type;
    }
    else {
        tk_void(&p->tk);
        pe = parse_braced_delimited_body(p, t->start, t->end, b, pt);
    }
    return pe;
}