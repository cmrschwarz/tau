#include "parser.h"
#include "tokenizer.h"
#include "error_log.h"
#include "keywords.h"
#include "tauc.h"
#include "print_ast.h"
#include "utils/math_utils.h"
#include <stddef.h>

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

static inline bool is_left_associative(expr_node_type t){
    switch(t){
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
        case OP_BITWISE_NOT_ASSIGN:
            return false;
        default: 
            return true;
    }
}

static inline expr_node_type token_to_binary_op(token* t){
    switch(t->type){
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
static inline expr_node_type token_to_prefix_unary_op(token* t){
    switch (t->type){
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
        case TT_STRING: switch (kw_match(t->str)){
            case KW_CONST: return OP_CONST;
            default: return OP_NOOP;
        }
        default: return OP_NOOP;
    }
}
static inline expr_node_type token_to_postfix_unary_op(token* t){
    switch (t->type){
        case TT_DOUBLE_PLUS: return OP_POST_INCREMENT;
        case TT_DOUBLE_MINUS: return OP_POST_DECREMENT;
        case TT_PAREN_OPEN: return OP_CALL;
        case TT_BRACKET_OPEN: return OP_ACCESS;
        default: return OP_NOOP;
    }
}

static inline void* alloc_ppool(parser* p, ureg size, pool* pool){
    void* mem = pool_alloc(pool, size);
    if(!mem) error_log_report_allocation_failiure(&p->tk.tc->error_log);
    return mem;
}
static inline void* alloc_stage(parser* p, ureg size){
   return alloc_ppool(p, size, &p->tk.tc->stagemem);
}
static inline void* alloc_perm(parser* p, ureg size){
    return alloc_ppool(p, size, &p->tk.tc->permmem);
}
static inline char* alloc_string_ppool(parser* p, string s, pool* pool){
    ureg len = string_len(s);
    char* mem = (char*)alloc_ppool(p, len + 1, pool);
    if(!mem)return NULL;
    memcpy(mem, s.start, len);
    mem[len] = '\0';
    return mem;
}
static inline char* alloc_string_stage(parser* p, string s){
   return alloc_string_ppool(p, s, &p->tk.tc->stagemem);
}
static inline char* alloc_string_perm(parser* p, string s){
    return alloc_string_ppool(p, s, &p->tk.tc->permmem);
}

static inline int parser_error_1a(
    parser* p, char* msg, ureg start, ureg end, char* annot
){
    error_log_report_error_1_annotation(
        &p->tk.tc->error_log, ES_PARSER, false,
        msg, p->tk.file, 
        start, end, annot
    );
    return ERR;
}
static inline int parser_error_2a(
    parser* p, char* msg, ureg start, ureg end,
    char* annot, ureg start2, ureg end2, char* annot2
){
    error_log_report_error_2_annotations(
        &p->tk.tc->error_log, ES_PARSER, false,
        msg, p->tk.file, 
        start, end, annot,
        start2, end2, annot2
    );
    return ERR;
}
static inline int parser_error_1at(
    parser* p, char* msg,
    token* t, char* annot
){
    return parser_error_1a(p, msg, t->start, t->end, annot);
}
static inline token* parser_expect(
    parser* p, token_type tt, char* msg,
    char* ctx, ureg ctx_start, ureg ctx_end
){
    token* t = tk_consume(&p->tk);
    if(t->type != tt){
        char* expstr = "expected ";
        ureg explen = strlen(expstr);
        ureg toklen = strlen(token_strings[tt]);
        char* ann = (char*)error_log_alloc(
            &p->tk.tc->error_log, explen + toklen + 3
        );
        if(!ann)return NULL;
        memcpy(ann, expstr, explen);
        ann[explen] = '\'';
        memcpy(ann + explen + 1, token_strings[tt], toklen);
        ann[explen + 1 + toklen] = '\'';
        ann[explen + 1 + toklen + 1] = '\0';
        parser_error_2a(
            p, msg, t->start, t->end, ann, ctx_start, ctx_end, ctx
        );
        return NULL;
    }
    return t;
}
int parser_init(parser* p, thread_context* tc){
    int r = tk_init(&p->tk, tc);
    if(r) return r;
    r = list_builder_init(&p->lb, &p->tk.tc->stagemem, 64);
    if(r) return r;
    p->root.name = "void";
    p->root.parent = NULL;
    p->root.astn.type = ASTNT_MODULE;
    return OK;
}
void parser_fin(parser* p){
    tk_fin(&p->tk);
}

int parser_search_extend(parser* p){
    token* t = tk_peek(&p->tk);
    if(!t)return ERR;
    if(t->type == TT_STRING && kw_equals(KW_EXTEND, t->str)){
        ureg estart = t->start; 
        tk_void(&p->tk);
        t = tk_consume(&p->tk);
        if(t->type != TT_STRING) return parser_error_1at(
            p, "invalid extend statement syntax", t, "expected module name"
        );
        t = parser_expect(
            p, TT_SEMICOLON, "invalid extend statement syntax",
            "statement start", estart,
            estart + strlen(keyword_strings[KW_EXTEND])
        );
        if(!t)return ERR;
        astn_extend* e = (astn_extend*)alloc_stage(p, sizeof(astn_extend));
        if(!e)return ERR;
        e->nastn.astn.type = ASTNT_EXTEND;
        e->nastn.astn.next = NULL;
        e->nastn.parent = &p->root;
        e->nastn.name = (char*)alloc_string_stage(p, t->str);
        if(e->nastn.name) return ERR;
        e->body = NULL;
        e->imports = NULL;
        p->curr_parent->astn.next = (ast_node*)e;
        p->curr_parent = (named_ast_node*)e;
        return OK;
    }
}
static inline expr_node* parse_str_value(parser* p, token* t){
    expr_node_type ent;
    switch (t->type){
        case TT_BINARY_LITERAL: ent = ENT_BINARY_LITERAL; break;
        case TT_LITERAL: ent = ENT_STRING_LITERAL; break;
        case TT_NUMBER: ent = ENT_NUMBER; break;
        case TT_STRING: ent = ENT_IDENTIFIER; break;
        default: return NULL;
    }
    en_str_value* sv = (en_str_value*)alloc_perm(
        p, sizeof(en_str_value)
    );
    if(!sv) return NULL;
    sv->en.type = ent;
    sv->value = alloc_string_stage(p, t->str);
    if(!sv->value)return NULL;
    return (expr_node*)sv;
}
expr_parse_error parse_expression_node_list(
    parser* p, void** tgt, ureg struct_size,
    ureg expr_node_list_offset, token_type expected_trailer
){
    token* t;
    void** list_start = list_builder_start(&p->lb);
    while(true){
        expr_node* en;
        expr_parse_error epe = parse_expression(p, &en);
        if(epe != EPE_OK){
            if(epe != EPE_EOEX)return epe;
            t = tk_peek(&p->tk);
            if(!t)return EPE_TK_ERROR;
            break;
        }
        int r = list_builder_add(&p->lb, (void*)en);
        if(r) return EPE_INSANE;
        t = tk_peek(&p->tk);
        if(!t)return EPE_TK_ERROR;
        //the way this is programmed right now
        //we allow trailing commas... so be it \(*.*)/
        if(t->type == TT_COMMA) tk_void(&p->tk);
        else break;
    }
    if(t->type != expected_trailer) return EPE_MISSMATCH;
    void** end = list_builder_pop_list(
        &p->lb, list_start, (void***)tgt, &p->tk.tc->permmem,
        struct_size, 0
    );
    if(!end)return EPE_INSANE;
    expr_node_list* nl = ptradd(*tgt, expr_node_list_offset);
    nl->end = (expr_node**)end;
    return EPE_OK;
}
static inline expr_parse_error parse_tuple(
    parser* p, token* t, expr_node** en, ureg start, ureg* end
){
    tk_void(&p->tk);
    en_tuple* tp;
    expr_parse_error epe = parse_expression_node_list(
        p, (void**)&tp, sizeof(en_tuple),
        offsetof(en_tuple, elements), TT_BRACKET_CLOSE
    );
    //EMSG: suboptimal e.g. for case [,,]
    if(epe == EPE_MISSMATCH){
        t = tk_peek(&p->tk);
        if(!t)return EPE_TK_ERROR;
        error_log_report_error_2_annotations(
            &p->tk.tc->error_log, ES_PARSER, false, 
            "unclosed tuple", p->tk.file,
            t->start, t->end, 
            "reached end of expression due to unexpected token",
            start, start + 1,
            "didn't find a matching bracket for this tuple"   
        );
    }
    if(epe != EPE_OK) return epe;
    t = tk_peek(&p->tk);
    *end = t->end;
    tk_void(&p->tk);
    tp->en.type = ENT_TUPLE;
    *en = (expr_node*)tp;
    return EPE_OK;
}
static inline expr_parse_error parse_array(
    parser* p, token* t, expr_node** en, ureg start, ureg* end
){
    tk_void(&p->tk);
    en_array* arr;
    expr_parse_error epe = parse_expression_node_list(
        p, (void**)&arr, sizeof(en_array),
        offsetof(en_array, elements), TT_BRACKET_CLOSE
    );
    t = tk_peek(&p->tk);
    if(!t)return EPE_TK_ERROR;
    //EMSG: suboptimal e.g. for case {,,}
    if(epe == EPE_MISSMATCH){
        error_log_report_error_2_annotations(
            &p->tk.tc->error_log, ES_PARSER, false, 
            "unclosed array", p->tk.file,
            t->start, t->end, 
            "reached end of expression due to unexpected token",
            start, start + 1,
            "didn't find a matching brace for this array"   
        );
    }
    if(epe != EPE_OK) return epe;
    *end = t->end;
    tk_void(&p->tk);
    arr->en.type = ENT_ARRAY;
    *en = (expr_node*)arr;
    return EPE_OK;
}
static inline expr_parse_error parse_parenthesis_group(
    parser* p, token* t, expr_node** en, ureg start, ureg* end
){
    tk_void(&p->tk);
    expr_parse_error epe = parse_expression_p(
        p, PREC_BASELINE, en, end, false
    );
    if(epe != EPE_OK && epe != EPE_EOEX) return epe;
    t = tk_peek(&p->tk);
    if(!t)return EPE_TK_ERROR;
    if(t->type != TT_PAREN_CLOSE){
        error_log_report_error_2_annotations(
            &p->tk.tc->error_log, ES_PARSER, false, 
            "parenthesis missmatch", p->tk.file,
            t->start, t->end, "reached end of expression",
            start, start + 1,
            "didn't find a match for this parenthesis"   
        );
        return EPE_MISSMATCH;
    }
    if(epe == EPE_EOEX){
        error_log_report_error_2_annotations(
            &p->tk.tc->error_log, ES_PARSER, false, 
            "empty parenthesis pair", p->tk.file,
            t->start, t->end, "found closing parenthesis",
            start, start + 1,
            "expected an evaluable expression"   
        );
        return EPE_HANDLED;
    }
    *end = t->end;
    tk_void(&p->tk);
    return EPE_OK;
}
static inline expr_parse_error parse_prefix_unary_op(
    parser* p, expr_node_type op, expr_node** en, ureg start, ureg* end
){
    token* t;
    tk_void(&p->tk);
    en_op_unary* ou = (en_op_unary*)alloc_perm(
        p, sizeof(en_op_unary)
    );
    if(!ou)return EPE_INSANE;
    ou->en.type = ENT_OP_UNARY;
    ou->en.op_type = op;
    expr_parse_error epe = parse_expression_p(
        p, op_precedence[op] + is_left_associative(op), 
        &ou->child, end, true
    );
    if(epe){
        t = tk_peek(&p->tk);
        if(!t)return EPE_TK_ERROR;
        if(epe == EPE_EOEX){
            error_log_report_error_2_annotations(
                &p->tk.tc->error_log, ES_PARSER, false, 
                "missing operand for unary operator", p->tk.file,
                t->start, t->end,
                "reached end of expression due to unexpected token",
                start, start + strlen(op_to_str(op)),
                "missing operand for this operator"   
            );
        }
        return EPE_MISSMATCH;
    }
    *en = (expr_node*)ou;
    return EPE_OK;
}
static inline expr_parse_error parse_single_value(
    parser* p, token* t, expr_node** en, ureg start, ureg* end
){
    expr_parse_error epe;
    switch(t->type){
        case TT_PAREN_OPEN:{
            epe = parse_parenthesis_group(p, t, en, start, end);
            if(epe) return epe;               
        } break;   
        case TT_BRACKET_OPEN:{
            epe = parse_tuple(p, t, en, start, end);
            if(epe) return epe;
        }break;
        case TT_BRACE_OPEN:{
            epe = parse_array(p, t, en, start, end);
            if(epe) return epe;
        }break;
        case TT_STRING:
        case TT_NUMBER:
        case TT_LITERAL:
        case TT_BINARY_LITERAL:{
            *en = parse_str_value(p, t);
            if(!*en) return EPE_INSANE;
            tk_void(&p->tk);
            *end = t->end;
            break;
        }
        default: return EPE_EOEX;
    }
    return EPE_OK;
}
static inline expr_parse_error parse_call(
    parser* p, token* t, expr_node** en,
    expr_node* lhs, ureg start, ureg* end
){
    ureg t_start = t->start;
    tk_void(&p->tk);
    en_call* call;
    expr_parse_error epe = parse_expression_node_list(
        p, (void**)&call, sizeof(en_call),
        offsetof(en_call, args), TT_PAREN_CLOSE
    );
    t = tk_peek(&p->tk);
    if(!t)return EPE_TK_ERROR;
    //EMSG: suboptimal e.g. for case {,,}
    if(epe == EPE_MISSMATCH){
        error_log_report_error_2_annotations(
            &p->tk.tc->error_log, ES_PARSER, false, 
            "unclosed function call", p->tk.file,
            t->start, t->end, 
            "reached end of expression due to unexpected token",
            t_start, t_start + 1,
            "didn't find a matching parenthesis for this"   
        );
    }
    if(epe != EPE_OK) return epe;
    *end = t->end;
    tk_void(&p->tk);
    call->en.type = ENT_OP_CALL;
    call->en.op_type = OP_CALL;
    call->lhs = lhs;
    *en = (expr_node*)call;
    return EPE_OK;
}
static inline expr_parse_error parse_access(
    parser* p, token* t, expr_node** en,
    expr_node* lhs, ureg start, ureg* end
){
    ureg t_start = t->start;
    tk_void(&p->tk);
    en_access* acc;
    expr_parse_error epe = parse_expression_node_list(
        p, (void**)&acc, sizeof(en_access),
        offsetof(en_access, args), TT_BRACKET_CLOSE
    );
    t = tk_peek(&p->tk);
    if(!t)return EPE_TK_ERROR;
    //EMSG: suboptimal e.g. for case {,,}
    if(epe == EPE_MISSMATCH){
        error_log_report_error_2_annotations(
            &p->tk.tc->error_log, ES_PARSER, false, 
            "unclosed access operator", p->tk.file,
            t->start, t->end, 
            "reached end of expression due to unexpected token",
            t_start, t_start + 1,
            "didn't find a matching bracket for this"   
        );
    }
    if(epe != EPE_OK) return epe;
    *end = t->end;
    tk_void(&p->tk);
    acc->en.type = ENT_OP_ACCESS;
    acc->en.op_type = OP_ACCESS;
    acc->lhs = lhs;
    *en = (expr_node*)acc;
    return EPE_OK;
}
static inline expr_parse_error parse_postfix_unary_op(
    parser* p, token* t, expr_node_type op, expr_node** en,
     expr_node* lhs, ureg start, ureg* end
) {
    if(op == OP_CALL){
        return parse_call(p, t, en, lhs, start, end); 
    }
    else if(op == OP_ACCESS){
        return parse_access(p, t, en, lhs, start, end); 
    }
    else{
        tk_void(&p->tk);
        en_op_unary* ou = (en_op_unary*)alloc_perm(
            p, sizeof(en_op_unary)
        );
        if(!ou)return EPE_INSANE;
        ou->en.type = ENT_OP_UNARY;
        ou->en.op_type = op;
        ou->child = lhs;
        *end = t->end;
        *en = (expr_node*)ou;
        return EPE_OK;
    }
}
static inline expr_parse_error parse_binary_op(
    parser* p, token* t, expr_node_type op, expr_node** en, expr_node* lhs,
    ureg start, ureg* end
) {
    ureg t_start = t->start;
    tk_void(&p->tk);
    en_op_binary* ob = (en_op_binary*)alloc_perm(
        p, sizeof(en_op_binary)
    );
    if(!ob)return EPE_INSANE;
    ob->en.type = ENT_OP_BINARY;
    ob->en.op_type = op;
    expr_parse_error epe = parse_expression_p(
        p, op_precedence[op] + is_left_associative(op), 
        &ob->rhs, end, true
    );
    if(epe){
        if(epe == EPE_EOEX){
            t = tk_peek(&p->tk);
            if(!t)return EPE_TK_ERROR;
            error_log_report_error_2_annotations(
                &p->tk.tc->error_log, ES_PARSER, false, 
                "missing operand for infix operator", p->tk.file,
                t->start, t->end, "reached end of expression",
                t_start, t_start + strlen(op_to_str(op)),
                "missing operand for this operator"   
            );
            return EPE_MISSMATCH;
        }
        return epe;
    }
    ob->lhs = *en;
    *en = (expr_node*)ob;
    return EPE_OK;
}
expr_parse_error parse_expression_p(
    parser* p, ureg prec, expr_node** en, ureg* end, bool fill_src_range
){
    token* t = tk_peek(&p->tk);
    if(!t)return EPE_TK_ERROR;
    *en = NULL;
    ureg start = t->start;
    expr_parse_error epe;
    //parse one prefix op(recursive) or a plain value 
    expr_node_type op = token_to_prefix_unary_op(t);
    if(op != OP_NOOP){
        epe = parse_prefix_unary_op(p, op, en, start, end);
        if(epe)return epe;
    }
    else{
        epe = parse_single_value(p, t, en, start, end);
        if(epe) return epe;
    }
    //parse arbitrarily many postfix operators
    while(true){
        t = tk_peek(&p->tk);
        if(!t)return EPE_TK_ERROR;
        expr_node_type op = token_to_postfix_unary_op(t);
        if(op == OP_NOOP)break;
        if(op_precedence[op] < prec)return EPE_OK;
        (*en)->srange = src_range_pack_lines(
            p->tk.tc, start, *end
        );
        if((*en)->srange == SRC_RANGE_INVALID) return EPE_INSANE;
        epe = parse_postfix_unary_op(p, t, op, en, *en, start, end);
        if(epe) return epe;
    }
    //parse arbitrarily many binary operators
    while(true){
        expr_node_type op = token_to_binary_op(t);
        ureg t_start = t->start;
        if(op == OP_NOOP)break;
        if(op_precedence[op] < prec)return EPE_OK;
        (*en)->srange = src_range_pack_lines(
            p->tk.tc, start, *end
        );
        if((*en)->srange == SRC_RANGE_INVALID) return EPE_INSANE;
        parse_binary_op(p, t, op, en, *en, start, end);
        t = tk_peek(&p->tk);
        if(!t)return EPE_TK_ERROR;
    }

    if(fill_src_range){
        (*en)->srange = src_range_pack_lines(
            p->tk.tc, start, *end
        );
        if((*en)->srange == SRC_RANGE_INVALID)return EPE_INSANE;
    }
    return EPE_OK;
}
expr_parse_error parse_expression(parser* p, expr_node** en){
    ureg end;
    return parse_expression_p(
        p, PREC_BASELINE, en, &end, true
    );
}

int parser_parse_file(parser* p, file* f){
    int r = tk_open_file(&p->tk, f);
    if(r) return r;
    p->root.astn.next = NULL;
    p->curr_parent = &p->root;
    r = parser_search_extend(p);
    expr_node* n;
    while(p->tk.status == TK_STATUS_OK){
        expr_parse_error epe = parse_expression(p, &n);
        if(epe)break;
        print_expr(n);
        putchar('\n');
    }
    tk_close_file(&p->tk);
    return r;
}
