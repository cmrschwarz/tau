#include "parser.h"
#include "tokenizer.h"
#include "error_log.h"
#include "keywords.h"
#include "tauc.h"
#include "print_ast.h"
#include "utils/math_utils.h"

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
expr_parsing_error parse_expression_node_list(
    parser* p, void** tgt, ureg struct_size, token_type expected_trailer
){
    token* t;
    void** list_start = list_builder_start(&p->lb);
    while(true){
        expr_node* en;
        expr_parsing_error epe = parse_expression(p, &en);
        if(epe != EPE_OK){
            if(epe != EPE_EOEX)return epe;
            t = tk_peek(&p->tk);
            break;
        }
        int r = list_builder_add(&p->lb, (void*)en);
        if(r) return EPE_INSANE;
        t = tk_peek(&p->tk);
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
    expr_node_list* nl = ptradd(*tgt, struct_size - sizeof(expr_node_list));
    nl->end = (expr_node**)end;
    return EPE_OK;
}
expr_parsing_error parse_expression_p(
    parser* p, ureg prec, expr_node** en, ureg* end, bool fill_src_range
){
    token* t = tk_peek(&p->tk);
    *en = NULL;
    ureg lhs_start = t->start;
    expr_parsing_error epe;
    int r;
    //parse prefix unary ops
   
    expr_node_type op = token_to_prefix_unary_op(t);
    if(op != OP_NOOP){
        tk_void(&p->tk);
        en_op_unary* ou = (en_op_unary*)alloc_perm(
            p, sizeof(en_op_unary)
        );
        if(!ou)return EPE_INSANE;
        ou->en.type = ENT_OP_UNARY;
        ou->en.op_type = op;
        epe = parse_expression_p(
            p, op_precedence[op] + is_left_associative(op), 
            &ou->child, end, true
        );
        if(epe){
            t = tk_peek(&p->tk);
            if(epe == EPE_EOEX){
                error_log_report_error_2_annotations(
                    &p->tk.tc->error_log, ES_PARSER, false, 
                    "missing operand for unary operator", p->tk.file,
                    t->start, t->end, "reached end of expression",
                    lhs_start, lhs_start + strlen(op_to_str(op)),
                    "missing operand for this operator"   
                );
            }
            return EPE_MISSMATCH;
        }
        *en = (expr_node*)ou;
        t = tk_peek(&p->tk);
    }
    //parse value
    if(*en == NULL){
        switch(t->type){
            case TT_PAREN_OPEN:{
                tk_void(&p->tk);
                epe = parse_expression_p(
                    p, PREC_BASELINE, en, end, false
                );
                if(epe != EPE_OK && epe != EPE_EOEX) return epe;
                t = tk_peek(&p->tk);
                if(t->type != TT_PAREN_CLOSE){
                    error_log_report_error_2_annotations(
                        &p->tk.tc->error_log, ES_PARSER, false, 
                        "parenthesis missmatch", p->tk.file,
                        t->start, t->end, "reached end of expression",
                        lhs_start, lhs_start + 1,
                        "didn't find a match for this parenthesis"   
                    );
                    return EPE_MISSMATCH;
                }
                if(epe == EPE_EOEX){
                    error_log_report_error_2_annotations(
                        &p->tk.tc->error_log, ES_PARSER, false, 
                        "empty parenthesis pair", p->tk.file,
                        t->start, t->end, "found closing parenthesis",
                        lhs_start, lhs_start + 1,
                        "expected an evaluable expression"   
                    );
                    return EPE_HANDLED;
                }
                tk_void(&p->tk);
                *end = t->end;
                break;   
            }
            case TT_BRACKET_OPEN:
            case TT_BRACE_OPEN:{
                bool is_tuple = (t->type == TT_BRACKET_OPEN); 
                tk_void(&p->tk);
                en_value_group* vg;
                epe = parse_expression_node_list(
                    p, (void**)&vg, sizeof(en_value_group),
                    is_tuple ? TT_BRACKET_CLOSE : TT_BRACE_CLOSE
                );
                t = tk_peek(&p->tk);
                //ERROR MESSAGES: in many cases (like[,,]) the provided
                //error message is far from ideal
                if(epe == EPE_MISSMATCH){
                    if(is_tuple){
                        error_log_report_error_2_annotations(
                            &p->tk.tc->error_log, ES_PARSER, false, 
                            "unclosed tuple", p->tk.file,
                            t->start, t->end, 
                            "reached end of expression due to unexpected token",
                            lhs_start, lhs_start + 1,
                            "didn't find a matching bracket for this tuple"   
                        );
                    }
                    else{
                        error_log_report_error_2_annotations(
                            &p->tk.tc->error_log, ES_PARSER, false, 
                            "unclosed array", p->tk.file,
                            t->start, t->end, 
                            "reached end of expression due to unexpected token",
                            lhs_start, lhs_start + 1,
                            "didn't find a matching brace for this array"   
                        );
                    }
                    return EPE_MISSMATCH;
                }
                *end = t->end;
                tk_void(&p->tk);
                vg->en.type = is_tuple ? ENT_TUPLE : ENT_ARRAY;
                vg->en.srange = src_range_pack_lines(
                    p->tk.tc, lhs_start, *end
                );
                *en = (expr_node*)vg;
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
            default: 
                return EPE_EOEX;
        }
        t = tk_peek(&p->tk);
    }
    //parse postfix unary ops
    while(true){
        expr_node_type op = token_to_postfix_unary_op(t);
        if(op == OP_NOOP)break;
        if(op_precedence[op] < prec)return EPE_OK;
        (*en)->srange = src_range_pack_lines(
            p->tk.tc, lhs_start, *end
        );
        if((*en)->srange == SRC_RANGE_INVALID) return EPE_INSANE;
        tk_void(&p->tk);
        en_op_unary* ou = (en_op_unary*)alloc_perm(
            p, sizeof(en_op_unary)
        );
        if(!ou)return EPE_INSANE;
        ou->en.type = ENT_OP_UNARY;
        ou->en.op_type = op;
        *end = t->end;
        *en = (expr_node*)ou;
        t = tk_peek(&p->tk);
        op = token_to_postfix_unary_op(t);
    }
    //parse binary ops
    while(true){
        expr_node_type op = token_to_binary_op(t);
        ureg t_start = t->start;
        if(op == OP_NOOP)break;
        if(op_precedence[op] < prec)return EPE_OK;
        (*en)->srange = src_range_pack_lines(
            p->tk.tc, lhs_start, *end
        );
        if((*en)->srange == SRC_RANGE_INVALID) return EPE_INSANE;
        tk_void(&p->tk);
        en_op_binary* ob = (en_op_binary*)alloc_perm(
            p, sizeof(en_op_binary)
        );
        if(!ob)return EPE_INSANE;
        ob->en.type = ENT_OP_BINARY;
        ob->en.op_type = op;
        epe = parse_expression_p(
            p, op_precedence[op] + is_left_associative(op), 
            &ob->rhs, end, true
        );
        if(epe){
            if(epe == EPE_EOEX){
                t = tk_peek(&p->tk);
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
        t = tk_peek(&p->tk);
    }
    if(fill_src_range){
        (*en)->srange = src_range_pack_lines(
            p->tk.tc, lhs_start, *end
        );
        if((*en)->srange == SRC_RANGE_INVALID)return EPE_INSANE;
    }
    return EPE_OK;
}
expr_parsing_error parse_expression(parser* p, expr_node** en){
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
        expr_parsing_error epe = parse_expression(p, &n);
        if(epe)break;
        print_expr(n);
        putchar('\n');
    }
    tk_close_file(&p->tk);
    return r;
}
