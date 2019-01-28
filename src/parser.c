#include "parser.h"
#include "tokenizer.h"
#include "error_log.h"
#include "keywords.h"
#include "tauc.h"

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
    [OP_VAL_OF] = 14,
    [OP_TYPE_MODIFIER] = 14,
    
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

};
#define PREC_BASELINE 0

static inline bool is_right_associative(expr_node_type t){
    switch(t){
        case OP_ASSIGN:
        case OP_ADD_ASSIGN:
        case OP_SUB_ASSIGN:
        case OP_MUL_ASSIGN:
        case OP_DIV_ASSIGN:
        case OP_MOD_ASSIGN:
        case OP_LSHIFT_ASSIGN:
        case OP_RSHIFT_ASSIGN:
            return true;
        default: return false;
    }
}

static inline expr_node_type token_to_binary_op(token_type t){
    switch(t){
        case TT_PLUS: return OP_ADD;
        case TT_MINUS: return OP_SUB;
        case TT_STAR: return OP_MUL;
        case TT_DOT: return OP_MEMBER_ACCESS;
        case TT_DOUBLE_COLON: return OP_SCOPE_ACCESS;
        case TT_AND: return OP_BITWISE_AND;
        case TT_DOUBLE_AND: return OP_AND;
        case TT_CARET: return OP_BITWISE_XOR;
        case TT_DOUBLE_CARET: return OP_XOR;
        case TT_PIPE: return OP_BITWISE_OR;
        case TT_DOUBLE_PIPE: return OP_OR;
        //TODO: ...
        default: return OP_NOOP;
    }
}
static inline expr_node_type token_to_prefix_unary_op(token_type t){
    switch (t){
        case TT_MINUS: return OP_UNARY_MINUS;
        case TT_PLUS: return OP_UNARY_PLUS;
        //TODO: ...
        default: return OP_NOOP;
    }
}
static inline expr_node_type token_to_prefix_postfix_op(token_type t){
    switch (t){
        case TT_DOUBLE_PLUS: return OP_POST_INCREMENT;
        case TT_DOUBLE_MINUS: return OP_POST_DECREMENT;
        //TODO: ...
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
static inline void* alloc_string_stage(parser* p, string s){
   return alloc_string_ppool(p, s, &p->tk.tc->stagemem);
}
static inline void* alloc_string_perm(parser* p, string s){
    return alloc_string_ppool(p, s, &p->tk.tc->permmem);
}

static inline int parser_error_1a(parser* p, char* msg, ureg start, ureg end, char* annot){
    error_log_report_error_1_annotation(
        &p->tk.tc->error_log, ES_PARSER, false,
        msg, p->tk.file, 
        start, end, annot
    );
    return ERR;
}
static inline int parser_error_2a(parser* p, char* msg, ureg start, ureg end, char* annot, ureg start2, ureg end2, char* annot2){
    error_log_report_error_2_annotations(
        &p->tk.tc->error_log, ES_PARSER, false,
        msg, p->tk.file, 
        start, end, annot,
        start2, end2, annot2
    );
    return ERR;
}
static inline int parser_error_1at(parser* p, char* msg, token* t, char* annot){
    return parser_error_1a(p, msg, t->start, t->end, annot);
}
static inline token* parser_expect(parser* p, token_type tt, char* msg, char* ctx, ureg ctx_start, ureg ctx_end){
    token* t = tk_consume(&p->tk);
    if(t->type != tt){
        char* expstr = "expected ";
        ureg explen = strlen(expstr);
        ureg toklen = strlen(token_strings[tt]);
        char* ann = (char*)error_log_alloc(&p->tk.tc->error_log, explen + toklen + 3);
        if(!ann)return NULL;
        memcpy(ann, expstr, explen);
        ann[explen] = '\'';
        memcpy(ann + explen + 1, token_strings[tt], toklen);
        ann[explen + 1 + toklen] = '\'';
        ann[explen + 1 + toklen + 1] = '\0';
        parser_error_2a(p, msg, t->start, t->end, ann, ctx_start, ctx_end, ctx);
        return NULL;
    }
    return t;
}
int parser_init(parser* p, thread_context* tc){
    int r = tk_init(&p->tk, tc);
    if(r) return r;
    p->root.name = "void";
    p->root.parent = NULL;
    p->root.type = ASTNT_MODULE;
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
            "statement start", estart, estart + strlen(keyword_strings[KW_EXTEND])
        );
        if(!t)return ERR;
        astn_extend* e = (astn_extend*)alloc_stage(p, sizeof(astn_extend));
        if(!e)return ERR;
        e->astn.type = ASTNT_EXTEND;
        e->astn.parent = &p->root;
        e->astn.next = NULL;
        e->astn.name = (char*)alloc_string_stage(p, t->str);
        if(e->astn.name) return ERR;
        e->body = NULL;
        e->imports = NULL;
        p->curr_parent->next = (ast_node*)e;
        p->curr_parent = (named_ast_node*)e;

        return OK;
    }
}
expr_node* parse_expression_above_prec(parser* p, ureg prec){
    token* t1 = tk_peek(&p->tk);
    switch(t1->type){
        case TT_AND
    }
}
expr_node* parse_expression(parser* p){
    return parse_expression_above_prec(p, PREC_BASELINE);
}
int parser_parse_file(parser* p, file* f){
    int r = tk_open_file(&p->tk, f);
    if(r) return r;
    p->root.next = NULL;
    p->curr_parent = &p->root;
    r = parser_search_extend(p);
    while(p->tk.status == TK_STATUS_OK){

    }
    tk_close_file(&p->tk);
    return r;
}
