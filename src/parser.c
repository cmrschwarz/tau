#include "parser.h"
#include "tokenizer.h"
#include "error_log.h"
#include "keywords.h"
#include "tauc.h"

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
        e->astn.name = alloc_string_stage(p, t->str);
        if(e->astn.name) return ERR;
        e->body = NULL;
        e->imports = NULL;
        e->next = NULL;
        p->curr_parent->next = e;
        p->curr_parent = e;

        return OK;
    }
}

int parser_parse_file(parser* p, file* f){
    int r = tk_open_file(&p->tk, f);
    if(r) return r;
    p->root.next = NULL;
    p->curr_parent = &p->root;
    r = parser_search_extend(p);
    tk_close_file(&p->tk);
    return r;
}
