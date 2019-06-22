#pragma once
#include "src_map.h"
#include "stmt_flags.h"
#include "symbol_table.h"
#include "utils/c_extensions.h"

typedef enum PACK_ENUM ast_node_kind {
    OSC_MODULE,
    OSC_MODULE_GENERIC,
    OSC_EXTEND,
    OSC_EXTEND_GENERIC,
    OSC_LAST_OSC_ID = OSC_EXTEND_GENERIC,

    SC_STRUCT,
    SC_STRUCT_GENERIC,
    SC_TRAIT,
    SC_TRAIT_GENERIC,
    SC_FUNC,
    SC_FUNC_GENERIC,
    SC_LAST_SC_ID = SC_FUNC_GENERIC,

    SYM_NAMED_USING,
    SYM_VAR,
    SYM_VAR_UNINITIALIZED,
    SYM_PARAM,
    SYM_LAST_SYM_ID = SYM_PARAM,

    STMT_IMPORT,
    STMT_USING,

    STMT_EXPRESSION,
    STMT_COMPOUND_ASSIGN,
    STMT_PP_STMT,
    STMT_LAST_STMT_ID = STMT_PP_STMT,

    EXPR_BLOCK,

    EXPR_RETURN,
    EXPR_CONTINUE,
    EXPR_BREAK,
    EXPR_MATCH,
    EXPR_IF,
    EXPR_WHILE,
    EXPR_DO_WHILE,
    EXPR_DO,
    EXPR_LOOP,
    EXPR_MACRO, // TODO

    EXPR_NUMBER,
    EXPR_STRING_LITERAL,
    EXPR_BINARY_LITERAL,
    EXPR_IDENTIFIER,
    EXPR_VARIABLE,
    EXPR_TYPE,
    EXPR_ARRAY,
    EXPR_TUPLE,
    EXPR_LAMBDA,
    EXPR_TYPE_ARRAY,
    EXPR_TYPE_SLICE,

    EXPR_OP_CALL,
    EXPR_OP_ACCESS,
    EXPR_OP_PARENTHESES,

    EXPR_OP_UNARY,
    EXPR_OP_BINARY,
} ast_node_kind;

typedef enum PACK_ENUM op_type {
    // special ops
    OP_NOOP, // invalid op, used for return values

    OP_CALL,
    OP_ACCESS,
    OP_PARENTHESES,

    // binary ops
    OP_MEMBER_ACCESS,
    OP_CAST,
    OP_ASSIGN,
    OP_ADD,
    OP_ADD_ASSIGN,
    OP_SUB,
    OP_SUB_ASSIGN,
    OP_MUL,
    OP_MUL_ASSIGN,
    OP_DIV,
    OP_DIV_ASSIGN,
    OP_MOD,
    OP_MOD_ASSIGN,
    OP_LSHIFT,
    OP_LSHIFT_ASSIGN,
    OP_RSHIFT,
    OP_RSHIFT_ASSIGN,
    OP_LESS_THAN,
    OP_LESS_THAN_OR_EQUAL,
    OP_GREATER_THAN,
    OP_GREATER_THAN_OR_EQUAL,
    OP_EQUAL,
    OP_UNEQAL,
    OP_AND,
    OP_BITWISE_AND,
    OP_BITWISE_AND_ASSIGN,
    OP_OR,
    OP_BITWISE_OR,
    OP_BITWISE_OR_ASSIGN,
    OP_XOR,
    OP_BITWISE_XOR,
    OP_BITWISE_XOR_ASSIGN,
    OP_BITWISE_NOT_ASSIGN,

    // unary ops
    OP_PP,
    OP_CONST,
    OP_DEREF,
    OP_POINTER_OF,
    OP_REF_OF,
    OP_RREF_OF,
    OP_CLOSURE_BY_VALUE,

    OP_NOT,
    OP_BITWISE_NOT,
    OP_UNARY_PLUS,
    OP_UNARY_MINUS,
    OP_PRE_INCREMENT,
    OP_PRE_DECREMENT,
    OP_POST_INCREMENT,
    OP_POST_DECREMENT,
} op_type;

typedef ast_node_kind ast_node;

typedef struct file_require {
    src_file* file;
    src_range srange;
} file_require;

typedef struct expr {
    ast_node_kind kind;
    op_type op_type;
    src_range srange;
} expr;

typedef struct stmt {
    ast_node_kind kind;
    stmt_flags flags;
    src_range srange;
    struct stmt* next;
} stmt;

typedef struct symbol {
    stmt stmt;
    char* name;
    // always points to the parent symbol, even if in a nested expr block
    symbol* parent;
} symbol;

typedef struct body {
    stmt* children;
    symbol_table* symtab;
    src_range srange;
} body;

typedef struct scope {
    symbol symbol;
    body body;
} scope;

typedef struct open_scope {
    scope scope;
    file_require* requires;
} open_scope;

typedef struct sym_named_using {
    symbol symbol;
    expr* target;
} sym_named_using;

typedef struct stmt_using {
    stmt stmt;
    expr* target;
} stmt_using;

typedef struct symbol_import {
    // always non NULL, so list of these can be terminated by a NULL
    char* symbol_name;
    char* alias;
    src_range srange;
} symbol_import;

typedef struct stmt_import stmt_import;
typedef struct mdg_node mdg_node;
typedef struct module_import {
    // always non NULL, so list of these can be terminated by a NULL
    stmt_import* statement;
    mdg_node* tgt; // can't be first, since possibly NULL
    char* name;
    struct module_import* nested_imports;
    symbol_import* selected_symbols; // points to a zero if .*
    src_range srange;
} module_import;

typedef struct expr_named {
    expr expr;
    char* name;
} expr_named;

typedef struct stmt_pp_stmt {
    stmt stmt;
    stmt* pp_stmt;
} stmt_pp_stmt;

typedef struct stmt_import {
    stmt stmt;
    module_import module_import;
} stmt_import;

typedef struct expr_return {
    expr expr;
    expr* value;
} expr_return;

typedef struct expr_break {
    expr expr;
    union {
        expr_named* expr;
        const char* name;
    } target;
    expr* value;
} expr_break;

typedef struct expr_continue {
    expr expr;
    union {
        expr_named* expr;
        const char* name;
    } target;
} expr_continue;

typedef struct expr_block {
    expr_named expr_named;
    body body;
} expr_block;

typedef struct expr_if {
    expr expr;
    expr* condition;
    expr* if_body;
    expr* else_body;
} expr_if;

typedef struct expr_loop {
    expr_named expr_named;
    body body;
} expr_loop;

typedef struct expr_do {
    expr_named expr_named;
    expr* expr_body;
} expr_do;

typedef struct expr_do_while {
    expr_named expr_named;
    expr* condition;
    body do_body;
    body finally_body;
} expr_do_while;

typedef struct expr_while {
    expr_named expr_named;
    expr* condition;
    body while_body;
    body finally_body;
    char* finally_name;
} expr_while;

typedef struct expr_macro {
    expr expr;
    expr** args;
    body body;
    struct expr_macro* next;
} expr_macro;

typedef struct expr_pp {
    expr expr;
    expr* child;
} expr_pp;

typedef struct match_arm {
    expr* condition;
    expr* value;
} match_arm;

typedef struct expr_match {
    expr_named expr_named;
    expr* match_expr;
    match_arm** match_arms;
    ureg body_end;
} expr_match;

typedef struct stmt_expr {
    stmt stmt;
    expr* expr;
} stmt_expr;

typedef struct sym_param {
    symbol symbol;
    expr* type;
    expr* default_value;
} sym_param;

typedef struct sc_func {
    scope scope;
    sym_param* params;
} sc_func;

typedef struct sc_func_generic {
    scope scope;
    sym_param* generic_params;
    sym_param* params;
} sc_func_generic;

typedef struct sc_struct {
    scope scope;
} sc_struct;

typedef struct sc_struct_generic {
    scope scope;
    sym_param* generic_params;
} sc_struct_generic;

typedef struct sc_trait {
    scope scope;
} sc_trait;

typedef struct sc_trait_generic {
    scope scope;
    sym_param* generic_params;
} sc_trait_generic;

typedef struct osc_module {
    open_scope oscope;
} osc_module;

typedef struct osc_module_generic {
    open_scope oscope;
    sym_param* generic_params;
} osc_module_generic;

typedef struct osc_extend {
    open_scope oscope;
} osc_extend;

typedef struct osc_extend_generic {
    open_scope oscope;
    sym_param* generic_params;
} osc_extend_generic;

typedef struct sym_var {
    symbol symbol;
    expr* type;
    expr* value;
} sym_var;

typedef struct sym_var_uninitialized {
    symbol symbol;
    expr* type;
} sym_var_uninitialized;

typedef struct stmt_compound_assignment {
    stmt stmt;
    expr** elements;
    expr* value;
} stmt_compound_assignment;

typedef struct expr_parentheses {
    expr expr;
    expr* child;
} expr_parentheses;

typedef struct expr_op_binary {
    expr expr;
    expr* lhs;
    expr* rhs;
} expr_op_binary;

typedef struct expr_op_unary {
    expr expr;
    expr* child;
} expr_op_unary;

// TODO: implement named arguments
typedef struct expr_call {
    expr expr;
    expr* lhs;
    expr** args;
} expr_call;

typedef struct expr_access {
    expr expr;
    expr* lhs;
    expr** args;
} expr_access;

typedef struct expr_str_value {
    expr expr;
    char* value;
} expr_str_value;

typedef expr_str_value expr_number;
typedef expr_str_value expr_identifier;
typedef expr_str_value expr_string_literal;
typedef expr_str_value expr_binary_literal;

typedef struct expr_cast {
    expr expr;
    expr* value;
    expr* target_type;
} expr_cast;

typedef struct expr_scope_access {
    expr expr;
    expr* lhs;
    expr* rhs;
} expr_scope_access;

typedef struct expr_member_access {
    expr expr;
    expr* lhs;
    expr* rhs;
} expr_member_access;

typedef struct expr_tuple {
    expr expr;
    expr** elements;
} expr_tuple;

typedef struct expr_array {
    expr expr;
    expr** elements;
} expr_array;

typedef struct expr_type_array {
    expr expr;
    expr* inside;
    expr* rhs;
} expr_type_array;

typedef struct expr_type_slice {
    expr expr;
    expr* rhs;
} expr_type_slice;

typedef struct expr_lambda {
    expr expr;
    sym_param* params;
    body body;
} expr_lambda;

src_range ast_node_get_src_range(ast_node* s);
bool ast_node_is_open_scope(ast_node* s);
bool ast_node_is_scope(ast_node* s);
bool ast_node_is_symbol(ast_node* s);
bool ast_node_is_expr(ast_node* s);
bool ast_node_is_stmt(ast_node* s);
src_file* open_scope_get_file(open_scope* s);
src_file* stmt_get_file(stmt* s);

bool body_is_braced(body* b);

bool is_unary_op_postfix(op_type t);
stmt* get_parent_body(scope* parent);
void stmt_get_highlight_bounds(stmt* stmt, ureg* start, ureg* end);
void get_expr_bounds(expr* n, ureg* start, ureg* end);
