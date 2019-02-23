#pragma once
#include "src_map.h"
#include "utils/c_extensions.h"

typedef u8 stmt_flags;
#define ASTN_FLAGS_DEFAULT 0

typedef enum PACK_ENUM access_modifier {
    AM_UNSPECIFIED = 0,
    AM_PRIVATE = 1,
    AM_PROTECTED = 2,
    AM_PUBLIC = 3,
} access_modifier;

typedef enum PACK_ENUM ast_node_type {
    SCS_MODULE,
    SCS_MODULE_GENERIC,

    SCF_EXTEND,
    SCF_EXTEND_GENERIC,
    SCF_FUNC,
    SCF_FUNC_GENERIC,

    SC_STRUCT,
    SC_STRUCT_GENERIC,
    SC_TRAIT,
    SC_TRAIT_GENERIC,

    SYM_ALIAS,
    SYM_VAR_DECL,
    SYM_PARAM_DECL,
    SYM_LABEL,

    STMT_EXPRESSION,

    EXPR_BLOCK,

    EXPR_GOTO,
    EXPR_GIVE,
    EXPR_RETURN,
    EXPR_CONTINUE,
    EXPR_BREAK,

    EXPR_SWITCH,
    EXPR_IF,
    EXPR_FOR,
    EXPR_FOR_EACH,
    EXPR_WHILE,
    EXPR_DO_WHILE,
    EXPR_LOOP,

    EXPR_NUMBER,
    EXPR_STRING_LITERAL,
    EXPR_BINARY_LITERAL,
    EXPR_IDENTIFIER,
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
} ast_node_type;

typedef enum PACK_ENUM op_type {
    // special ops
    OP_NOOP, // invalid op, used for return values

    OP_CALL,
    OP_ACCESS,
    OP_PARENTHESES,

    // binary ops
    OP_MEMBER_ACCESS,
    OP_SCOPE_ACCESS,
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

typedef ast_node_type ast_node;

typedef struct expr {
    ast_node_type type;
    op_type op_type;
    src_range srange;
} expr;

typedef struct stmt {
    ast_node_type type;
    stmt_flags flags;
    struct stmt* next;
} stmt;

typedef struct symbol {
    stmt stmt;
    char* name;
    src_range decl_range;
} symbol;

typedef struct body {
    stmt* children;
    ureg body_end;
} body;

typedef struct scope {
    symbol symbol;
    body body;
} scope;

typedef struct scope_full {
    scope scope;
    scope* parent;
    ast_node** imports;
    ast_node** includes;
} scope_full;

typedef struct scope_sealed {
    scope scope;
    ast_node** imports;
    ast_node** includes;
} scope_sealed;

typedef struct expr_named {
    expr expr;
    char* name;
} expr_named;

typedef struct stmt_label {
    symbol symbol;
} stmt_label;

typedef struct stmt_alias {
    symbol symbol;
    expr* target;
} stmt_alias;

typedef struct expr_return {
    expr expr;
    expr* value;
} expr_return;

typedef struct expr_give {
    expr expr;
    union {
        expr* expr;
        char* name;
    } target;
    expr* value;
} expr_give;

typedef struct expr_goto {
    expr expr;
    union {
        stmt_label* label;
        char* name;
    } target;
} expr_goto;

typedef struct expr_block {
    expr expr;
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
    expr* body;
} expr_loop;

typedef struct expr_while {
    expr_named expr_named;
    expr* condition;
    expr* while_body;
    expr* finally_body;
} expr_while;

typedef struct expr_for_in {
    expr_named expr_named;
    stmt* decl;
    expr* range;
    expr* for_body;
    expr* finally_body;
} expr_for_in;

typedef struct expr_for {
    expr_named expr_named;
    stmt* decl;
    expr* condition;
    stmt* iteration;
    expr* for_body;
    expr* finally_body;
} expr_for;

typedef struct match_arm {
    struct match_arm* next;
    expr* condition; // debatable
    expr* body;
} match_arm;

typedef struct expr_match {
    expr_named expr_named;
    match_arm* match_arms;
    ureg body_end;
} expr_match;

typedef struct stmt_expr {
    stmt stmt;
    expr* expr;
    src_range expr_range; // debatable
} stmt_expr;

typedef struct stmt_param_decl {
    symbol symbol;
    expr* type;
    expr* default_value;
} stmt_param_decl;

typedef struct stmt_func {
    scope_full scope_full;
    stmt_param_decl* params;
} stmt_func;

typedef struct stmt_func_generic {
    scope_full scope_full;
    stmt_param_decl* generic_params;
    stmt_param_decl* params;
} stmt_func_generic;

typedef struct stmt_struct {
    scope scope;
} stmt_struct;

typedef struct stmt_struct_generic {
    scope scope;
    stmt_param_decl* generic_params;
} stmt_struct_generic;

typedef struct stmt_trait {
    scope scope;
} stmt_trait;

typedef struct stmt_trait_generic {
    scope scope;
    stmt_param_decl* generic_params;
} stmt_trait_generic;

typedef struct stmt_module {
    scope_sealed scope_sealed;
} stmt_module;

typedef struct stmt_module_generic {
    scope_sealed scope_sealed;
    stmt_param_decl* generic_params;
} stmt_module_generic;

typedef struct stmt_extend {
    scope_full scope_full;
} stmt_extend;

typedef struct stmt_extend_generic {
    scope_full scope_full;
    stmt_param_decl* generic_params;
} stmt_extend_generic;

typedef struct stmt_var_decl {
    symbol symbol;
    expr* type;
    expr* value;
} stmt_var_decl;

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
    stmt_param_decl* params;
    body body;
} expr_lambda;

bool is_unary_op_postfix(op_type t);
stmt* get_parent_body(scope* parent);
void stmt_get_highlight_bounds(stmt* stmt, ureg* start, ureg* end);
void get_expr_bounds(expr* n, ureg* start, ureg* end);

void stmt_flags_set_access_mod(stmt_flags* f, access_modifier m);
access_modifier stmt_flags_get_access_mod(stmt_flags f);

void stmt_flags_set_const(stmt_flags* f, bool cnst);
bool stmt_flags_get_const(stmt_flags f);

void stmt_flags_set_sealed(stmt_flags* f, bool sld);
bool stmt_flags_get_sealed(stmt_flags f);

void stmt_flags_set_virtual(stmt_flags* f, bool virt);
bool stmt_flags_get_virtual(stmt_flags f);

void stmt_flags_set_static(stmt_flags* f, bool stat);
bool stmt_flags_get_static(stmt_flags f);

void stmt_flags_set_module_extension(stmt_flags* f, bool ext);
bool stmt_flags_get_module_extension(stmt_flags f);