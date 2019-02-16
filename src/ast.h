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

typedef enum PACK_ENUM astnt {
    // statement nodes
    ASTNT_MODULE,
    ASTNT_GENERIC_MODULE,
    ASTNT_EXTEND,
    ASTNT_GENERIC_EXTEND,

    ASTNT_FUNCTION,
    ASTNT_GENERIC_FUNCTION,
    ASTNT_STRUCT,
    ASTNT_GENERIC_STRUCT,
    ASTNT_TRAIT,
    ASTNT_GENERIC_TRAIT,

    ASTNT_VAR_DECL,
    ASTNT_PARAM_DECL,

    ASTNT_CONTINUE,
    ASTNT_BREAK,

    ASTNT_RETURN,
    ASTNT_GIVE,

    ASTNT_LABEL,
    ASTNT_GOTO,

    ASTNT_EXPRESSION,

    // expression / statement hybrids
    ENT_LABEL,
    ASTNT_SWITCH,
    ASTNT_IF,
    ASTNT_IF_LET,
    ASTNT_FOR,
    ASTNT_FOR_EACH,
    ASTNT_WHILE,
    ASTNT_DO_WHILE,
    ASTNT_LOOP,

    // expression nodes
    ENT_NUMBER,
    ENT_STRING_LITERAL,
    ENT_BINARY_LITERAL,
    ENT_IDENTIFIER,
    ENT_ARRAY,
    ENT_TUPLE,
    ENT_LAMBDA,
    ENT_TYPE_ARRAY,
    ENT_TYPE_SLICE,

    ENT_OP_UNARY,
    ENT_OP_BINARY,

    ENT_OP_CALL,
    ENT_OP_ACCESS,
    ENT_OP_PARENTHESES,
} astnt;

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

typedef astnt astn;
typedef struct stmt {
    astnt type;
    stmt_flags flags;
    struct stmt* next;
} stmt;

typedef struct named_stmt {
    stmt stmt;
    char* name;
    struct named_stmt* parent;
    src_range decl_range;
} named_stmt;

typedef struct expr {
    astnt type;
    op_type op_type;
    src_range srange;
} expr;

typedef struct stmt_label {
    named_stmt nstmt;
} expr_label;

// these hybrid expressions actually "inherit" named statement to allow it to be
// in the IHT this is the reason why exprs dont't have astn* but astn* we use
// the next pointer from stmt to store the child expression

typedef struct expr_if {
    named_stmt nstmt; // next becomes the if body
    astn* condition;
    stmt* else_body;
} expr_if;

typedef struct expr_loop {
    named_stmt nstmt; // next becomes the loop body
    ureg block_end;
} expr_loop;

typedef struct stmt_expr {
    stmt stmt;
    astn* expr;
    src_range stmt_range;
} stmt_expr;

typedef struct stmt_param_decl {
    named_stmt nstmt;
    astn* type;
    astn* default_value;
} stmt_param_decl;

typedef struct stmt_function {
    named_stmt nstmt;
    stmt_param_decl* params;
    stmt* body;
    ureg body_end;
} stmt_function;

typedef struct stmt_generic_function {
    named_stmt nstmt;
    stmt_param_decl* generic_params;
    stmt_param_decl* params;
    stmt* body;
    ureg body_end;
} stmt_generic_function;

typedef struct stmt_struct {
    named_stmt nstmt;
    stmt* body;
    ureg body_end;
} stmt_struct;

typedef struct stmt_generic_struct {
    named_stmt nstmt;
    stmt_param_decl* generic_params;
    stmt* body;
    ureg body_end;
} stmt_generic_struct;

typedef struct stmt_trait {
    named_stmt nstmt;
    stmt* body;
    ureg body_end;
} stmt_trait;

typedef struct stmt_generic_trait {
    named_stmt nstmt;
    stmt_param_decl* generic_params;
    stmt* body;
    ureg body_end;
} stmt_generic_trait;

typedef struct stmt_module {
    named_stmt nstmt;
    stmt* body;
    ureg body_end;
} stmt_module;

typedef struct stmt_generic_module {
    named_stmt nstmt;
    stmt_param_decl* generic_params;
    stmt* body;
    ureg body_end;
} stmt_generic_module;

typedef struct stmt_extend {
    named_stmt nstmt;
    stmt* body;
    ureg body_end;
} stmt_extend;

typedef struct stmt_generic_extend {
    named_stmt nstmt;
    stmt_param_decl* generic_params;
    stmt* body;
    ureg body_end;
} stmt_generic_extend;

typedef struct stmt_var_decl {
    named_stmt nstmt;
    astn* type;
    astn* value;
} stmt_var_decl;

typedef struct expr_parentheses {
    expr ex;
    astn* child;
} expr_parentheses;

typedef struct expr_list {
    astn*** end_ptr;
} expr_list;

typedef struct expr_op_binary {
    expr ex;
    astn* lhs;
    astn* rhs;
} expr_op_binary;

typedef struct expr_op_unary {
    expr ex;
    astn* child;
} expr_op_unary;

// TODO: implement named arguments
typedef struct expr_call {
    expr ex;
    astn* lhs;
    expr_list args;
} expr_call;

// PERF: maybe provide an optimized variant for one arg
typedef struct expr_access {
    expr ex;
    astn* lhs;
    expr_list args;
} expr_access;

typedef struct expr_str_value {
    expr ex;
    char* value;
} expr_str_value;
typedef expr_str_value expr_number;
typedef expr_str_value expr_identifier;
typedef expr_str_value expr_string_literal;
typedef expr_str_value expr_binary_literal;

typedef struct expr_cast {
    expr ex;
    astn* value;
    astn* target_type;
} expr_cast;

typedef struct expr_scope_access {
    expr ex;
    astn* lhs;
    astn* rhs;
} expr_scope_access;

typedef struct expr_member_access {
    expr ex;
    astn* lhs;
    astn* rhs;
} expr_member_access;

typedef struct expr_tuple {
    expr ex;
    expr_list elements;
} expr_tuple;

typedef struct expr_array {
    expr ex;
    expr_list elements;
} expr_array;

typedef struct expr_type_array {
    expr ex;
    astn* inside;
    astn* rhs;
} expr_type_array;

typedef struct expr_type_slice {
    expr ex;
    astn* rhs;
} expr_type_slice;

typedef struct expr_lambda {
    expr ex;
    expr_list params;
    stmt* body;
} expr_lambda;

bool is_unary_op_postfix(op_type t);
stmt* get_parent_body(named_stmt* parent);
void stmt_get_highlight_bounds(stmt* stmt, ureg* start, ureg* end);
void get_expr_bounds(astn* n, ureg* start, ureg* end);

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