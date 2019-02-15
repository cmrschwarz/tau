#pragma once
#include "src_map.h"
#include "utils/c_extensions.h"

typedef u8 astn_flags;
#define ASTN_FLAGS_DEFAULT 0

typedef enum PACK_ENUM access_modifier {
    AM_UNSPECIFIED = 0,
    AM_PRIVATE = 1,
    AM_PROTECTED = 2,
    AM_PUBLIC = 3,
} access_modifier;

typedef enum PACK_ENUM ast_node_type {
    ASTNT_MODULE,
    ASTNT_EXTEND,

    ASTNT_FUNCTION,
    ASTNT_GENERIC_FUNCTION,
    ASTNT_STRUCT,
    ASTNT_GENERIC_STRUCT,

    ASTNT_VAR_DECL,
    ASTNT_PARAM_DECL,

    ASTNT_FOR,
    ASTNT_FOR_EACH,
    ASTNT_WHILE,
    ASTNT_DO_WHILE,
    ASTNT_LOOP,

    ASTNT_CONTINUE,
    ASTNT_BREAK,

    ASTNT_RETURN,
    ASTNT_GIVE,

    ASTNT_LABEL,
    ASTNT_GOTO,

    ASTNT_IF,
    ASTNT_SWITCH,
    ASTNT_IF_LET,

    ASTNT_EXPRESSION,
} ast_node_type;

typedef enum PACK_ENUM expr_node_type {
    ENT_NUMBER,
    ENT_STRING_LITERAL,
    ENT_BINARY_LITERAL,
    ENT_IDENTIFIER,
    ENT_ARRAY,
    ENT_TUPLE,
    ENT_LAMBDA,
    ENT_TYPE_ARRAY,
    ENT_TYPE_SLICE,

    ENT_OP_CALL,
    OP_CALL = ENT_OP_CALL,

    ENT_OP_ACCESS,
    OP_ACCESS = ENT_OP_ACCESS,

    ENT_OP_PARENTHESES,
    OP_PARENTHESES = ENT_OP_PARENTHESES,

    ENT_OP_BINARY,
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

    ENT_OP_UNARY,
    OP_CONST,
    OP_DEREF,      // *
    OP_POINTER_OF, // %
    OP_REF_OF,     // &
    OP_RREF_OF,    // $
    OP_VALUE_OF,   // ^ (don't confuse with deref, this is used in lambdas to
                   // indicate closure by value)
    OP_NOT,
    OP_BITWISE_NOT,
    OP_UNARY_PLUS,
    OP_UNARY_MINUS,
    OP_PRE_INCREMENT,
    OP_PRE_DECREMENT,
    OP_POST_INCREMENT,
    OP_POST_DECREMENT,
    OP_NOOP, // invalid op, used for return values
} expr_node_type;

typedef struct expr_node expr_node;
typedef struct astn_module astn_module;

typedef struct import {
    bool resolved;
    union {
        char* module_name;
        astn_module* module;
    } target;
    struct include* next;
} import;

typedef struct ast_node {
    ast_node_type type;
    astn_flags flags;
    struct ast_node* next;
} ast_node;

typedef struct named_ast_node {
    ast_node astn;
    char* name;
    struct named_ast_node* parent;
    src_range_packed decl_range;
} named_ast_node;

typedef struct astn_expr {
    ast_node astn;
    expr_node* expr;
} astn_expr;

typedef struct astn_param_decl {
    named_ast_node nastn;
    expr_node* type;
    expr_node* default_value;
} astn_param_decl;

typedef struct astn_module {
    named_ast_node nastn;
    import* imports;
    ast_node* body;
} astn_module;

typedef struct astn_extend {
    named_ast_node nastn;
    import* imports;
    ast_node* body;
} astn_extend;

// TODO: implement named arguments
typedef struct astn_named_argument {
    struct astn_named_argument* next;
    char* name;
    astn_expr* value;
} astn_named_argument;

typedef struct astn_function {
    named_ast_node nastn;
    astn_param_decl* params;
    ast_node* body;
} astn_function;

typedef struct astn_generic_function {
    named_ast_node nastn;
    astn_param_decl* generic_parameters;
    astn_param_decl* parameters;
} astn_generic_function;

typedef struct astn_struct {
    named_ast_node nastn;
    ast_node* body;
} astn_struct;

typedef struct astn_generic_struct {
    named_ast_node nastn;
    astn_param_decl* generic_parameters;
    ast_node* body;
} astn_generic_struct;

typedef struct astn_var_decl {
    named_ast_node nastn;
    expr_node* type;
    expr_node* value;
} astn_var_decl;

typedef struct astn_if {
    ast_node astn;
    expr_node* condition;
    ast_node* then_body;
    ast_node* else_body;
} astn_if;

typedef struct astn_while {
    ast_node astn;
    expr_node* condition;
    ast_node* body;
    ast_node* finally_body;
} astn_while;

typedef struct expr_node {
    expr_node_type type;
    expr_node_type op_type;
    src_range_packed srange;
} expr_node;

typedef struct expr_node_le {
    struct expr_node_le* next;
    expr_node en;
} expr_node_le;

typedef struct en_parentheses {
    expr_node en;
    expr_node* child;
} en_parentheses;

typedef struct expr_node_list {
    expr_node*** end_ptr;
} expr_node_list;

typedef struct en_op_binary {
    expr_node en;
    expr_node* lhs;
    expr_node* rhs;
} en_op_binary;

typedef struct en_op_unary {
    expr_node en;
    expr_node* child;
} en_op_unary;

typedef struct en_call {
    expr_node en;
    expr_node* lhs;
    expr_node_list args;
} en_call;

// PERF: maybe provide an optimized variant for one arg
typedef struct en_access {
    expr_node en;
    expr_node* lhs;
    expr_node_list args;
} en_access;

typedef struct en_str_value {
    expr_node en;
    char* value;
} en_str_value;
typedef en_str_value en_number;
typedef en_str_value en_identifier;
typedef en_str_value en_string_literal;
typedef en_str_value en_binary_literal;

typedef struct en_cast {
    expr_node en;
    expr_node* value;
    expr_node* target_type;
} en_cast;

typedef struct en_scope_access {
    expr_node en;
    expr_node* lhs;
    expr_node* rhs;
} en_scope_access;

typedef struct en_member_access {
    expr_node en;
    expr_node* lhs;
    expr_node* rhs;
} en_member_access;

typedef struct en_tuple {
    expr_node en;
    expr_node_list elements;
} en_tuple;

typedef struct en_array {
    expr_node en;
    expr_node_list elements;
} en_array;

typedef struct en_type_array {
    expr_node en;
    expr_node* inside;
    expr_node* rhs;
} en_type_array;

typedef struct en_type_slice {
    expr_node en;
    expr_node* rhs;
} en_type_slice;

typedef struct en_lambda {
    expr_node en;
    expr_node_list params;
    ast_node* body;
} en_lambda;

void astn_flags_set_access_mod(astn_flags* f, access_modifier m);
access_modifier astn_flags_get_access_mod(astn_flags f);

void astn_flags_set_const(astn_flags* f, bool cnst);
bool astn_flags_get_const(astn_flags f);

void astn_flags_set_sealed(astn_flags* f, bool sld);
bool astn_flags_get_sealed(astn_flags f);

void astn_flags_set_virtual(astn_flags* f, bool virt);
bool astn_flags_get_virtual(astn_flags f);

void astn_flags_set_static(astn_flags* f, bool stat);
bool astn_flags_get_static(astn_flags f);