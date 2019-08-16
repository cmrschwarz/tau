#pragma once
#include "src_map.h"
#include "ast_node_flags.h"
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
    SYM_VAR_DECL,
    SYM_VAR_DECL_UNINITIALIZED,
    SYM_PARAM,
    SYM_LAST_SYM_ID = SYM_PARAM,

    STMT_IMPORT,
    STMT_USING,

    STMT_COMPOUND_ASSIGN,
    STMT_LAST_STMT_ID = STMT_COMPOUND_ASSIGN,

    EXPR_BLOCK,

    EXPR_PP,
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

    PRIMITIVE,
    TYPE_ARRAY,
    TYPE_TUPLE,
    TYPE_MODIFIERS,

} ast_node_kind;

typedef enum PACK_ENUM operator_kind {
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
    OP_PP, // only used for precedence, has it's own expr_pp node
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
} operator_kind;

typedef enum PACK_ENUM primitive_kind {
    PT_INT,
    PT_UINT,
    PT_FLOAT,
    PT_STRING,
    PT_LAST_ENTRY,
} primitive_kind;

typedef struct ast_element {
    ast_node_kind kind;
} ast_element;

typedef struct ast_node {
    ast_node_kind kind;
    ANONYMOUS_UNION_START
    primitive_kind primitive_kind;
    operator_kind operator_kind;
    ANONYMOUS_UNION_END
    ast_node_flags flags;
    src_range srange;
} ast_node;

typedef struct file_require {
    src_file* file;
    src_range srange;
} file_require;

typedef struct scope scope;
typedef struct symbol {
    ast_node node;
    char* name;
    struct symbol* next;
} symbol;

typedef struct body {
    ast_node** elements;
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
    ast_node* target;
} sym_named_using;

typedef struct stmt_using {
    ast_node node;
    ast_node* target;
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

typedef struct stmt_import {
    ast_node node;
    module_import module_import;
} stmt_import;

typedef struct expr_return {
    ast_node node;
    ast_node* value; // NULL if no value provided
} expr_return;

typedef struct expr_break {
    ast_node node;
    ast_node* target;
    ast_node* value; // NULL if no value provided
} expr_break;

typedef struct expr_continue {
    ast_node node;
    ast_node* target;
} expr_continue;

typedef struct expr_block {
    ast_node node;
    char* name; // NULL if no label provided and not in if/else
    body body;
} expr_block;

typedef struct expr_if {
    ast_node node;
    ast_node* condition;
    ast_node* if_body;
    ast_node* else_body;
} expr_if;

typedef struct expr_loop {
    ast_node node;
    char* name;
    body body;
} expr_loop;

typedef struct expr_macro {
    ast_node node;
    ast_node** args;
    char* name;
    body body;
    struct expr_macro* next;
} expr_macro;

typedef struct expr_pp {
    ast_node node;
    ast_node* pp_expr;
} expr_pp;

typedef struct match_arm {
    ast_node* condition;
    ast_node* value;
} match_arm;

typedef struct expr_match {
    ast_node node;
    char* name;
    ast_node* match_expr;
    body body;
} expr_match;

typedef struct sym_param {
    symbol symbol;
    ast_node* type;
    ast_element* type_reduced;
    ast_node* default_value;
} sym_param;

typedef struct sc_func {
    scope scope;
    sym_param* params;
    ast_node* return_type;
    ast_element* return_type_reduced;
} sc_func;

typedef struct sc_func_generic {
    scope scope;
    sym_param* generic_params;
    sym_param* params;
    ast_node* return_type;
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

typedef struct sym_var_decl {
    symbol symbol;
    ast_node* type;
    ast_element* type_reduced;
    ast_node* value;
} sym_var_decl;

typedef struct sym_var_decl_uninitialized {
    symbol symbol;
    ast_node* type;
    ast_element* type_reduced;
} sym_var_decl_uninitialized;

typedef struct stmt_compound_assignment {
    ast_node node;
    ast_node** elements;
    ast_node* value;
} stmt_compound_assignment;

typedef struct expr_parentheses {
    ast_node node;
    ast_node* child;
} expr_parentheses;

typedef struct expr_op_binary {
    ast_node node;
    ast_node* lhs;
    ast_node* rhs;
} expr_op_binary;

typedef struct expr_op_unary {
    ast_node node;
    ast_node* child;
} expr_op_unary;

// TODO: implement named arguments
typedef struct expr_call {
    ast_node node;
    ast_node* lhs;
    ast_node** args;
} expr_call;

typedef struct expr_access {
    ast_node node;
    ast_node* lhs;
    ast_node** args;
} expr_access;

typedef struct expr_str_value {
    ast_node node;
    union {
        char* str;
        ast_node* node;
    } value;
} expr_str_value;

typedef expr_str_value expr_number;
typedef expr_str_value expr_identifier;
typedef expr_str_value expr_string_literal;
typedef expr_str_value expr_binary_literal;

typedef struct expr_cast {
    ast_node node;
    ast_node* value;
    ast_node* target_type;
} expr_cast;

typedef struct expr_scope_access {
    ast_node node;
    ast_node* lhs;
    ast_node* rhs;
} expr_scope_access;

typedef struct expr_member_access {
    ast_node node;
    ast_node* lhs;
    ast_node* rhs;
} expr_member_access;

typedef struct expr_tuple {
    ast_node node;
    ast_node** elements;
} expr_tuple;

typedef struct expr_array {
    ast_node node;
    ast_node** elements;
} expr_array;

typedef struct expr_type_array {
    ast_node node;
    ast_node* inside;
    ast_node* rhs;
} expr_type_array;

typedef struct expr_type_slice {
    ast_node node;
    ast_node* rhs;
} expr_type_slice;

typedef struct expr_lambda {
    ast_node node;
    sym_param* params;
    body body;
} expr_lambda;

typedef enum ast_type_mod {
    ATM_NONE = 0,
    ATM_CONST = 1,
    ATM_PTR = 2,
    ATM_REF = 3,
} ast_type_mod;

#define ATM_BITS 2
#define ATM_MASK 0x3
#define ATM_PER_BYTE (8 / ATM_BITS)
#define ATM_BYTES (sizeof(ast_element*) - sizeof(ast_node_kind))
#define ATM_MAX_COUNT (ATM_BYTES * ATM_PER_BYTE)

typedef struct ast_type_node {
    ast_node_kind kind;
    u8 mods[ATM_BYTES];
} ast_type_node;

typedef struct type_modifiers {
    ast_type_node node;
    ast_element* base;
} type_modifiers;

typedef struct type_array {
    ast_type_node node;
    ast_element* members_type;
    ureg size;
} type_array;

typedef struct type_tuple {
    ast_type_node node;
    ast_element** member_types;
    ureg size;
} type_tuple;

extern symbol primitives[];

int ast_type_node_get_mod_count(ast_type_node atn);
ast_type_mod ast_type_node_get_mod_n(ast_type_node atn, int n);
void ast_type_node_set_mod_n(ast_type_node atn, ast_type_mod mod, int n);

src_range ast_node_get_src_range(ast_node* s);
bool ast_node_is_open_scope(ast_node* s);
bool ast_node_is_scope(ast_node* s);
bool ast_node_is_symbol(ast_node* s);
bool ast_node_is_expr(ast_node* s);
bool ast_node_is_stmt(ast_node* s);
src_file* open_scope_get_file(open_scope* s);
src_file* ast_node_get_file(ast_node* n, symbol_table* st);

bool body_is_braced(body* b);

bool is_unary_op_postfix(operator_kind t);
ast_node* get_parent_body(scope* parent);
void ast_node_get_highlight_bounds(ast_node* n, ureg* start, ureg* end);
void ast_node_get_bounds(ast_node* n, ureg* start, ureg* end);
