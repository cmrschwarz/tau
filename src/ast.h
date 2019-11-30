#ifndef TAUC_AST_H
#define TAUC_AST_H

#include "src_map.h"
#include "ast_flags.h"
#include "symbol_table.h"
#include "utils/c_extensions.h"

#define VOID_ELEM ((ast_elem*)&PRIMITIVES[PT_VOID])
#define UNREACHABLE_ELEM ((ast_elem*)&PRIMITIVES[PT_UNREACHABLE])
#define PASTE_EXPR_ELEM ((ast_elem*)&PRIMITIVES[PT_UNREACHABLE])
#define TYPE_ELEM ((ast_elem*)&PRIMITIVES[PT_TYPE])

typedef struct mdg_node_s mdg_node;
typedef struct pp_resolve_node_s pp_resolve_node;

typedef enum PACK_ENUM ast_node_kind_e {
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
    SC_MACRO,
    SC_FUNC_GENERIC,
    SC_LAST_SC_ID = SC_FUNC_GENERIC,

    SYM_VAR,
    SYM_VAR_INITIALIZED,
    SYM_PARAM,
    SYM_NAMED_USING,
    SYM_IMPORT_MODULE,
    SYM_IMPORT_SYMBOL,
    SYM_IMPORT_GROUP,
    SYM_IMPORT_PARENT,
    SYM_FUNC_EXTERN, // TODO
    SYM_FUNC_OVERLOADED,
    PRIMITIVE,
    SYM_LAST_SYM_ID = PRIMITIVE,

    STMT_USING,
    STMT_COMPOUND_ASSIGN,
    STMT_LAST_STMT_ID = STMT_COMPOUND_ASSIGN,

    EXPR_BLOCK,
    EXPR_PP,
    EXPR_RETURN,
    EXPR_CONTINUE,
    EXPR_BREAK,
    EXPR_MATCH,
    EXPR_PASTE_STR,
    EXPR_IF,
    EXPR_WHILE,
    EXPR_DO_WHILE,
    EXPR_DO,
    EXPR_LOOP,
    EXPR_MACRO, // TODO
    EXPR_LITERAL,
    EXPR_IDENTIFIER,
    EXPR_VARIABLE,
    EXPR_TYPE,
    EXPR_ARRAY,
    EXPR_TUPLE,
    EXPR_LAMBDA,
    EXPR_TYPE_ARRAY,
    EXPR_TYPE_SLICE,
    EXPR_CALL,
    EXPR_NO_BLOCK_MACRO_CALL,
    EXPR_MACRO_CALL,
    EXPR_ACCESS,
    EXPR_PARENTHESES,
    EXPR_SCOPE_ACCESS,
    EXPR_MEMBER_ACCESS,
    EXPR_OP_UNARY,
    EXPR_OP_BINARY,
    EXPR_LAST_EXPR_ID = EXPR_OP_BINARY,

    TYPE_POINTER,
    TYPE_ARRAY,
    TYPE_TUPLE,
    ELEM_MDG_NODE,

} ast_node_kind;

typedef enum PACK_ENUM operator_kind_e {
    OP_NOOP, // invalid op, used for return values
    // special ops, these have their own node types but have a precedence
    OP_CALL,
    OP_ACCESS,
    OP_MACRO_CALL,
    OP_PARENTHESES,
    OP_MEMBER_ACCESS,
    OP_SCOPE_ACCESS,
    OP_CAST,

    // binary ops
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
    OP_ADDRESS_OF,
    OP_RREF_OF,
    OP_ESCAPE_SCOPE,

    OP_NOT,
    OP_BITWISE_NOT,
    OP_UNARY_PLUS,
    OP_UNARY_MINUS,
    OP_PRE_INCREMENT,
    OP_PRE_DECREMENT,
    OP_POST_INCREMENT,
    OP_POST_DECREMENT,
} operator_kind;

typedef enum PACK_ENUM primitive_kind_e {
    PT_VOID,
    PT_UNREACHABLE,
    PT_INT,
    PT_UINT,
    PT_FLOAT,
    PT_STRING,
    PT_BINARY_STRING,
    PT_TYPE,
    PT_VOID_PTR,
    PT_PASTED_EXPR,
    PRIMITIVE_COUNT,
} primitive_kind;

// root of all, can be cast from ast_node since it only contains one element
typedef struct ast_elem_s {
    ast_node_kind kind;
} ast_elem;

typedef struct ast_node_s {
    ast_node_kind kind;
    ANONYMOUS_UNION_START
    primitive_kind pt_kind;
    operator_kind op_kind;
    ANONYMOUS_UNION_END
    ast_flags flags;
    src_range srange;
} ast_node;

typedef struct file_require_s {
    src_file* file;
    src_range srange;
    bool handled;
} file_require;

typedef struct symbol_s {
    ast_node node;
    char* name;
    struct symbol_s* next;
    // PERF: store this only for symbols in mdg symtabs, either by placing it
    // before the symbol pointer or by creating some sort if lookup table
    // in these symtabs (or find a smarter solution)
    symbol_table* declaring_st;
} symbol;

typedef struct ast_body_s {
    ast_node** elements;
    symbol_table* symtab;
    src_range srange;
} ast_body;

typedef struct scope_s {
    symbol sym;
    ast_body body;
} scope;

// TODO: rename this to module frame?
typedef struct open_scope_s {
    scope sc;
    file_require* requires;
    ureg* shared_pp_decls;
} open_scope;

typedef struct sym_named_using_s {
    symbol sym;
    ast_node* target;
} sym_named_using;

typedef struct stmt_using_s {
    ast_node node;
    ast_node* target;
} stmt_using;

struct mdg_node_s;
typedef struct sym_import_symbol_s {
    symbol sym;
    // PERF: this member is kinda uneccessary since the group already knows,
    // but this connection gets lost during the insertion into tht target st
    symbol_table* target_st;
    union {
        char* name;
        symbol* sym;
    } target;
} sym_import_symbol;

typedef struct sym_import_parent_s {
    symbol sym;
    union {
        symbol_table* symtab;
        symbol* symbols;
    } children;
} sym_import_parent;

typedef struct sym_import_module_s {
    symbol sym;
    mdg_node* target;
} sym_import_module;

typedef struct sym_import_group_s {
    symbol sym; // name is NULL for an unnamed import group
    mdg_node* parent_mdgn;
    union {
        // used for named groups
        // the actual symbols are stored in the first element of the symtab
        symbol_table* symtab;
        symbol* symbols; // used for unnamed groups
    } children;
} sym_import_group;

// expr_return also uses this struct
typedef struct expr_break_s {
    ast_node node;
    ast_node* target;
    ast_node* value; // NULL if no value provided
    ast_elem* value_ctype; // void if value not provided
} expr_break;
typedef struct expr_break_s expr_return;

typedef struct expr_continue_s {
    ast_node node;
    ast_node* target;
} expr_continue;

typedef struct expr_block_s {
    ast_node node;
    char* name; // NULL if no label provided and not in if/else/etc.
    ast_body body;
    ast_elem* ctype;
    ANONYMOUS_UNION_START
    void* control_flow_ctx;
    pp_resolve_node* pprn;
    ANONYMOUS_UNION_END
} expr_block;

typedef struct expr_if_s {
    ast_node node;
    ast_node* condition;
    ast_node* if_body;
    ast_node* else_body;
    ast_elem* ctype;
} expr_if;

typedef struct expr_loop_s {
    ast_node node;
    char* name;
    ast_body body;
    ast_elem* ctype;
    void* control_flow_ctx;
} expr_loop;

typedef struct sym_param_s {
    symbol sym;
    ast_node* type;
    ast_elem* ctype;
    ast_node* default_value;
} sym_param;

typedef struct chained_macro_s {
    ast_node node;
    char* name;
    ureg param_count;
    sym_param* params;
    struct chained_macro_s* next;
} chained_macro;

typedef struct sc_macro_s {
    scope sc;
    ureg param_count;
    sym_param* params;
    ast_body body;
    ast_elem* return_ctype;
    struct sc_macro_s* next;
} sc_macro;

typedef struct expr_macro_call_s {
    ast_node node;
    char* name;
    ast_node* lhs;
    ureg arg_count;
    ast_node** args;
    ast_body body;
    ast_elem* ctype;
    sc_macro* tgt;
} expr_macro_call;

typedef struct expr_pp_s {
    ast_node node;
    ast_node* pp_expr;
    ast_elem* ctype;
    void* result;
    union result_buffer_u {
        ureg data[2];
        struct state_s {
            void* true_res_buffer;
            struct pp_resolve_node_s* pprn;
        } state;
    } result_buffer;
} expr_pp;

// TODO: add "syntactical pasting" paste{}
// we can depend on this always being after a expr_pp of sorts
typedef struct expr_paste_str_s {
    ast_node node;
    ast_node* value; // ctype shall always be string
} expr_paste_str;

typedef struct match_arm_s {
    ast_node* condition;
    ast_node* value;
} match_arm;

typedef struct expr_match_s {
    ast_node node;
    char* name;
    ast_node* match_expr;
    ast_body body;
} expr_match;

typedef struct osc_module_s {
    open_scope oscope;
} osc_module;
typedef osc_module osc_extend;

typedef struct osc_module_generic_s {
    open_scope oscope;
    sym_param* generic_params;
    ureg generic_param_count;
} osc_module_generic;
typedef osc_module_generic osc_extend_generic;

typedef struct sc_func_s {
    scope sc;
    sym_param* params;
    ureg param_count;
    ast_node* return_type;
    ast_elem* return_ctype;
    ureg id;
    pp_resolve_node* pprn;
} sc_func;

typedef struct sym_func_overloaded_s {
    symbol sym;
    scope* overloads;
} sym_func_overloaded;

typedef struct sc_func_generic_s {
    scope sc;
    sym_param* generic_params;
    ureg generic_param_count;
    sym_param* params;
    ureg param_count;
    ast_node* return_type;
    ast_elem* return_ctype;
} sc_func_generic;

typedef struct sc_struct_s {
    scope sc;
    ureg id;
    pp_resolve_node* pprn;
} sc_struct;

typedef struct sc_struct_generic_s {
    scope sc;
    sym_param* generic_params;
    ureg generic_param_count;
} sc_struct_generic;

typedef struct sc_trait_s {
    scope sc;
} sc_trait;

typedef struct sc_trait_generic_s {
    scope sc;
    sym_param* generic_params;
    ureg generic_param_count;
} sc_trait_generic;

typedef struct sym_var_s {
    symbol sym;
    ast_node* type; // may be NULL in sym_var_initialized or compoind_assignment
    ast_elem* ctype;
    pp_resolve_node* pprn;
    ureg var_id;
} sym_var;

typedef struct sym_var_initialized_s {
    sym_var var;
    ast_node* initial_value;
} sym_var_initialized;

typedef struct stmt_compound_assignment_s {
    ast_node node;
    ast_node** elements;
    ureg elem_count; // TODO
    ast_node* value;
} stmt_compound_assignment;

typedef struct expr_parentheses_s {
    ast_node node;
    ast_node* child;
} expr_parentheses;

typedef struct expr_op_binary_s {
    ast_node node;
    ast_node* lhs;
    ast_node* rhs;
    // points to a primitive (for inbuilt ops) or a sc_func (for overloaded ops)
    ast_elem* op;
} expr_op_binary;

typedef struct expr_scope_access_s {
    ast_node node;
    ast_node* lhs;
    union {
        char* name;
        symbol* sym;
    } target;
    src_range target_srange;
} expr_scope_access;
typedef struct expr_scope_access_s expr_member_access;

typedef struct expr_op_unary_s {
    ast_node node;
    ast_node* child;
    // points to a primitive (for inbuilt ops) or a sc_func (for overloaded ops)
    ast_elem* op;
} expr_op_unary;

// TODO: implement named arguments
typedef struct expr_call_s {
    ast_node node;
    ast_node* lhs;
    ast_node** args;
    ureg arg_count;
    union {
        sc_func* fn;
        expr_block* macro_block;
    } target;
} expr_call;

typedef struct expr_access_s {
    ast_node node;
    ast_node* lhs;
    ast_node** args;
    ureg arg_count;
} expr_access;

typedef struct expr_literal_s {
    ast_node node;
    union {
        char* str;
        ureg val_ureg;
    } value;
} expr_literal;

typedef struct expr_identifier_s {
    ast_node node;
    union {
        char* str;
        symbol* sym;
    } value;
} expr_identifier;

typedef struct expr_cast_s {
    ast_node node;
    ast_node* value;
    ast_node* target_type;
} expr_cast;

typedef struct expr_tuple_s {
    ast_node node;
    ast_node** elements;
    ureg elem_count;
} expr_tuple;

typedef struct expr_array_s {
    ast_node node;
    ast_node** elements;
    ureg elem_count;
} expr_array;

typedef struct expr_type_array_s {
    ast_node node;
    ast_node* inside;
    ast_node* rhs;
} expr_type_array;

typedef struct expr_type_slice_s {
    ast_node node;
    ast_node* rhs;
} expr_type_slice;

typedef struct expr_lambda_s {
    ast_node node;
    sym_param* params;
    ast_body body;
} expr_lambda;

typedef enum ast_type_mod_s {
    ATM_NONE = 0,
    ATM_CONST = 1,
    ATM_PTR = 2,
    ATM_REF = 3,
} ast_type_mod;

#define ATM_BITS 2
#define ATM_MASK 0x3
#define ATM_PER_BYTE (8 / ATM_BITS)
#define ATM_BYTES (sizeof(ast_elem*) - sizeof(ast_node_kind))
#define ATM_MAX_COUNT (ATM_BYTES * ATM_PER_BYTE)

typedef struct type_pointer_s {
    ast_node_kind kind;
    bool rvalue;
    ast_elem* base;
} type_pointer;

typedef struct type_array_s {
    ast_node_kind kind;
    ast_elem* ctype_members;
    ureg size;
} type_array;

typedef struct type_tuple_s {
    ast_node_kind kind;
    ast_elem** ctypes_of_members;
    ureg size;
} type_tuple;

typedef struct primitive_s {
    symbol sym;
    ureg size; // size is different from alignment e.g. for void, string, etc.
    ureg alignment;
} primitive;

extern primitive PRIMITIVES[];

src_range ast_node_get_src_range(ast_node* s);
bool ast_elem_is_open_scope(ast_elem* s);
bool ast_elem_is_scope(ast_elem* s);
bool ast_elem_is_symbol(ast_elem* s);
bool ast_elem_is_expr(ast_elem* s);
bool ast_elem_is_stmt(ast_elem* s);
ast_body* ast_elem_get_body(ast_elem* s);
char* ast_elem_get_label(ast_elem* n, bool* lbl);
src_file* open_scope_get_file(open_scope* s);
src_file* ast_node_get_file(ast_node* n, symbol_table* st);
void ast_node_fill_src_range(
    ast_node* n, symbol_table* st, src_range_large* srl);
bool ast_body_is_braced(ast_body* b);

bool is_unary_op_postfix(operator_kind t);
ast_node* get_parent_body(scope* parent);
void ast_node_get_bounds(ast_node* n, ureg* start, ureg* end);
char* op_to_str(operator_kind t);

#endif
