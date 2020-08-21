#pragma once

#include "src_map.h"
#include "ast_flags.h"
#include "symbol_table.h"
#include "generic_inst_map.h"
#include "utils/threading.h"
#include "utils/c_extensions.h"
#include "utils/list.h"

#define VOID_ELEM ((ast_elem*)&PRIMITIVES[PT_VOID])
#define ERROR_ELEM ((ast_elem*)&PRIMITIVES[PT_ERROR])
#define UNREACHABLE_ELEM ((ast_elem*)&PRIMITIVES[PT_UNREACHABLE])
#define PASTED_EXPR_ELEM ((ast_elem*)&PRIMITIVES[PT_PASTED_EXPR])
#define GENERIC_TYPE_ELEM ((ast_elem*)&PRIMITIVES[PT_GENERIC_TYPE])
#define TYPE_ELEM ((ast_elem*)&PRIMITIVES[PT_TYPE])

typedef struct mdg_node_s mdg_node;
typedef struct pp_resolve_node_s pp_resolve_node;
typedef struct prp_var_node_s prp_var_node;
typedef struct prp_block_node_s prp_block_node;
typedef struct src_file_s src_file;
typedef struct file_map_head_s file_map_head;
typedef struct type_slice_s type_slice;
typedef struct sc_func_s sc_func;
typedef struct expr_block_s expr_block;
typedef struct src_map_s src_map;

typedef enum PACK_ENUM ast_node_kind_e {
    ELEM_INVALID, // make 0 invalid for debugging
    // used for expr pps that still have pprns after being overwritten by pastes
    ELEM_DUMMY,
    ELEM_MDG_NODE,
    ELEM_FIRST_ID = ELEM_MDG_NODE,
    ELEM_SRC_FILE,
    ELEM_SRC_LIB,
    ELEM_SRC_DIR,
    ELEM_LAST_ID = ELEM_SRC_DIR,

    ASTN_ANONYMOUS_SYM_IMPORT_GROUP,
    ASTN_FIRST_ID = ASTN_ANONYMOUS_SYM_IMPORT_GROUP,
    ASTN_ANONYMOUS_MOD_IMPORT_GROUP,

    MF_MODULE,
    MF_FIRST_ID = MF_MODULE,
    MF_MODULE_GENERIC,
    MF_EXTEND,
    MF_EXTEND_GENERIC,
    MF_LAST_ID = MF_EXTEND_GENERIC,

    SC_STRUCT,
    SC_FIRST_ID = SC_STRUCT,
    SC_STRUCT_GENERIC,
    SC_STRUCT_GENERIC_INST,
    SC_TRAIT,
    SC_TRAIT_GENERIC,
    SC_FUNC,
    SC_MACRO,
    SC_FUNC_GENERIC,
    SC_LAST_ID = SC_FUNC_GENERIC,

    SYM_PRIMITIVE,
    SYM_FIRST_ID = SYM_PRIMITIVE,
    SYM_PARAM,
    SYM_GENERIC_PARAM,
    SYM_PARAM_GENERIC_INST,
    SYM_FUNC_OVERLOADED,
    SYM_VAR,
    SYM_FIRST_OPEN_ID = SYM_VAR,
    SYM_VAR_INITIALIZED,
    SYM_NAMED_USE,
    SYM_IMPORT_MODULE,
    SYM_IMPORT_SYMBOL,
    SYM_NAMED_MOD_IMPORT_GROUP,
    SYM_NAMED_SYM_IMPORT_GROUP,
    SYM_IMPORT_PARENT,
    SYM_FUNC_EXTERN, // TODO
    SYM_LAST_ID = SYM_FUNC_EXTERN,
    SYM_LAST_OPEN_ID = SYM_LAST_ID,

    STMT_USE,
    STMT_FIRST_ID = STMT_USE,
    STMT_PASTE_EVALUATION,
    STMT_COMPOUND_ASSIGN,
    STMT_LAST_ID = STMT_COMPOUND_ASSIGN,

    EXPR_BLOCK,
    EXPR_FIRST_ID = EXPR_BLOCK,
    EXPR_CAST,
    EXPR_PP,
    EXPR_PASTE_EVALUATION,
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
    EXPR_MACRO_STR_CALL,
    EXPR_ACCESS,
    EXPR_PARENTHESES,
    EXPR_SCOPE_ACCESS,
    EXPR_MEMBER_ACCESS,
    EXPR_OP_UNARY,
    EXPR_OP_BINARY,
    EXPR_OP_INFIX_FUNC,
    EXPR_ARRAY_TYPE,
    EXPR_SLICE_TYPE,
    EXPR_LAST_ID = EXPR_SLICE_TYPE,
    ASTN_LAST_ID = EXPR_LAST_ID,

    TYPE_POINTER,
    TYPE_FIRST_ID = TYPE_POINTER,
    TYPE_ARRAY,
    TYPE_SLICE,
    TYPE_TUPLE,
    TYPE_LAST_ID = TYPE_TUPLE,
} ast_node_kind;

typedef enum PACK_ENUM operator_kind_e {
    OP_NOOP, // invalid op, used for return values
    // special ops, these have their own node types but have a precedence
    OP_CALL,
    OP_ACCESS,
    OP_MACRO_CALL,
    OP_MACRO_STR_CALL,
    OP_PARENTHESES,
    OP_MEMBER_ACCESS,
    OP_SCOPE_ACCESS,
    OP_CAST,
    OP_ARRAY_ACCESS, // used to differentiate between different access ops
    OP_GENERIC_ACCESS, // in the resolver and onwards

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
    // THINK: maybe add OP_POINTER_TO for once we know after resolution?
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
    PT_ERROR,
    PT_UNREACHABLE,
    PT_UNDEFINED,
    PT_DEFINED,
    PT_GENERIC_TYPE,
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
    union {
        ast_node_kind kind;
        ast_elem elem;
    };
    union {
        primitive_kind pt_kind;
        operator_kind op_kind;
        u8 symbol_flags;
    };
    u16 flags;
    src_range srange;
} ast_node;

// TODO: make this a proper ast stmt so we can print it, use it in pp parsing
// etc.
typedef struct file_require_s {
    file_map_head* fmh;
    src_range srange;
    bool handled;
    bool runtime;
    bool is_extern;
    bool is_pp;
} file_require;

typedef struct symbol_s {
    ast_node node;
    char* name;
    // the symbol table where this symbol was DECLARED in
    // (not necessarily the one it resides in)
    // for pasted blocks this points to the metatable
    // for mdg node symbols it points to the declaring module frame
    ast_body* declaring_body;
    struct symbol_s* next;
} symbol;

// all symbols with specifieable visibility
typedef struct open_symbol_s {
    symbol sym;
    // inclusive limit for private[foo] visibility
    ast_body* visible_within_body;
} open_symbol;

typedef struct ast_body_s {
    ast_node** elements; // zero terminated
    struct ast_body_s* parent;
    ast_elem* owning_node;
    symbol_table* symtab;
    // this is always NULL for module frames, but
    // it's still necessary because we want to be able to get the
    // pprn of the cuurent body
    union {
        prp_block_node* prpbn;
        void* control_flow_ctx;
        // this works, since the block ITSELF (not its children)
        // won't be touched by the backend until its fully resolved
        // and then we don't need the pprn anymore
        pp_resolve_node* pprn;
    };
    src_range srange;
} ast_body;

typedef struct scope_s {
    open_symbol osym;
    ast_body body;
} scope;

typedef struct sym_named_use_s {
    open_symbol osym;
    ast_node* target;
} sym_named_use;

typedef struct stmt_use_s {
    ast_node node;
    ast_node* target;
} stmt_use;

typedef struct import_module_data_s {
    // not relative to what, but who actually imported
    mdg_node* importing_module;
    mdg_node* imported_module;
    atomic_boolean done;
    pp_resolve_node* pprn;
} import_module_data;

typedef struct sym_import_module_s {
    open_symbol osym;
    import_module_data im_data;
} sym_import_module;

// the foo in import foo::bar;
// problematic since we can also have import foo::baz;
// we join these into one and drop the other
typedef struct sym_import_parent_s {
    open_symbol osym;
    // to prevent weird collisions like foo::bar, baz::{foo::bar}
    mdg_node* module;
    union {
        symbol* children; // used during add decl to add all associates
        symbol_table* symtab; // created after all siblings are combined
    };
} sym_import_parent;

typedef struct import_group_data_s {
    mdg_node* relative_to;
    list children_ordered; // allocated from module poo
} import_group_data;

typedef struct sym_named_sym_import_group_s {
    open_symbol osym;
    import_module_data im_data;
    import_group_data ig_data;
    symbol_table* symtab;
} sym_named_sym_import_group;

typedef struct sym_named_mod_import_group_s {
    open_symbol osym;
    mdg_node* group_parent;
    import_group_data ig_data;
    symbol_table* symtab;
} sym_named_mod_import_group;

typedef struct astn_anonymous_mod_import_group_s {
    ast_node node;
    mdg_node* group_parent;
    import_group_data ig_data;
} astn_anonymous_mod_import_group;

typedef struct astn_anonymous_sym_import_group_s {
    ast_node node;
    import_group_data ig_data;
    import_module_data im_data;
} astn_anonymous_sym_import_group;

struct mdg_node_s;
typedef struct sym_import_symbol_s {
    open_symbol osym;
    ast_node* import_group;
    // for overloaded symbols, we can't resolve this to a single target symbol
    // therefore we keep the name and store the st to start overload lookup
    // for non overlodable symbols (vars, etc.) this is NULL
    ast_body* target_body;
    union {
        char* name;
        symbol* sym;
    } target;
} sym_import_symbol;

typedef struct expr_block_base_s {
    ast_node node;
    char* name;
    ast_elem* ctype;
    ast_body body;
} expr_block_base;

// these two are currently identical, so this is a little silly
typedef struct expr_block_s {
    expr_block_base ebb;
} expr_block;

typedef struct expr_loop_s {
    expr_block_base ebb;
} expr_loop;

typedef struct expr_continue_s {
    ast_node node;
    union {
        expr_block_base* ebb;
        const char* label;
    } target;
} expr_continue;

typedef struct expr_break_s {
    ast_node node;
    union {
        expr_block_base* ebb;
        const char* label;
    } target;
    ast_node* value; // NULL if no value provided
    ast_elem* value_ctype; // void if value not provided
} expr_break;

typedef struct expr_return_s {
    ast_node node;
    ast_node* target; // func or lambda(TODO)
    ast_node* value; // NULL if no value provided
    ast_elem* value_ctype; // void if value not provided
} expr_return;

// this does not have a name or a control_flow_context member since it is not a
// break target. only if the if/else branches contains a block these blocks
// are break targets and can have a (potentially different) name.
// x := if (true) break 4; is an error (unless there's a parent block)
// During resolution both if and else block point to the same shared control
// flow context since the if/else expression results in one value
typedef struct expr_if_s {
    ast_node node;
    ast_node* condition;
    ast_node* if_body;
    ast_node* else_body;
    ast_elem* ctype;
    prp_block_node* prpbn;
} expr_if;

typedef struct sym_param_s {
    symbol sym;
    ast_node* type;
    ast_elem* ctype;
    ast_node* default_value;
} sym_param;

typedef struct sym_param_generic_inst_s {
    symbol sym;
    ast_elem* value;
    ast_elem* ctype;
} sym_param_generic_inst;

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

typedef struct expr_macro_str_call_s {
    ast_node node;
    ast_node* lhs;
    string str_param;
} expr_macro_str_call;

typedef struct pasted_str_s {
    char* str;
    struct pasted_str_s* next;
} pasted_str;

typedef struct expr_pp_s {
    ast_node node;
    ast_node* pp_expr;
    ast_elem* ctype;
    struct pp_resolve_node_s* pprn;
    void* result;
    union result_buffer_u {
        // this must be big enough that
        // sizeof(expr_paste_evaluation) <= sizeof(expr_pp)
        ureg data[8];
        struct paste_result_s {
            pasted_str* first;
            pasted_str** last_next;
        } paste_result;
    } result_buffer;
} expr_pp;

// TODO: add "syntactical pasting" paste{}
// we can depend on this always being after a expr_pp of sorts
typedef struct expr_paste_str_s {
    ast_node node;
    expr_pp* target;
    ast_node* value; // ctype shall always be string
} expr_paste_str;

// we take the existing expr_pp and override it with this to preserve
// the pointers
typedef struct paste_evaluation_s {
    ast_node node;
    ast_body body;
    pasted_str* paste_str;
    union {
        pasted_str* read_str;
        ast_elem* ctype;
    };
    union {
        char* read_pos;
        ast_node* expr;
    };
    ast_node* source_pp_expr; // the original expr contained in expr_pp
    src_range source_pp_srange; // the src range OF the original expr_pp
} paste_evaluation;

// ASSERT: sizeof(stmt_paste_evaluation) < sizeof(expr_pp)

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

typedef struct sc_func_base_s {
    scope sc;
    sym_param* params;
    ureg param_count;
    ast_node* return_type;
    ast_elem* return_ctype;
} sc_func_base;

typedef struct sc_func_s {
    sc_func_base fnb;
    ureg id;
} sc_func;

typedef struct sc_func_generic_s {
    sc_func_base fnb;
    sym_param* generic_params;
    ureg generic_param_count;
    generic_inst_map gim;
} sc_func_generic;

typedef struct sc_func_generic_inst_s {
    sc_func fnb;
    sc_func_generic* generic;
} sc_func_generic_inst;

typedef struct sym_func_overloaded_s {
    symbol sym;
    scope* overloads;
} sym_func_overloaded;

typedef struct sc_struct_s sc_struct;

typedef struct sc_struct_base_s {
    scope sc;
    sc_struct* extends;
    ast_node* extends_spec;
} sc_struct_base;

typedef struct sc_struct_s {
    sc_struct_base sb;
    ureg id;
    sc_func* dtor;
} sc_struct;

typedef struct sc_struct_generic_inst_s sc_struct_generic_inst;

typedef struct sc_struct_generic_s {
    sc_struct_base sb;
    sym_param* generic_params;
    ureg generic_param_count;
    sc_struct_generic_inst* instances;
} sc_struct_generic;

typedef struct sc_struct_generic_inst_s {
    sc_struct st;
    sym_param_generic_inst* generic_args;
    ureg generic_arg_count;
    sc_struct_generic* base;
} sc_struct_generic_inst;

typedef struct sc_trait_s {
    sc_struct_base sb;
} sc_trait;

typedef struct sc_trait_generic_s {
    sc_struct_base sb;
    sym_param* generic_params;
    ureg generic_param_count;
} sc_trait_generic;

typedef struct sym_var_s {
    open_symbol osym;
    ast_node* type; // may be NULL in sym_var_initialized or compound_assignment
    ast_elem* ctype;
    // PERF: maybe use a scheme to share pprn* and id memory
    // (e.g. put id in pprn, calc id after pp...)
    union {
        pp_resolve_node* pprn;
        prp_var_node* prpvn;
    };
    ureg var_id;
} sym_var;

// TODO: remove this, maybe create member variable opt instead (no open_symbol
// required)
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

typedef struct expr_slice_type_s {
    ast_node node;
    ast_node* base_type;
    type_slice* ctype;
} expr_slice_type;

typedef struct expr_array_type_s {
    expr_slice_type slice_type;
    ast_node* length_spec; // NULL in case of [_]
} expr_array_type;

// TODO
typedef struct expr_op_infix_func_s {
    ast_node node;
    ast_node* lhs;
    ast_node* rhs;
    // points to a primitive (for inbuilt ops) or a sc_func (for overloaded ops)
    union {
        char* name;
        sc_func* fn;
        expr_block* macro_block;
    } target;
} expr_op_infix_func;

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
    // one might argue that this is inefficient/weird for the common case foo()
    // since the lhs 'identifier' is not really a full entity by itself
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
    ast_elem* ctype;
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
    ast_elem* target_ctype;
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
    expr_slice_type* explicit_decl; // slice or array decl or NULL if implicit
    type_slice* ctype; // either slice or array
} expr_array;

typedef struct expr_lambda_s {
    ast_node node;
    sym_param* params;
    ast_body body;
} expr_lambda;

typedef struct module_frame_s {
    ast_node node;
    ast_body body;
    file_require* requires;
    // TODO: take out all smap annotations from src_range and rely solely on
    // these (put some in paste stmt/expr aswell)
    src_map* smap;
} module_frame;

typedef struct module_frame_generic_s {
    module_frame frame;
    sym_param* generic_params;
    ureg generic_param_count;
} module_frame_generic;

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
    union {
        ast_node_kind kind;
        ast_elem elem;
    };
    ast_elem* base;
    bool rvalue;
} type_pointer;

typedef struct type_slice_s {
    union {
        ast_node_kind kind;
        ast_elem elem;
    };
    ast_elem* ctype_members;
} type_slice;

typedef struct type_array_s {
    type_slice slice_type;
    ureg length;
} type_array;

typedef struct type_tuple_s {
    union {
        ast_node_kind kind;
        ast_elem elem;
    };
    ast_elem** ctypes_of_members;
    ureg size;
} type_tuple;

typedef struct primitive_s {
    symbol sym;
    ureg size; // size is different from alignment e.g. for void, string, etc.
    ureg alignment;
} primitive;

extern primitive PRIMITIVES[];

bool ast_elem_is_func_base(ast_elem* s);
bool symbol_is_open_symbol(symbol* s);
bool ast_elem_is_struct_base(ast_elem* s);
bool ast_elem_is_struct(ast_elem* s);
bool ast_elem_is_var(ast_elem* s);
bool ast_elem_is_any_import(ast_elem* s);
bool ast_elem_is_module_frame(ast_elem* s);
bool ast_elem_is_scope(ast_elem* s);
bool ast_elem_is_symbol(ast_elem* s);
bool ast_elem_is_expr(ast_elem* s);
bool ast_elem_is_type_slice(ast_elem* s);
bool ast_elem_is_stmt(ast_elem* s);
bool ast_elem_is_node(ast_elem* e);
bool ast_elem_is_expr_block_base(ast_elem* n);
bool ast_elem_is_paste_evaluation(ast_elem* s);
bool assignment_is_meta_assignment(expr_op_binary* ob, bool* defined);
ast_body* ast_elem_get_body(ast_elem* s);
ast_body* ast_body_get_non_paste_parent(ast_body* b);
char* ast_elem_get_label(ast_elem* n, bool* lbl);
src_map* scope_get_smap(scope* s);
src_map* module_frame_get_smap(module_frame* mf);
void ast_node_get_src_range(ast_node* n, ast_body* body, src_range_large* srl);
void ast_node_get_full_src_range(
    ast_node* n, ast_body* body, src_range_large* srl);
bool ast_body_is_braced(ast_body* b);
bool ast_body_is_public(ast_body* st);

ast_body* ast_body_get_parent_module_body(ast_body* b);
src_map* ast_body_get_smap(ast_body* b);

bool is_unary_op_postfix(operator_kind t);
ast_node* get_parent_body(scope* parent);
void ast_node_get_bounds(ast_node* n, ureg* start, ureg* end);
char* op_to_str(operator_kind t);

void import_group_get_data(
    ast_node* n, import_group_data** ig_data, import_module_data** im_data,
    const char** name, mdg_node** group_parent);

bool ast_elem_is_import_group(ast_elem* e);
