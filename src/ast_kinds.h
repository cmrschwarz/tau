#pragma once
#include "utils/c_extensions.h"

typedef enum PACK_ENUM ast_node_kind_e {
    ELEM_INVALID, // make 0 invalid for debugging
    // used for expr pps that still have pprns after being overwritten by pastes
    ELEM_DUMMY,
    ELEM_MDG_NODE,
    ELEM_FIRST_ID = ELEM_MDG_NODE,
    ELEM_SRC_FILE,
    ELEM_PASTED_SRC,
    ELEM_SRC_LIB,
    ELEM_SRC_DIR,
    ELEM_LAST_ID = ELEM_SRC_DIR,

    ASTN_ANONYMOUS_SYM_IMPORT_GROUP,
    ASTN_FIRST_ID = ASTN_ANONYMOUS_SYM_IMPORT_GROUP,
    ASTN_ANONYMOUS_MOD_IMPORT_GROUP,

    TRAIT_IMPL,
    TRAIT_IMPL_GENERIC,
    TRAIT_IMPL_GENERIC_INST,

    MF_MODULE,
    MF_FIRST_ID = MF_MODULE,
    MF_MODULE_GENERIC,
    MF_EXTEND,
    MF_EXTEND_GENERIC,
    MF_LAST_ID = MF_EXTEND_GENERIC,

    SC_STRUCT,
    SYM_FIRST_ID = SC_STRUCT, // scopes are symbols
    SC_FIRST_ID = SC_STRUCT,
    SC_STRUCT_GENERIC,
    SC_STRUCT_GENERIC_INST,
    SC_TRAIT,
    SC_TRAIT_GENERIC,
    SC_TRAIT_GENERIC_INST,
    SC_FUNC,
    SC_MACRO,
    SC_FUNC_GENERIC,
    SC_FUNC_GENERIC_INST,
    SC_LAST_ID = SC_FUNC_GENERIC_INST,

    SYM_PRIMITIVE,
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
    EXPR_SPECIAL_IDENTIFIER,
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
    PT_GENERIC_TRAIT,
    PT_FLUID_INT,
    PT_INT,
    PT_UINT,
    PT_FLOAT,
    PT_STRING,
    PT_BINARY_STRING,
    PT_TYPE,
    PT_TRAIT,
    PT_VOID_PTR,
    PT_PASTED_EXPR,
    PRIMITIVE_COUNT,
} primitive_kind;
