#pragma once
#include "tokenizer.h"

typedef enum ast_node_type{
    ASTNT_MODULE,
    
    ASTNT_FUNCTION,
    ASTNT_GENERIC_FUNCTION,
    ASTNT_STRUCT,
    ASTNT_GENERIC_STRUCT,

    ASTNT_VAR_DECLARATION,
    
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

typedef enum expression_node_type{
    ENT_OP_BINARY,
    ENT_OP_UNARY,
    ENT_CALL,
    ENT_ACCESS,
    ENT_NUMBER,
    ENT_STRING_LITERAL,
    ENT_CAST,
    ENT_VARIABLE,
    ENT_ARRAY,
    ENT_TUPLE,
    ENT_LAMBDA,
}expression_node_type;

typedef enum operator_type{
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

    OP_DEREF,       // *
    OP_POINTER_OF,  // %
    OP_REF_OF,      // &
    OP_RREF_OF,     // $
    OP_VAL_OF,      // ^ (don't confuse with deref, this is used in lambdas to indicate closure by value)

    OP_LESS_THAN,
    OP_LESS_THAN_OR_EQUAL,
    OP_GREATER_THAN,
    OP_GREATER_THAN_OR_EQUAL,
    OP_EQUAL,
    OP_UNEQAL,

    OP_AND,
    OP_BIT_AND,
    OP_OR,
    OP_BIT_OR,
    OP_XOR,
    OP_BIT_XOR,
    
    OP_UNARY_NOT,
    OP_UNARY_BIT_NOT,
    OP_UNARY_PLUS,
    OP_UNARY_MINUS,

}operator_type;

typedef struct expression_node{
    expression_node_type ent;
}expression_node;

typedef struct en_op_binary{
    expression_node en;
    expression_node* left;
    expression_node* right;
}en_op_binary;

struct lexer{
    tokenizer tk;

}lexer;