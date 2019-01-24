#pragma once
#include "tokenizer.h"
#include "utils/c_extensions.h"

typedef enum PACK_ENUM ast_node_type{
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

typedef enum PACK_ENUM expr_node_type{
    ENT_OP_BINARY,
    ENT_OP_UNARY,
    ENT_CALL,
    ENT_ACCESS,
    ENT_NUMBER,
    ENT_STRING_LITERAL,
    ENT_CAST,
    ENT_IDENTIFIER,
    ENT_SCOPE_ACCESS,
    ENT_MEMBER_ACCESS,
    ENT_ARRAY,
    ENT_TUPLE,
    ENT_LAMBDA,
}expr_node_type;

typedef enum PACK_ENUM op_type{
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

}op_type;

typedef struct expr_node expr_node;

typedef struct var_decl{
    char* name;
    unsigned short modifiers;
    expr_node* start_value;
    expr_node* type;
}var_decl;

typedef struct ast_node{
    ast_node_type type;
    struct ast_node* next;
}ast_node;

typedef struct astn_expr{
    ast_node astn;
    expr_node* expr;
}astn_expr;

typedef struct astn_module{
    ast_node astn;
    char* name;
    ast_node* body;
}astn_module;

typedef struct astn_function{
    ast_node astn;
    char* name;
}astn_function;

typedef struct expr_node{
    expr_node_type type;
}expr_node;

typedef struct expr_node_list{
    expr_node** last;
}expr_node_list;

typedef struct en_op_binary{
    expr_node en;
    expr_node* lhs;
    expr_node* rhs;
}en_op_binary;

typedef struct en_op_unary{
    expr_node en;
    expr_node* right;
}en_op_unary;

typedef struct en_call{
    expr_node en;
    expr_node_list params;
}en_call;

typedef struct en_access{
    expr_node en;
    expr_node* index;
}en_access;

typedef struct en_number{
    expr_node en;
    char* number;
}en_number;

typedef struct en_string_literal{
    expr_node en;
    string literal;
}en_string_literal;

typedef struct en_cast{
    expr_node en;
    expr_node* value;
    expr_node* target_type;
}en_cast;

typedef struct en_identifier{
    expr_node en;
    char* identifier;
}en_identifier;

typedef struct en_scope_access{
    expr_node en;
    expr_node* lhs;
    expr_node* rhs;
}en_scope_access;

typedef struct en_member_access{
    expr_node en;
    expr_node* lhs;
    expr_node* rhs;
}en_member_access;

typedef struct en_array{
    expr_node en;
    expr_node_list elements;
}en_array;

typedef struct en_tuple{
    expr_node en;
    expr_node_list elements;
}en_tuple;

typedef struct en_lambda{
    expr_node en;
    expr_node_list params;
    ast_node* body;
}en_lambda;

struct lexer{
    tokenizer tk;

}lexer;