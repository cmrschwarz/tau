#include "print_ast.h"
#include "utils/math_utils.h"
#include "stdio.h"
#include "error_log.h"

int print_astn(ast_node* astn){
    return 0;
}
int p(char* c){
    fputs(c, stdout);
    fflush(stdout);
    return 0;
}
int print_expr_list(expr_node_list* enl){
    int r = 17; //don't print comma on first iteration :)
    expr_node** n = (expr_node**)ptradd(enl, sizeof(expr_node_list));
    while(n != enl->end) {
        if(!r)p(", ");
        r = print_expr(*n);
        if(r) return r;
        n++;
    }
}
int print_expr(expr_node* en){
    switch (en->type){
        case ENT_IDENTIFIER:
        case ENT_NUMBER:
            p(((en_str_value*)en)->value);
            break;
        case ENT_BINARY_LITERAL:
            putchar('\'');
            p(((en_str_value*)en)->value);
            putchar('\'');
            break;
        case ENT_STRING_LITERAL:
            putchar('"');
            p(((en_str_value*)en)->value);
            putchar('"');
            break;
        case ENT_OP_BINARY:{
            en_op_binary* b = (en_op_binary*)en;
            putchar('(');
            print_expr(b->lhs);
            putchar(' ');
            p(op_to_str(en->op_type));
            putchar(' ');
            print_expr(b->rhs);   
            putchar(')');
            break;
        }
        case ENT_OP_UNARY:{
            en_op_unary* u = (en_op_unary*)en;
            putchar('(');
            if (is_unary_op_prefix(en->op_type)){
                print_expr(u->child);
                p(op_to_str(en->op_type));
            }
            else{
                p(op_to_str(en->op_type));
                print_expr(u->child);
            }
            putchar(')');
            break;
        }
        case ENT_ARRAY:{
            putchar('{');
            print_expr_list(&((en_array*)en)->elements);
            putchar('}');
        }break;
        case ENT_TUPLE:{
            putchar('[');
            print_expr_list(&((en_tuple*)en)->elements);
            putchar(']');
        }break;
        default: p("unknown"); return -1;
    }
    return OK;
}


char* op_to_str(expr_node_type t){
    switch (t){
        case OP_ASSIGN:                 return "=";
        case OP_ADD:                    return "+";
        case OP_ADD_ASSIGN:             return "+=";
        case OP_SUB:                    return "-";
        case OP_SUB_ASSIGN:             return "-=";
        case OP_MUL:                    return "*";
        case OP_MUL_ASSIGN:             return "*=";
        case OP_DIV:                    return "/";
        case OP_DIV_ASSIGN:             return "/=";
        case OP_MOD:                    return "%";
        case OP_MOD_ASSIGN:             return "%=";
        case OP_LSHIFT:                 return "<<";
        case OP_LSHIFT_ASSIGN:          return "<<=";
        case OP_RSHIFT:                 return ">>";
        case OP_RSHIFT_ASSIGN:          return ">>=";
        case OP_LESS_THAN:              return "<";
        case OP_LESS_THAN_OR_EQUAL:     return "<=";
        case OP_GREATER_THAN:           return ">";
        case OP_GREATER_THAN_OR_EQUAL:  return ">=";
        case OP_EQUAL:                  return "==";
        case OP_UNEQAL:                 return "!=";
        case OP_AND:                    return "&&";
        case OP_BITWISE_AND:            return "&";
        case OP_BITWISE_AND_ASSIGN:     return "&=";
        case OP_OR:                     return "||";
        case OP_BITWISE_OR:             return "|";
        case OP_BITWISE_OR_ASSIGN:      return "|=";
        case OP_XOR:                    return "^^";
        case OP_BITWISE_XOR:            return "^";
        case OP_BITWISE_XOR_ASSIGN:     return "^=";
        case OP_BITWISE_NOT_ASSIGN:     return "~=";

        case OP_DEREF:                  return "*";
        case OP_POINTER_OF:             return "%";
        case OP_REF_OF:                 return "&";
        case OP_RREF_OF:                return "$";
        case OP_VALUE_OF:               return "^";
        case OP_NOT:                    return "!";
        case OP_BITWISE_NOT:            return "~";
        case OP_UNARY_PLUS:             return "+";
        case OP_UNARY_MINUS:            return "-";
        case OP_PRE_INCREMENT:          return "++";
        case OP_PRE_DECREMENT:          return "--";
        case OP_POST_INCREMENT:         return "++";
        case OP_POST_DECREMENT:         return "--";
        default:                        return NULL;
    }
    return 0;
}

bool is_unary_op_prefix(expr_node_type t){
    switch (t){
        case OP_PRE_INCREMENT:
        case OP_PRE_DECREMENT:
            return true;
        default:
            return false;
    }
}