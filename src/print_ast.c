#include "print_ast.h"
#include "utils/math_utils.h"
#include "stdio.h"
#include "error_log.h"

int print_astn(ast_node* astn){
    return 0;
}
int pc(char c){
   putchar(c);
}
int p(char* c){
    if(c == NULL)c = "unknown";
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
            pc('\'');
            p(((en_str_value*)en)->value);
            pc('\'');
            break;
        case ENT_STRING_LITERAL:
            pc('"');
            p(((en_str_value*)en)->value);
            pc('"');
            break;
        case ENT_OP_BINARY:{
            en_op_binary* b = (en_op_binary*)en;
            print_expr(b->lhs);
            pc(' ');
            p(op_to_str(en->op_type));
            pc(' ');
            print_expr(b->rhs);   
            break;
        }
        case ENT_OP_UNARY:{
            en_op_unary* u = (en_op_unary*)en;
            if (is_unary_op_postfix(en->op_type)){
                print_expr(u->child);
                p(op_to_str(en->op_type));
            }
            else{
                p(op_to_str(en->op_type));
                print_expr(u->child);
            }
            break;
        }
        case ENT_ARRAY:{
            pc('{');
            print_expr_list(&((en_array*)en)->elements);
            pc('}');
        }break;
        case ENT_TUPLE:{
            pc('[');
            print_expr_list(&((en_tuple*)en)->elements);
            pc(']');
        }break;
        case ENT_OP_CALL:{
            en_call* c = (en_call*)en;
            print_expr(c->lhs);
            pc('(');
            print_expr_list(&c->args);
            pc(')');
        }break;
        case ENT_OP_ACCESS:{
            en_access* acc = (en_access*)en;
            print_expr(acc->lhs);
            pc('[');
            print_expr_list(&acc->args);
            pc(']');
        }break;
        case ENT_OP_PARENTHESES:{
            en_parentheses* pr = (en_parentheses*)en;
            pc('(');
            print_expr(pr->child);
            pc(')');
        } break;
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
        case OP_CONST:                  return "const ";
        default:                        return NULL;
    }
    return 0;
}

bool is_unary_op_postfix(expr_node_type t){
    switch (t){
        case OP_POST_INCREMENT:
        case OP_POST_DECREMENT:
            return true;
        default:
            return false;
    }
}