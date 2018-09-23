#pragma once
#include "utils/dbuffer.h"
#include "tokenizer.h"
#include "lexer.h"
typedef struct{
    dbuffer data;
    lexer* tks_start;
    lexer* tks_end;
    
}tauc;