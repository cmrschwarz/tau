#pragma once
#include "types.h"
#include <string.h>

typedef struct{
    char* start;
    char* end;
}string;

static inline void string_assign(string* s, char* start, char* end){
    s->start = start;
    s->end = end;
}

static inline void string_set(string* s, char* zstring){
    s->start = zstring;
    s->end = zstring + strlen(zstring);
}

static inline ureg string_len(string s){
    return s.end - s.start;
}

//TODO: try to make use of the really good standard
//string compare functions for superior performance
static inline int string_cmp(string l, string r){
    if(string_len(l) < string_len(r)){
        while(l.start != l.end){
            if(*l.start != *r.start)return *l.start - *r.start;
            r.start++;
            l.start++;
        }
        return -1;
    }
    else{
        while(r.start != r.end){
            if(*r.start != *l.start)return *l.start - *r.start;
            l.start++;
            r.start++;
        }
        if(l.start == l.end) return 0;
        return 1;
    }
}

static inline int string_cmpz(string l, char* r){
    while (true){
        if(*r == 0){
            if(l.start == l.end) return 0;
            return 1;
        }
        if(l.start == l.end) return -1;
        if(*l.start != *r) return *l.start - *r;
        l.start++;
        r++;
    }
}

static inline char* string_to_cstr(string s){
    //this is temporary until we get rid of c strings entirely
    ureg path_len = string_len(s);
    char* path = malloc(path_len + 1);
    if(!path) return NULL;
    memcpy(path, s.start, path_len);
    path[path_len] = '\0';
    return path;
}

static inline void string_free_cstr(char* s){
    free(s);
}
