#ifndef TAUC_UTILS_STRING_H
#define TAUC_UTILS_STRING_H

#include "stdio.h"
#include "types.h"
#include "utils/allocator.h"
#include <stdio.h>
#include <string.h>

typedef struct string_s {
    char* start;
    char* end;
} string;

static inline string string_empty()
{
    string s;
    s.start = NULL;
    s.end = NULL;
    return s;
}
static inline bool string_is_empty(string s)
{
    return (s.start == s.end);
}
static inline void string_set(string* s, char* start, char* end)
{
    s->start = start;
    s->end = end;
}

static inline void string_set_cstr(string* s, char* zstring)
{
    s->start = zstring;
    s->end = zstring + strlen(zstring);
}

static inline string string_from_cstr(char* s)
{
    string str;
    string_set_cstr(&str, s);
    return str;
}

static inline string string_create(char* start, char* end)
{
    string s;
    string_set(&s, start, end);
    return s;
}

static inline ureg string_len(string s)
{
    return s.end - s.start;
}

static inline void string_print(string s)
{
    fwrite(s.start, 1, string_len(s), stdout);
}
static inline void string_print_stderr(string s)
{
    fwrite(s.start, 1, string_len(s), stderr);
}

// TODO: try to make use of the really good standard
// string compare functions for superior performance
static inline int string_cmp(string l, string r)
{
    if (string_len(l) < string_len(r)) {
        while (l.start != l.end) {
            if (*l.start != *r.start) return *l.start - *r.start;
            r.start++;
            l.start++;
        }
        return -1;
    }
    while (r.start != r.end) {
        if (*r.start != *l.start) return *l.start - *r.start;
        l.start++;
        r.start++;
    }
    if (l.start == l.end) return 0;
    return 1;
}

static inline int string_cmp_cstr(string l, char* r)
{
    while (true) {
        if (*r == 0) {
            if (l.start == l.end) return 0;
            return 1;
        }
        if (l.start == l.end) return -1;
        if (*l.start != *r) return *l.start - *r;
        l.start++;
        r++;
    }
}

static inline bool string_eq(string l, string r)
{
    ureg len = string_len(l);
    if (len != string_len(r)) return false;
    return !memcmp(l.start, r.start, len);
}

static inline bool string_eq_cstr(string l, const char* r)
{
    while (l.start != l.end) {
        if (*l.start != *r) return false;
        l.start++;
        r++;
    }
    if (*r != '\0') return false;
    return true;
}

static inline char* string_to_cstr(string s)
{
    // this is temporary until we get rid of c strings entirely
    ureg path_len = string_len(s);
    char* path = (char*)tmalloc(path_len + 1);
    if (!path) return NULL;
    memcpy(path, s.start, path_len);
    path[path_len] = '\0';
    return path;
}

static inline void string_free_cstr(char* s)
{
    tfree(s);
}

static inline bool is_utf8_continuation(char c)
{
    return ((c & 0xC0) == 0x80);
}
static inline bool is_utf8_head(char c)
{
    return (((unsigned char)c & (unsigned char)0xC0) == (unsigned char)0xC0);
}
static inline int get_utf8_seq_len_from_head(char c)
{
    int r = 1;
    if ((unsigned char)c > 127) c <<= 1;
    ;
    while ((unsigned char)c > 127) {
        c <<= 1;
        r++;
    }
    return r;
}

#endif