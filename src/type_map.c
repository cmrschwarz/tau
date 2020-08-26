#include "type_map.h"
#include "ast.h"

int type_map_init(type_map* tm)
{
    int r = rwlock_init(&tm->lock);
    if (r) return r;
    tm->map = (ast_elem**)NULL_PTR_PTR;
    tm->capactiy = 0;
    tm->count = 0;
    return ERR;
}
void type_map_fin(type_map* tm)
{
}

type_array* type_map_get_array(type_map* tm, ureg elem_count)
{
}
type_tuple* type_map_get_tuple_follower(type_map* tm, ast_elem* following_type)
{
}