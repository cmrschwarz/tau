#include "resolver.h"
#include "utils/error.h"
int resolver_init(resolver* r, thread_context* tc)
{
    r->tc = tc;
    return OK;
}
void resolver_fin(resolver* r)
{
}
resolve_error
resolver_resolve_multiple(resolver* r, mdg_node** start, mdg_node** end)
{
    r->start = start;
    r->end = end;
    // TODO: resolve
    puts("resolving");
    return RE_OK;
}

resolve_error resolver_resolve_single(resolver* r, mdg_node* node)
{
    // TODO: resolve
    puts("resolving");
    return RE_OK;
}
