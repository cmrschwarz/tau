#include "resolver.h"
#include "thread_context.h"
#include "utils/error.h"
int resolver_init(resolver* r, thread_context* tc)
{
    r->tc = tc;
    return OK;
}
void resolver_fin(resolver* r)
{
}
int resolver_resolve_multiple(resolver* r, mdg_node** start, mdg_node** end)
{
    r->start = start;
    r->end = end;
    // TODO: resolve
    printf("resolving {");
    mdg_node** i = start;
    while (i + 1 != end) {
        printf("%s, ", (**i).name);
        i++;
    }
    printf("%s}\n", (**i).name);
    return mdg_nodes_resolved(start, end, r->tc);
}

int resolver_resolve_single(resolver* r, mdg_node* node)
{
    // TODO: resolve
    printf("resolving %s\n", node->name);
    return mdg_node_resolved(node, r->tc);
}
