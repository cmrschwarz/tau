#pragma once
#include "resolver.h"
#include "trait_table.h"

resolve_error resolve_mf_traits(resolver* r);
resolve_error resolve_body_traits(resolver* r, ast_body* body);
resolve_error resolve_trait_impl(resolver* r, trait_impl* ti, ast_body* body);

resolve_error
block_elem_resolve_traits(resolver* r, ast_body* body, ast_node* n);