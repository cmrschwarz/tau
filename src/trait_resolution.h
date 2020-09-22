#pragma once
#include "resolver.h"
#include "trait_table.h"

resolve_error add_mf_trait_decls(resolver* r);
resolve_error unordered_body_add_trait_decls(resolver* r, ast_body* body);
resolve_error resolve_trait_impl(resolver* r, trait_impl* ti, ast_body* body);

resolve_error
ast_node_add_trait_decls(resolver* r, ast_body* body, ast_node* n);
