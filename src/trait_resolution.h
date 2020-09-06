#pragma once
#include "resolver.h"
#include "trait_table.h"

resolve_error resolve_mf_traits(resolver* r);
resolve_error resolve_body_traits(resolver* r, ast_body* body);