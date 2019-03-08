#pragma once
#include "src_file.h"
#include "utils/rwslock.h"

typedef struct file_map {
    rwslock lock;
    // TODO: hash table mapping the filepath to a src_file
} file_map;