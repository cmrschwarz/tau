cmake_minimum_required(VERSION 3.7)
project(tau C)

set(TAUC_SRCS
    ./src/tokenizer.c
    ./src/parser.c
    ./src/keywords.c
    ./src/utils/dbuffer.c
    ./src/utils/panic.c
    ./src/utils/os/libc/atomics_libc.c
    ./src/utils/os/libc/allocator_libc.c
    ./src/utils/os/linux/threading_linux.c
    ./src/utils/os/linux/allocator_linux_caching.c
    ./src/utils/os/linux/allocator_linux.c
    ./src/utils/os/linux/timing_linux.c
    ./src/utils/timing_common.c
    ./src/utils/pool.c
    ./src/utils/hms.c
    ./src/utils/sbuffer.c
    ./src/main.c
    ./src/tauc.c
    ./src/token.c
    ./src/src_map.c
    ./src/error_log.c
    ./src/print_ast.c
    ./src/utils/list_builder.c
    ./src/utils/zero.c
    ./src/ast.c
    ./src/utils/evmap2.c
    ./src/src_file.c
)
add_executable(tauc ${TAUC_SRCS})
target_include_directories(tauc PUBLIC "./src" "./deps")
# add "-pg -no-pie" for debugging with gprof
# add "-O3 -flto" for release
set(CMAKE_C_FLAGS ${CMAKE_C_FLAGS} "-pthread -lm -Wall -pedantic")
target_compile_options(tauc PUBLIC)
