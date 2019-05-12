cmake_minimum_required(VERSION 3.7)
project(tau C)

set(TAUC_SRCS
    ./src/tokenizer.c
    ./src/parser.c
    ./src/resolver.c
    ./src/symbol_table.c
    ./src/utils/dbuffer.c
    ./src/utils/panic.c
    ./src/utils/os/libc/atomics_libc.c
    ./src/utils/os/libc/allocator_libc.c
    ./src/utils/os/linux/threading_linux.c
    ./src/utils/os/libc/allocator_libc.c
    ./src/utils/os/linux/timing_linux.c
    ./src/utils/timing_common.c
    ./src/utils/pool.c
    ./src/utils/hms.c
    ./src/utils/sbuffer.c
    ./src/main.c
    ./src/tauc.c
    ./src/thread_context.c
    ./src/token.c
    ./src/src_map.c
    ./src/error_log.c
    ./src/print_ast.c
    ./src/utils/list_builder.c
    ./src/utils/zero.c
    ./src/ast.c
    ./src/utils/evmap2.c
    ./src/mdg.c
    ./src/mdght.c
    ./src/file_map.c
    ./src/job_queue.c
    ./src/utils/os/linux/plattform.c
)

set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -pthread -lm -Wall -pedantic")
set(TAUC_PP_DIRECTIVES "")
if (CMAKE_BUILD_TYPE STREQUAL "Debug")
    # add "-pg -no-pie" for profiling with gprof
    message("Creating Debug Buildfiles")
    set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -O0 -g")
    set(TAUC_SRCS ${TAUC_SRCS} ./test/main_test.c)
else()
    message("Creating Release Buildfiles")
    set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -O3 -flto")
    set(TAUC_PP_DIRECTIVES ${TAUC_PP_DIRECTIVES} CMAKE_NO_DEBUG)
endif()

add_executable(tauc ${TAUC_SRCS})
target_include_directories(tauc PUBLIC "./src" "./deps")
target_compile_definitions(tauc PRIVATE ${TAUC_PP_DIRECTIVES})
target_compile_options(tauc PUBLIC)
