cmake_minimum_required(VERSION 3.4.3)
project(tau C CXX)

set(TAUC_C_SRCS
    ./src/lexer.c
    ./src/ast_flags.c
    ./src/parser.c
    ./src/resolver.c
    ./src/symbol_lookup.c
    ./src/post_resolution_pass.c
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
    ./src/utils/list.c
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
    ./src/ptr_map.c
    ./src/scc_detector.c
    ./src/generic_instance_resolution.c
    ./src/mdght.c
    ./src/file_map.c
    ./src/job_queue.c
    ./src/utils/debug_utils.c
    ./src/utils/os/linux/plattform.c
    ./src/target_platform.c
)
set (TAUC_CPP_SRCS
    ./src/llvm_backend.cpp
)
set(TAUC_INCLUDE_DIRS
    "${CMAKE_CURRENT_LIST_DIR}/src"
)

set(TAU_LLVM_PRECOMPILED ON CACHE BOOL "")

add_subdirectory("${CMAKE_CURRENT_LIST_DIR}/deps")


if (CMAKE_BUILD_TYPE STREQUAL "Debug")
    message("Creating Debug Buildfiles")
    set(TAUC_C_TESTS
        ./src/test/unit_tests.c
    )
    set(TAUC_CPP_TESTS)
    set(TAUC_C_SRCS ${TAUC_C_SRCS} ${TAUC_C_TESTS})
    set(TAUC_CPP_SRCS ${TAUC_CPP_SRCS} ${TAUC_CPP_TESTS})
    set(TAUC_PP_DIRECTIVES ${TAUC_PP_DIRECTIVES} TAUC_DEBUG)
else()
    message("Creating Release Buildfiles")
endif()


if ("${CMAKE_CXX_COMPILER_ID}" STREQUAL "Clang" OR 
    "${CMAKE_CXX_COMPILER_ID}" STREQUAL "GNU"
)
    # add "-pg -no-pie" for profiling with gprof
    set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -Wall -Wextra -Wno-unused-parameter -Wno-maybe-uninitialized -pedantic")
    set(CMAKE_C_FLAGS_DEBUG "${CMAKE_C_FLAGS_DEBUG} -g")
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wall -pedantic -fno-rtti -fno-exceptions")
    set(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} -g")
endif ()

add_executable(tauc ${TAUC_C_SRCS} ${TAUC_CPP_SRCS})


target_compile_definitions(tauc PRIVATE 
    ${TAUC_PP_DIRECTIVES}
    ${TAU_DEPS_DEFINITIONS}
)
target_include_directories(tauc PUBLIC 
    ${TAUC_INCLUDE_DIRS}
    ${TAU_DEPS_INCLUDE_DIRS}
)
target_link_libraries(tauc LINK_PUBLIC
    ${TAU_DEPS_LIBS}
    #TODO: handle TAU_DEPS_EXTERNAL_LIBS
)

add_dependencies(tauc ${TAU_DEPS_DEPS})
