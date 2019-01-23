cmake_minimum_required(VERSION 3.7)
project(tau C)

set(TAUC_SRCS
    ./src/tokenizer.c
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
)
add_executable(tauc ${TAUC_SRCS})
target_include_directories(tauc PUBLIC "./src" "./deps")
set(CMAKE_C_FLAGS ${CMAKE_C_FLAGS} "-Wall -pthread -lm")
target_compile_options(tauc PUBLIC)



