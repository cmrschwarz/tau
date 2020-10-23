cmake_minimum_required(VERSION 3.4.3)
# exports: TAU_DEPS_INCLUDE_DIRS, TAU_DEPS_DEFINITIONS,
#          TAU_DEPS_LIBS, TAU_DEPS_TARGETS

# options: TAU_LLVM_PRECOMPILE, [TAU_LLVM_PRECOMPILED_LIB_PATHES,
#          TAU_LLVM_INCLUDE_DIRS, TAU_LLVM_DEFINITIONS]


# LLVM
###############################################################################

option(TAU_COMPILE_LLVM
    "compile custom version of llvm"
    OFF
)
option(TAU_LLVM_DYNAMIC
    "link llvm dynamically (linux only)"
    OFF
)
set(TAU_LLVM_DIR "${CMAKE_CURRENT_LIST_DIR}/llvm-project"
    CACHE STRING
    "location of llvm"
)
set(TAU_LLVM_PRECOMPILE_DIR "${CMAKE_CURRENT_LIST_DIR}/llvm-project-prebuild"
    CACHE STRING
    "location for custom llvm precompile"
)

set (TAU_LLVM_COMPONENTS
    core
    support
    target
    irreader
    bitreader
    bitwriter
    codegen
    analysis
    asmprinter
    asmparser
    ExecutionEngine
    OrcJIT
    "option"
    lto
    debuginfopdb
    debuginfodwarf
    windowsmanifest
    libdriver
)

set (TAU_LLD_COMPONENTS
    lldDriver
    lldMinGW
    lldELF
    lldCOFF
    lldMachO
    lldWasm
    lldReaderWriter
    lldCore
    lldYAML
    lldCommon
)

set (TAU_CLANG_COMPONENTS)

if(${TAU_COMPILE_LLVM})
    set(TAU_LLVM_TARGETS_TO_BUILD "X86;" CACHE BOOL
        "Semicolon separated list of target architectures to build and link"
    FORCE)
    set(LLVM_TARGETS_TO_BUILD ${TAU_LLVM_TARGETS_TO_BUILD} CACHE BOOL "" FORCE)
    #disable unneeded LLVM parts
    set(LLVM_BUILD_TOOLS OFF CACHE BOOL "" FORCE)
    #turning this off unfortunately disables lld's library targets
    #set(LLVM_INCLUDE_TOOLS OFF CACHE BOOL "" FORCE)
    set(LLVM_INCLUDE_BENCHMARKS OFF CACHE BOOL "" FORCE)
    set(LLVM_INCLUDE_TESTS OFF CACHE BOOL "" FORCE)
    set(LLVM_INCLUDE_EXAMPLES OFF CACHE BOOL "" FORCE)

    #enable exception handling (disabled, can't catch alloc anyways)
    #set(LLVM_ENABLE_EH ON CACHE BOOL "" FORCE)
    #set(LLVM_ENABLE_RTTI ON CACHE BOOL "" FORCE)


    # enable LLD disable other projects
    # (clang disabled for now)
    set(LLVM_ENABLE_PROJECTS "lld" CACHE STRING "" FORCE)

    #disable DLL build
    set(LLVM_BUILD_LLVM_DYLIB ${TAU_LLVM_DYNAMIC} CACHE BOOL "" FORCE)

    #makes things go faster apparently, docs unclear
    #set(LLVM_OPTIMIZED_TABLEGEN ON CACHE BOOL "" FORCE)

    #force LLVM to build in Release Mode
    set(CMAKE_BUILD_TYPE_SAVED "${CMAKE_BUILD_TYPE}" CACHE STRING "" FORCE)
    set(CMAKE_BUILD_TYPE Release CACHE STRING "" FORCE)
    #include llvm
    add_subdirectory(
        "${TAU_LLVM_DIR}/llvm"
        EXCLUDE_FROM_ALL
    )
    # restore build type, this is a veru ugly hack
    # TODO: find a better way to do this
    set(CMAKE_BUILD_TYPE ${CMAKE_BUILD_TYPE_SAVED})
    set(CMAKE_BUILD_TYPE ${CMAKE_BUILD_TYPE_SAVED} PARENT_SCOPE)
    set(TAU_LLVM_INCLUDE_DIRS
        "${TAU_LLVM_DIR}/llvm/include"
        "${TAU_LLVM_DIR}/lld/include"
        "${TAU_LLVM_DIR}/clang/include"
        "${LLVM_BINARY_DIR}/include"
    )
    set(TAU_LLVM_DEFINITIONS ${LLVM_DEFINITIONS})
    llvm_map_components_to_libnames(TAU_LLVM_CORE_LIBS
       ${TAU_LLVM_COMPONENTS}
    )
    set(TAU_LLVM_LIBS
        ${TAU_LLVM_CORE_LIBS}
        ${TAU_LLD_COMPONENTS}
        ${TAU_CLANG_COMPONENTS}
    )
    set(TAU_LLVM_TARGETS
        llvm-config
    )
else()
    if(EXISTS "${TAU_LLVM_PRECOMPILE_DIR}")
        message("found precompiled llvm!")
        set(TAU_LLVM_PRECOMPILED ON)
    else()
        set(TAU_LLVM_PRECOMPILED OFF)
    endif()

    include("${CMAKE_CURRENT_LIST_DIR}/cmake/findLLVM.cmake")
    include("${CMAKE_CURRENT_LIST_DIR}/cmake/findLLD.cmake")
    #include("${CMAKE_CURRENT_LIST_DIR}/cmake/findClang.cmake")
    set(TAU_LLVM_INCLUDE_DIRS
        ${LLVM_INCLUDE_DIRS}
        ${LLD_INCLUDE_DIRS}
        ${CLANG_INCLUDE_DIRS}
    )
    set(TAU_LLVM_DEFINITIONS
        ${LLVM_DEFINITIONS}
        ${LLD_DEFINITIONS}
        ${CLANG_DEFINITIONS}
    )
    set(TAU_LLVM_LIBS
        ${CLANG_LIBS}
        ${LLD_LIBS}
        ${LLVM_LIBS}
    )
endif()

# SORT_H
###############################################################################
set(TAU_SORT_H_INCLUDE_DIRS
    ${CMAKE_CURRENT_LIST_DIR}
)

# THREADS
###############################################################################
set(THREADS_PREFER_PTHREAD_FLAG OFF)
find_package(Threads REQUIRED)
set(TAU_THREADS_LIBS "${CMAKE_THREAD_LIBS_INIT}")


# Math
###############################################################################
if ("${CMAKE_CXX_COMPILER_ID}" STREQUAL "Clang" OR
    "${CMAKE_CXX_COMPILER_ID}" STREQUAL "GNU"
)
    set(TAU_MATH_LIBS "m")
endif ()


# Exports
###############################################################################

set(TAU_DEPS_INCLUDE_DIRS
    ${TAU_LLVM_INCLUDE_DIRS}
    ${TAU_SORT_H_INCLUDE_DIRS}
    PARENT_SCOPE
)
set(TAU_DEPS_DEFINITIONS
    ${TAU_LLVM_DEFINITIONS}
    PARENT_SCOPE
)
set(TAU_DEPS_LIBS
    ${TAU_LLVM_LIBS}
    ${TAU_THREADS_LIBS}
    ${TAU_MATH_LIBS}
    PARENT_SCOPE
)
set(TAU_DEPS_TARGETS ${TAU_LLVM_TARGETS} PARENT_SCOPE)
