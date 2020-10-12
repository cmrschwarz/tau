cmake_minimum_required(VERSION 3.4.3)
# exports: TAU_DEPS_INCLUDE_DIRS, TAU_DEPS_DEFINITIONS,
#          TAU_DEPS_LIBS, TAU_DEPS_TARGETS

# options: TAU_LLVM_PRECOMPILE, [TAU_LLVM_PRECOMPILED_LIB_PATHES,
#          TAU_LLVM_INCLUDE_DIRS, TAU_LLVM_DEFINITIONS]


# LLVM
###############################################################################

set(TAU_LLVM_DIR "${CMAKE_CURRENT_LIST_DIR}/llvm-project"
    CACHE STRING 
    "location of llvm"
)

option(TAU_LLVM_PRECOMPILE
    "compile custom version of llvm" 
    OFF
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


if(${TAU_LLVM_PRECOMPILE})
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
    

    #enable LLD, disable other projects
    set(LLVM_ENABLE_PROJECTS lld CACHE STRING "" FORCE)

    #disable DLL build
    set(LLVM_BUILD_LLVM_DYLIB OFF CACHE BOOL "" FORCE)

    #makes things go faster apparently, docs unclear
    #set(LLVM_OPTIMIZED_TABLEGEN ON CACHE BOOL "" FORCE)

    #force LLVM to build in Release Mode
    set(CMAKE_BUILD_TYPE_SAVED "${CMAKE_BUILD_TYPE}" CACHE STRING "" FORCE)
    set(CMAKE_BUILD_TYPE Release CACHE STRING "" FORCE)
    #include llvm
    add_subdirectory(
        "${CMAKE_CURRENT_LIST_DIR}/llvm-project/llvm"
        EXCLUDE_FROM_ALL
    )
    # restore build type, this is a veru ugly hack
    # TODO: find a better way to do this 
    set(CMAKE_BUILD_TYPE ${CMAKE_BUILD_TYPE_SAVED})
    set(CMAKE_BUILD_TYPE ${CMAKE_BUILD_TYPE_SAVED} PARENT_SCOPE)
    set(TAU_LLVM_INCLUDE_DIRS 
        "${CMAKE_CURRENT_LIST_DIR}/llvm-project/llvm/include"
        "${CMAKE_CURRENT_LIST_DIR}/llvm-project/lld/include"
        "${LLVM_BINARY_DIR}/include"
    )
    set(TAU_LLVM_DEFINITIONS ${LLVM_DEFINITIONS})
    llvm_map_components_to_libnames(TAU_LLVM_CORE_LIBS
       ${TAU_LLVM_COMPONENTS}
    )
    set(TAU_LLVM_LLD_LIBS ${TAU_LLD_COMPONENTS})
    set(TAU_LLVM_LIBS
        ${TAU_LLVM_CORE_LIBS}
        ${TAU_LLVM_LLD_LIBS}
    )
    set(TAU_LLVM_TARGETS
        llvm-config
    )
elseif(EXISTS "${TAU_LLVM_PRECOMPILE_DIR}")
    message("found precompiled llvm!")
    
    set(TAU_LLVM_CONFIG_DIR "${TAU_LLVM_PRECOMPILE_DIR}/llvm/bin" CACHE STRING 
        "directory of the llvm-config utility"
    )
    find_program(TAU_LLVM_CONFIG_EXE
        NAMES llvm-config
        PATHS ${TAU_LLVM_CONFIG_DIR}
    )
    link_directories(${TAU_LLVM_PRECOMPILED_LIBS_FOLDER})
    set(TAU_LLVM_INCLUDE_DIRS 
        "${CMAKE_CURRENT_LIST_DIR}/llvm-project/llvm/include"
        "${CMAKE_CURRENT_LIST_DIR}/llvm-project/lld/include"
        "${TAU_LLVM_PRECOMPILE_DIR}/llvm/include"
        "${TAU_LLVM_PRECOMPILE_DIR}/llvm/tools/lld/include"
        CACHE STRING "Include Directories for LLVM Headers"
    )
    set(TAU_LLVM_DEFINITIONS "" 
        CACHE STRING "Preprocessor Definitions for the precompiled LLVM"
    )
    execute_process(
        COMMAND sh -c "${TAU_LLVM_CONFIG_EXE} --libfiles --link-static | xargs du | sed -e '/^0\\s*/d' -e 's/^[0-9]*\\s*//'"
        OUTPUT_VARIABLE LLVM_LIBRARIES_SPACES
        OUTPUT_STRIP_TRAILING_WHITESPACE
    )
    string(REPLACE "\n" ";" TAU_LLVM_LIBRARIES "${LLVM_LIBRARIES_SPACES}")
    execute_process(
        COMMAND ${TAU_LLVM_CONFIG_EXE} --system-libs --link-static 
        OUTPUT_VARIABLE LLVM_SYSTEM_LIBS_SPACES
        OUTPUT_STRIP_TRAILING_WHITESPACE
    )
    string(REPLACE " " ";" TAU_LLVM_SYSTEM_LIBS "${LLVM_SYSTEM_LIBS_SPACES}")
    execute_process(
        COMMAND ${TAU_LLVM_CONFIG_EXE} --libdir --link-static 
        OUTPUT_VARIABLE LLVM_LIBDIRS_SPACES
        OUTPUT_STRIP_TRAILING_WHITESPACE
    )
    string(REPLACE " " ";" TAU_LLVM_LIBDIRS "${LLVM_LIBDIRS_SPACES}")
   
    link_directories("${TAU_LLVM_LIBDIRS}")
    macro(TAU_ADD_LLVM_LIB _libname_)
        string(TOUPPER "LLVM_${_libname_}_LIB" _prettylibname_)
        find_library(${_prettylibname_} NAMES ${_libname_}
            PATHS ${TAU_LLVM_LIBDIRS}
        )
        if(${_prettylibname_})
            set(TAU_LLVM_LIBS ${TAU_LLVM_LIBS} ${${_prettylibname_}}
        )
        else()
            message(SEND_ERROR "lib not found: ${_libname_}")
        endif()
    endmacro()
    foreach(lib ${TAU_LLD_COMPONENTS})
        TAU_ADD_LLVM_LIB(${lib})
    endforeach()
    set(TAU_LLVM_LIBS
        ${TAU_LLVM_LIBS}
        ${TAU_LLVM_LIBRARIES}
        ${TAU_LLVM_SYSTEM_LIBS}
    )
    set(TAU_LLVM_TARGETS "")
else()
    set (TAU_LLVM_COMPONENTS "${TAU_LLVM_COMPONENTS}" lto)
    include("${CMAKE_CURRENT_LIST_DIR}/cmake/findLLVM.cmake")
    include("${CMAKE_CURRENT_LIST_DIR}/cmake/findLLD.cmake")
    set(TAU_LLVM_INCLUDE_DIRS ${LLVM_INCLUDE_DIRS} ${LLD_INCLUDE_DIRS})
    set(TAU_LLVM_DEFINITIONS ${LLVM_DEFINITIONS} ${LLD_DEFINITIONS})
    set(TAU_LLVM_LIBS ${LLD_LIBRARIES} ${LLVM_LIBRARIES})
    set(TAU_LLVM_TARGETS "")
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
set(TAU_DEPS_TARGETS
    ${TAU_LLVM_TARGETS}
    PARENT_SCOPE
)
