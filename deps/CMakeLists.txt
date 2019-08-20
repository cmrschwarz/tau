cmake_minimum_required(VERSION 3.4.3)
# exports: TAU_DEPS_INCLUDE_DIRS, TAU_DEPS_LIBS, TAU_DEPS_EXTERNAL_LIBS,
#          TAU_DEPS_C_FLAGS

# options: TAU_LLVM_PRECOMPILED, [TAU_LLVM_PRECOMPILED_LIB_PATHES,
#          TAU_LLVM_INCLUDE_DIRS, TAU_LLVM_DEFINITIONS]


# LLVM
###############################################################################
option(TAU_LLVM_PRECOMPILED 
    "use precompiled llvm libraries locaded in TAU_LLVM_LIB_PATHES" 
    OFF
)
set(TAU_LLVM_TARGETS_TO_BUILD "X86;" CACHE BOOL 
    "Semocion separated list of Target architectures to build and link" 
FORCE)
if(${TAU_LLVM_PRECOMPILED})
    message("precompiled!")
    set(TAU_LLVM_PROJECT_DIR "${CMAKE_CURRENT_LIST_DIR}/llvm-project-prebuild"
        CACHE STRING 
        "llvm location, used as a basis for default includes etc."
    )
    set(TAU_LLVM_CONFIG_DIR "${TAU_LLVM_PROJECT_DIR}/llvm/bin" CACHE STRING 
        "directory of the llvm-config utility"
    )
    find_program(TAU_LLVM_CONFIG_EXE
        NAMES llvm-config-8 llvm-config-8.0 llvm-config80 llvm-config
        PATHS ${TAU_LLVM_CONFIG_DIR}
    )
    link_directories(${TAU_LLVM_PRECOMPILED_LIBS_FOLDER})
    set(TAU_LLVM_INCLUDE_DIRS 
        "${CMAKE_CURRENT_LIST_DIR}/llvm-project/llvm/include"
        "${CMAKE_CURRENT_LIST_DIR}/llvm-project/lld/include"
        "${TAU_LLVM_PROJECT_DIR}/llvm/include"
        "${TAU_LLVM_PROJECT_DIR}/llvm/tools/lld/include"
        CACHE STRING 
        "Include Directories for LLVM Headers"
    )
    set(TAU_LLVM_DEFINITIONS 
        ""
        CACHE STRING 
        "Preprocessor Definitions for the precompiled LLVM"
    )
    execute_process(
        COMMAND ${TAU_LLVM_CONFIG_EXE} --libfiles --link-static
        OUTPUT_VARIABLE LLVM_LIBRARIES_SPACES
        OUTPUT_STRIP_TRAILING_WHITESPACE)
    string(REPLACE " " ";" TAU_LLVM_LIBRARIES "${LLVM_LIBRARIES_SPACES}")

    execute_process(
        COMMAND ${TAU_LLVM_CONFIG_EXE} --system-libs --link-static
        OUTPUT_VARIABLE LLVM_SYSTEM_LIBS_SPACES
        OUTPUT_STRIP_TRAILING_WHITESPACE)
    string(REPLACE " " ";" TAU_LLVM_SYSTEM_LIBS "${LLVM_SYSTEM_LIBS_SPACES}")

    execute_process(
        COMMAND ${TAU_LLVM_CONFIG_EXE} --libdir --link-static
        OUTPUT_VARIABLE LLVM_LIBDIRS_SPACES
        OUTPUT_STRIP_TRAILING_WHITESPACE)
    string(REPLACE " " ";" TAU_LLVM_LIBDIRS "${LLVM_LIBDIRS_SPACES}")
   
    link_directories("${TAU_LLVM_LIBDIRS}")
    macro(TAU_ADD_LLVM_LIB _libname_)
        string(TOUPPER "LLVM_${_libname_}_LIB" _prettylibname_)
        find_library(${_prettylibname_} NAMES ${_libname_}
            PATHS ${TAU_LLVM_LIBDIRS}
        )
        if(${_prettylibname_})
            set(TAU_LLVM_LIBS ${TAU_LLVM_LIBS} ${${_prettylibname_}})
        else()
            message("lib not found: ${_libname_}")
        endif()
    endmacro()
    set (TAU_LLVM_LLD_LIBS
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
    foreach(lib ${TAU_LLVM_LLD_LIBS})
        TAU_ADD_LLVM_LIB(${lib})
    endforeach()
    set(TAU_LLVM_LIBS
        ${TAU_LLVM_LIBS}
        ${TAU_LLVM_LIBRARIES}
        ${TAU_LLVM_SYSTEM_LIBS}
    )
   
   
else()
    option(TAU_LLVM_BUILD_TOOLS "build llvm tools (like llvm-config)" ON)
    set(LLVM_TARGETS_TO_BUILD ${TAU_LLVM_TARGETS_TO_BUILD} CACHE BOOL "" FORCE)
    #disable unneeded LLVM parts
    set(LLVM_BUILD_TOOLS ${TAU_LLVM_BUILD_TOOLS} CACHE BOOL "" FORCE)
    #turning this off unfortunately disables lld's library targets
    #set(LLVM_INCLUDE_TOOLS OFF CACHE BOOL "" FORCE)
    set(LLVM_INCLUDE_BENCHMARKS OFF CACHE BOOL "" FORCE)
    set(LLVM_INCLUDE_TESTS OFF CACHE BOOL "" FORCE)
    set(LLVM_INCLUDE_EXAMPLES OFF CACHE BOOL "" FORCE)

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
    if(${TAU_LLVM_BUILD_TOOLS})
        set(TAU_LLVM_EXCLUDE_FROM_ALL EXCLUDE_FROM_ALL)
    else()
        set(TAU_LLVM_EXCLUDE_FROM_ALL "")
    endif()
    add_subdirectory(
        "${CMAKE_CURRENT_LIST_DIR}/llvm-project/llvm"
        ${TAU_LLVM_EXCLUDE_FROM_ALL}
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
    set(TAU_LLVM_DEFINITIONS
        ${LLVM_DEFINITIONS}
    )
    llvm_map_components_to_libnames(LLVM_LIBS
        core
        support
        target
        irreader
        bitwriter
        codegen
        analysis
        asmprinter
        asmparser
        "option"
    )
    set(TAU_LLVM_MAIN_LIBS ${LLVM_LIBS})
    set(TAU_LLVM_LLD_LIBS
        lldCOFF
        lldDriver
        lldELF
        lldMinGW
        lldWasm
    )
    set(TAU_LLVM_LIBS
        ${TAU_LLVM_MAIN_LIBS}
        ${TAU_LLVM_LLD_LIBS}
    )
endif()

# SORT_H
###############################################################################
set(TAU_SORT_H_INCLUDE_DIRS
    ${CMAKE_CURRENT_LIST_DIR}
)

# THREADS
###############################################################################
find_package(Threads REQUIRED)
if(THREADS_HAVE_PTHREAD_ARG)
    set(TAU_THREADS_C_FLAGS "-pthread")
endif()
if(CMAKE_THREAD_LIBS_INIT)
    set(TAU_THREADS_LIBS "${CMAKE_THREAD_LIBS_INIT}")
endif()


# Math
###############################################################################
if ("${CMAKE_CXX_COMPILER_ID}" STREQUAL "Clang" OR 
    "${CMAKE_CXX_COMPILER_ID}" STREQUAL "GNU"
)
    set(TAU_MATH_LIBS "m")
endif ()


# Exports
###############################################################################

set(TAU_DEPS_C_FLAGS
    "${TAU_THREADS_C_FLAGS}"
    PARENT_SCOPE
)
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
set(TAU_DEPS_EXTERNAL_LIBS
    ${TAU_LLVM_PRECOMPILED_LIB_PATHES}
    PARENT_SCOPE
)
