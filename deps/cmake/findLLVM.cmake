set(LLVM_VERSION_MAJOR 10)
set(LLVM_VERSION_MINOR 0)

set (LLVM_CONFIG_NAMES
    "llvm-config"
    "llvm-config-${LLVM_VERSION_MAJOR}" 
    "llvm-config${LLVM_VERSION_MAJOR}"
    "llvm-config-${LLVM_VERSION_MAJOR}.${LLVM_VERSION_MINOR}"
    "llvm-config${LLVM_VERSION_MAJOR}${LLVM_VERSION_MINOR}"  
)

if(${TAU_LLVM_PRECOMPILED})
    find_program(LLVM_CONFIG_EXE
        NAMES ${LLVM_CONFIG_NAMES}
        REQUIRED
        PATHS "${TAU_LLVM_PRECOMPILE_DIR}/bin"
    )
    set(LLVM_INCLUDE_DIRS
        "${TAU_LLVM_DIR}/llvm/include"
        "${TAU_LLVM_PRECOMPILE_DIR}/include"
    )
else()
    find_package(LLVM ${LLVM_VERSION_MAJOR} QUIET CONFIG)
    find_program(LLVM_CONFIG_EXE
        NAMES ${LLVM_CONFIG_NAMES}
        REQUIRED
        PATHS 
            "${LLVM_TOOLS_BINARY_DIR}"
            "/usr/lib/llvm/${LLVM_VERSION_MAJOR}/bin"
            "/usr/lib/llvm-${LLVM_VERSION_MAJOR}/bin"
            "/usr/lib/llvm-${LLVM_VERSION_MAJOR}.${LLVM_VERSION_MINOR}/bin"
            "/usr/local/llvm${LLVM_VERSION_MAJOR}${LLVM_VERSION_MINOR}/bin"
            "/usr/local/llvm${LLVM_VERSION_MAJOR}/bin"
            "/mingw64/bin"
            "/c/msys64/mingw64/bin"
            "c:/msys64/mingw64/bin"
    )
    if(NOT ${LLVM_FOUND})
        set(LLVM_DEFINITIONS)
        execute_process(
            COMMAND "${LLVM_CONFIG_EXE}" ${LLVM_STATIC_LIBS_OPTION} --includedir 
            OUTPUT_VARIABLE LLVM_INCLUDE_DIRS
            OUTPUT_STRIP_TRAILING_WHITESPACE
        )
    endif()
endif()

if(${TAU_LLVM_DYNAMIC})
    set(LLVM_STATIC_LIBS_OPTION "--link-static ")
else()
    set(LLVM_STATIC_LIBS_OPTION "")
endif()

execute_process(
    COMMAND "${LLVM_CONFIG_EXE}" --targets-built
    OUTPUT_VARIABLE LLVM_TARGETS
    OUTPUT_STRIP_TRAILING_WHITESPACE
)
string(TOLOWER ${LLVM_TARGETS} LLVM_TARGETS)
string(REPLACE " " ";" LLVM_TARGETS "${LLVM_TARGETS}")
execute_process(
    COMMAND "${LLVM_CONFIG_EXE}" ${LLVM_STATIC_LIBS_OPTION} --system-libs
    OUTPUT_VARIABLE LLVM_SYSTEM_LIBS
    OUTPUT_STRIP_TRAILING_WHITESPACE
)
string(REPLACE " " ";" LLVM_SYSTEM_LIBS "${LLVM_SYSTEM_LIBS}")

execute_process(
    COMMAND "${LLVM_CONFIG_EXE}" --libdir ${LLVM_STATIC_LIBS_OPTION} 
    OUTPUT_VARIABLE LLVM_LIBDIRS
    OUTPUT_STRIP_TRAILING_WHITESPACE
)

if(${TAU_LLVM_DYNAMIC})
    find_library(LLVM_LIBS REQUIRED
        NAMES
            "LLVM-${LLVM_VERSION_MAJOR}.${LLVM_VERSION_MINOR}"
            "LLVM-${LLVM_VERSION_MAJOR}"
            "LLVM-${LLVM_VERSION_MAJOR}${LLVM_VERSION_MINOR}"
            "LLVM"
        REQUIRED
        PATHS ${LLVM_LIBDIRS}
    )
else()
    execute_process(
        COMMAND "${LLVM_CONFIG_EXE}" --link-static --libnames ${LLVM_TARGETS} ${TAU_LLVM_COMPONENTS}
        OUTPUT_VARIABLE LLVM_CORE_LIBS
        OUTPUT_STRIP_TRAILING_WHITESPACE
    )
    string(REPLACE " " ";" LLVM_CORE_LIBS "${LLVM_CORE_LIBS}")
    foreach(libname ${LLVM_CORE_LIBS})
        string(TOUPPER "LLVM_${libname}_LIB" lib_tgt_name)
        find_library(${lib_tgt_name} 
            NAMES "${libname}"
            REQUIRED
            PATHS "${LLVM_LIBDIRS}"
        )
        set(DUMMY false)
        if(${TAU_LLVM_PRECOMPILED})
            file(STRINGS "${${lib_tgt_name}}" FILE_TEXT LIMIT_INPUT 2)
            if("${FILE_TEXT}" STREQUAL "")
                set(DUMMY true)
            endif()
        endif()
        if(NOT ${DUMMY})
            set(LLVM_LIBS "${LLVM_LIBS}" "${${lib_tgt_name}}")
        endif()
    endforeach()
endif()

set(LLVM_LIBS
    ${LLVM_LIBS}
    ${LLVM_SYSTEM_LIBS}
)
