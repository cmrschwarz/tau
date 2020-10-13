set(LLVM_VERSION_MAJOR 10)
set(LLVM_VERSION_MINOR 0)

find_package(LLVM 10 REQUIRED CONFIG)
find_program(LLVM_CONFIG_EXE
    NAMES 
        "llvm-config"
        "llvm-config-${LLVM_VERSION_MAJOR}" 
        "llvm-config${LLVM_VERSION_MAJOR}"
        "llvm-config-${LLVM_VERSION_MAJOR}.${LLVM_VERSION_MINOR}"
        "llvm-config${LLVM_VERSION_MAJOR}${LLVM_VERSION_MINOR}"  
    PATHS 
        "${LLVM_TOOLS_BINARY_DIR}"
)
if (NOT EXISTS "${LLVM_CONFIG_EXE}")
    message(FATAL_ERROR "unable to find llvm-config")
endif()


execute_process(
    COMMAND "${LLVM_CONFIG_EXE}" --targets-built
    OUTPUT_VARIABLE LLVM_TARGETS
    OUTPUT_STRIP_TRAILING_WHITESPACE
)
string(TOLOWER ${LLVM_TARGETS} LLVM_TARGETS)
string(REPLACE " " ";" LLVM_TARGETS "${LLVM_TARGETS}")

execute_process(
    COMMAND "${LLVM_CONFIG_EXE}" --libdir --link-static 
    OUTPUT_VARIABLE LLVM_LIBDIRS
    OUTPUT_STRIP_TRAILING_WHITESPACE
)

execute_process(
    COMMAND "${LLVM_CONFIG_EXE}" --link-static --libnames ${LLVM_TARGETS} ${TAU_LLVM_COMPONENTS}
    OUTPUT_VARIABLE LLVM_CORE_LIBS
    OUTPUT_STRIP_TRAILING_WHITESPACE
)
string(REPLACE " " ";" LLVM_CORE_LIBS "${LLVM_CORE_LIBS}")

execute_process(
    COMMAND "${LLVM_CONFIG_EXE}" --link-static --system-libs
    OUTPUT_VARIABLE LLVM_SYSTEM_LIBS
    OUTPUT_STRIP_TRAILING_WHITESPACE
)
string(REPLACE " " ";" LLVM_SYSTEM_LIBS "${LLVM_SYSTEM_LIBS}")

foreach(libname ${LLVM_CORE_LIBS})
    string(TOUPPER "LLVM_${libname}_LIB" prettylibname)
    find_library(${prettylibname} 
        NAMES "${libname}"
        PATHS "${LLVM_LIBDIRS}"
    )
    if(EXISTS "${${prettylibname}}")
        set(LLVM_LIBS "${LLVM_LIBS}" "${${prettylibname}}")
    endif()
endforeach()

set(LLVM_LIBS
    ${LLVM_LIBS}
    ${LLVM_SYSTEM_LIBS}
)
