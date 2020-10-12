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
    COMMAND ${LLVM_CONFIG_EXE} --targets-built
    OUTPUT_VARIABLE LLVM_TARGETS
    OUTPUT_STRIP_TRAILING_WHITESPACE
)
string(TOLOWER ${LLVM_TARGETS} LLVM_TARGETS)
string(REPLACE " " ";" LLVM_TARGETS "${LLVM_TARGETS}")
execute_process(
    COMMAND ${LLVM_CONFIG_EXE} --link-static --libfiles ${LLVM_TARGETS} ${TAU_LLVM_COMPONENTS}
    OUTPUT_VARIABLE LLVM_CORE_LIBRARIES
    OUTPUT_STRIP_TRAILING_WHITESPACE
)
string(REPLACE " " ";" LLVM_CORE_LIBRARIES "${LLVM_CORE_LIBRARIES}")

execute_process(
    COMMAND ${LLVM_CONFIG_EXE} --link-static --system-libs
    OUTPUT_VARIABLE LLVM_SYSTEM_LIBRARIES
    OUTPUT_STRIP_TRAILING_WHITESPACE
)
string(REPLACE " " ";" LLVM_SYSTEM_LIBRARIES "${LLVM_SYSTEM_LIBRARIES}")

foreach(libname ${LLVM_CORE_LIBRARIES})
    if(EXISTS ${libname})
        set(LLVM_LIBRARIES
            ${LLVM_LIBRARIES}
            ${libname}
        )
    endif()
endforeach()

set(LLVM_LIBRARIES
    ${LLVM_LIBRARIES}
    ${LLVM_SYSTEM_LIBRARIES}
)
