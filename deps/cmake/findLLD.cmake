set(LLD_VERSION_MAJOR 10)
set(LLD_VERSION_MINOR 0)

find_path(LLD_INCLUDE_DIRS NAMES lld/Common/Driver.h
    PATHS
        "/usr/lib/llvm-${LLD_VERSION_MAJOR}/include"
        "/usr/local/llvm${LLD_VERSION_MAJOR}${LLD_VERSION_MINOR}/include"
        "/usr/local/llvm${LLD_VERSION_MAJOR}/include"
        "/mingw64/include"
)

if(NOT EXISTS "${LLD_INCLUDE_DIRS}")
    message(SEND_ERROR "failed to find LLD include directory")
endif()

find_library(LLD_LIBRARY 
    NAMES 
        "lld-${LLD_VERSION_MAJOR}.${LLD_VERSION_MINOR}"
        "lld${LLD_VERSION_MAJOR}${LLD_VERSION_MINOR}" 
        "lld"
    PATHS
        "/usr/lib/llvm-${LLD_VERSION_MAJOR}/lib"
        "/usr/local/llvm${LLD_VERSION_MAJOR}${LLD_VERSION_MINOR}/lib"
        "/usr/local/llvm${LLD_VERSION_MAJOR}/lib"
)
if(EXISTS ${LLD_LIBRARY})
    set(TAU_LLD_LIBS ${LLD_LIBRARY})
else()
    foreach(libname ${TAU_LLD_COMPONENTS})
        string(TOUPPER ${libname} lib_tgt_name)
        find_library(${lib_tgt_name} 
            NAMES 
                ${libname}
            PATHS
                ${LLD_LIBDIRS}
                "/usr/lib/llvm-${LLD_VERSION_MAJOR}/lib"
                "/usr/local/llvm${LLD_VERSION_MAJOR}${LLD_VERSION_MINOR}/lib"
                "/usr/local/llvm${LLD_VERSION_MAJOR}/lib"
                "/mingw64/lib"
                "/c/msys64/mingw64/lib"
                "c:/msys64/mingw64/lib"
        )
        if(EXISTS "${${lib_tgt_name}}")
            set(LLD_LIBS ${LLD_LIBS} ${${lib_tgt_name}})
        else()
            message(FATAL_ERROR "failed to find a LLD library: ${libname}")
        endif()
        unset(lib_found)
    endforeach()
endif()

set(TAU_LLD_LIBDIRS "")
set(TAU_LLD_DEFINITIONS "")
