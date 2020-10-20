set(LLD_VERSION_MAJOR 10)
set(LLD_VERSION_MINOR 0)


if(${TAU_LLVM_PRECOMPILED})
    set(LLD_INCLUDE_DIRS 
        "${TAU_LLVM_DIR}/lld/include"
        "${TAU_LLVM_PRECOMPILE_DIR}/include"
    )
    foreach(dirname ${LLD_INCLUDE_DIRS})
        if (NOT EXISTS "${dirname}")
            message(FATAL_ERROR "missing lld include directory ${dirname}")
        endif()
    endforeach()
else()
    find_path(LLD_INCLUDE_DIRS NAMES lld/Common/Driver.h REQUIRED
        PATHS
            "/usr/lib/llvm-${LLD_VERSION_MAJOR}/include"
            "/usr/local/llvm${LLD_VERSION_MAJOR}${LLD_VERSION_MINOR}/include"
            "/usr/local/llvm${LLD_VERSION_MAJOR}/include"
            "/mingw64/include"
            "/c/msys64/mingw64/include"
            "c:\\msys64\\mingw64\\include"
    )
endif()

set (LLD_LIBRARY_NAMES 
    "lld-${LLD_VERSION_MAJOR}.${LLD_VERSION_MINOR}"
    "lld${LLD_VERSION_MAJOR}${LLD_VERSION_MINOR}" 
    "lld"
)
if(${TAU_LLVM_PRECOMPILED})
    find_library(LLD_LIBRARY 
        NAMES ${LLD_LIBRARY_NAMES}
        REQUIRED
        NO_DEFAULT_PATH
        PATHS "${TAU_LLVM_PRECOMPILE_DIR}/lib"
    )
else()
    find_library(LLD_LIBRARY 
        NAMES ${LLD_LIBRARY_NAMES}
        REQUIRED
        PATHS   
            "${TAU_LLVM_PRECOMPILE_DIR}/lib"
            "/usr/lib/llvm-${LLD_VERSION_MAJOR}/lib"
            "/usr/local/llvm${LLD_VERSION_MAJOR}${LLD_VERSION_MINOR}/lib"
            "/usr/local/llvm${LLD_VERSION_MAJOR}/lib"
    )
endif()

if(${TAU_LLVM_DYNAMIC} AND EXISTS ${LLD_LIBRARY})
    set(LLD_LIBS ${LLD_LIBRARY})
else()
    foreach(libname ${TAU_LLD_COMPONENTS})
        string(TOUPPER "LLD_${libname}_LIB" lib_tgt_name)
        if(${TAU_LLVM_PRECOMPILED})
            find_library(${lib_tgt_name} 
                NAMES ${libname}
                REQUIRED
                NO_DEFAULT_PATH
                PATHS "${TAU_LLVM_PRECOMPILE_DIR}/lib"
            )
        else()
            find_library(${lib_tgt_name} 
                NAMES ${libname}
                REQUIRED
                PATHS
                    "/usr/lib/llvm-${LLD_VERSION_MAJOR}/lib"
                    "/usr/local/llvm${LLD_VERSION_MAJOR}${LLD_VERSION_MINOR}/lib"
                    "/usr/local/llvm${LLD_VERSION_MAJOR}/lib"
                    "/mingw64/lib"
                    "/c/msys64/mingw64/lib"
                    "c:/msys64/mingw64/lib"
            )
        endif()
        set(LLD_LIBS ${LLD_LIBS} ${${lib_tgt_name}})
    endforeach()
endif()

set(LLD_DEFINITIONS "")
