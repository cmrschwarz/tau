set(CLANG_VERSION_MAJOR 10)
set(CLANG_VERSION_MINOR 0)

if(${TAU_LLVM_PRECOMPILED})
    set(CLANG_INCLUDE_DIRS 
        "${TAU_LLVM_DIR}/clang/include"
        "${TAU_LLVM_PRECOMPILE_DIR}/include"
    )
    foreach(dirname ${CLANG_INCLUDE_DIRS})
        if (NOT EXISTS "${dirname}")
            message(FATAL_ERROR "missing clang include directory ${dirname}")
        endif()
    endforeach()
else()
    find_path(CLANG_INCLUDE_DIRS NAMES clang/Frontend/ASTUnit.h REQUIRED
        PATHS
            "/usr/lib/llvm/${CLANG_VERSION_MAJOR}/include"
            "/usr/lib/llvm-${CLANG_VERSION_MAJOR}/include"
            "/usr/lib/llvm-${CLANG_VERSION_MAJOR}.${CLANG_VERSION_MINOR}/include"
            "/usr/local/llvm${CLANG_VERSION_MAJOR}${CLANG_VERSION_MINOR}/include"
            "/usr/local/llvm${CLANG_VERSION_MAJOR}/include"
            "/mingw64/include"
            "/c/msys64/mingw64/include"
            "c:\\msys64\\mingw64\\include"
    )
endif()

set (CLANG_LIBRARY_NAMES 
    "clang-cpp-${CLANG_VERSION_MAJOR}.${CLANG_VERSION_MINOR}"
    "clang-cpp-${CLANG_VERSION_MAJOR}${CLANG_VERSION_MINOR}"
    "clang-cpp"
)
if(${TAU_LLVM_PRECOMPILED})
    find_library(CLANG_LIBRARY 
        NAMES ${CLANG_LIBRARY_NAMES}
        REQUIRED
        NO_DEFAULT_PATH
        PATHS "${TAU_LLVM_PRECOMPILE_DIR}/lib"
    )
else()
    find_library(CLANG_LIBRARY 
        NAMES ${CLANG_LIBRARY_NAMES}
        REQUIRED
        PATHS   
            "/usr/lib/llvm/${CLANG_VERSION_MAJOR}/lib"
            "/usr/lib/llvm/${CLANG_VERSION_MAJOR}/lib64"
            "/usr/lib/llvm-${CLANG_VERSION_MAJOR}/lib"
            "/usr/local/llvm${CLANG_VERSION_MAJOR}${CLANG_VERSION_MINOR}/lib"
            "/usr/local/llvm${CLANG_VERSION_MAJOR}/lib"
    )
endif()

if(${TAU_LLVM_DYNAMIC} AND EXISTS ${CLANG_LIBRARY})
    set(CLANG_LIBS ${CLANG_LIBRARY})
else()
    foreach(libname ${TAU_CLANG_COMPONENTS})
        string(TOUPPER "CLANG_${libname}_LIB" lib_tgt_name)
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
                    "/usr/lib/llvm/${CLANG_VERSION_MAJOR}/lib"
                    "/usr/lib/llvm-${CLANG_VERSION_MAJOR}/lib"
                    "/usr/lib/llvm-${CLANG_VERSION_MAJOR}.${CLANG_VERSION_MINOR}/lib"
                    "/usr/local/llvm${CLANG_VERSION_MAJOR}${CLANG_VERSION_MINOR}/lib"
                    "/usr/local/llvm${CLANG_VERSION_MAJOR}/lib"
                    "/mingw64/lib"
                    "/c/msys64/mingw64/lib"
                    "c:\\msys64\\mingw64\\lib"
            )
        endif()
        set(CLANG_LIBS ${CLANG_LIBS} ${${lib_tgt_name}})
    endforeach()
endif()

set(CLANG_DEFINITIONS "")
