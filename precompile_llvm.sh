#!/bin/bash
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"
cd $SCRIPT_DIR

if [ -d "./deps/llvm-project-prebuild" ]; then
    exit 0
fi

mkdir prebuild
cd prebuild
cmake -DCMAKE_BUILD_TYPE=Release -DTAU_LLVM_PRECOMPILED:BOOL=OFF ../ || exit 1
make -j$(nproc) || exit 1
mv ./deps/llvm-project ../deps/llvm-project-prebuild
cd ..
rm -rf prebuild
cd ./deps/llvm-project-prebuild/llvm/bin/
./llvm-config --libs 2>&1 | sed -n "s/.*error: missing: //p" | xargs touch
exit 0
