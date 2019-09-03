#!/bin/bash
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"
cd $SCRIPT_DIR

#if this dir exists we already precompiled, so we exit successfully
if [ -d "./deps/llvm-project-prebuild" ]; then
    exit 0
fi

# compile tauc with llvm as a non precompled dependency in ./precompile_llvm
mkdir precompile_llvm
cd ./precompile_llvm
cmake -DCMAKE_BUILD_TYPE=Release -DTAU_LLVM_PRECOMPILED:BOOL=OFF ../ || exit 1
make -j$(nproc) || exit 1

# move the resulting llvm libs to ./deps/llvm-project-prebuild and delete the rest
mv ./deps/llvm-project ../deps/llvm-project-prebuild
cd ..
rm -rf ./precompile_llvm

# create empty files for the libs we don't need to stop llvm-config from complaining
# we later check for these during linking and ignore them
cd ./deps/llvm-project-prebuild/llvm/bin/
./llvm-config --libs 2>&1 1>/dev/null | sed -n "s/.*error: missing: //p" | xargs touch

# done
exit 0
