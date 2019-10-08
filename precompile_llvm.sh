#!/bin/bash
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"
cd $SCRIPT_DIR

#make sure llvm is up to date
cd ./deps/llvm-project/
git checkout release/9.x
git pull
#if this dir exists we already precompiled, so we exit successfully
if [ -d "../llvm-project-prebuild" ]; then
    
    #if we have the same commit id as during the prebuild exit successfully
    if [ "$(git rev-parse HEAD)" == "$(cat ../llvm-project-prebuild/prebuild_commit_id.txt 2>/dev/null || : )" ]; then
        echo "found existing prebuild"
        exit 0
    fi
    rm -rf ../llvm-project-prebuild
    rm -rf ../../precomile_llvm
fi
cd ../../
mkdir precompile_llvm
cd ./precompile_llvm

# compile tauc with llvm as a non precompiled dependency in ./precompile_llvm
cmake -DCMAKE_BUILD_TYPE=Release -DTAU_LLVM_PRECOMPILED:BOOL=OFF ../ || exit 1
make -j$(nproc) || exit 1

# move the resulting llvm libs to ./deps/llvm-project-prebuild and delete the rest
mv ./deps/llvm-project ../deps/llvm-project-prebuild
cd ..
rm -rf ./precompile_llvm

#store the llvm commit id
cd ./deps/llvm-project
git rev-parse HEAD > ../llvm-project-prebuild/prebuild_commit_id.txt

# create empty files for the libs we don't need to stop llvm-config from complaining
# we later check for these during linking and ignore them
cd ./../llvm-project-prebuild/llvm/bin/
./llvm-config --libs 2>&1 1>/dev/null | sed -n "s/.*error: missing: //p" | xargs touch

# done
exit 0
