#!/bin/bash
set -Eeuo pipefail
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"
cd $SCRIPT_DIR

keep_build=false
if [ $# -gt 0 ]; then
   if  [ "$1" = "-k" ] || [ "$1" = "--keep-build" ]; then
        keep_build=true
   fi
fi

# # make sure llvm is up to date
# git submodule update --init --recursive --remote
cd ./deps/llvm-project/

#if this dir exist we already precompiled
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
mkdir -p ./precompile_llvm
cd ./precompile_llvm

# compile tauc with llvm as a non precompiled dependency in ./precompile_llvm
cmake -DCMAKE_BUILD_TYPE=Release -DTAU_LLVM_PRECOMPILED:BOOL=OFF ../
make -j$(nproc) || :

# move the resulting llvm libs to ./deps/llvm-project-prebuild and delete the rest
if $keep_build; then
    cp -r ./deps/llvm-project ../deps/llvm-project-prebuild
    cd ..
else
    mv ./deps/llvm-project ../deps/llvm-project-prebuild
    cd ..
    rm -rf ./precompile_llvm
fi


#store the llvm commit id
cd ./deps/llvm-project
git rev-parse HEAD > ../llvm-project-prebuild/prebuild_commit_id.txt

# create empty files for the libs we don't need to stop llvm-config from complaining
# we later check for these during linking and ignore them
cd ./../llvm-project-prebuild/llvm/bin/
#since we are using the error output here we expect to get an error so we ignore the status code
./llvm-config --libs 2>&1 1>/dev/null | sed -n "s/.*error: missing: //p" | xargs -r touch || : 

# done
exit 0
