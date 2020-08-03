#!/bin/bash
set -Eeuo pipefail
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"
cd $SCRIPT_DIR

keep_build=true
force_override=false
full_rerun=false
while [ $# -gt 0 ]; do
    if  [ "$1" = "-c" ] || [ "$1" = "--cleanup" ]; then
        keep_build=false
        shift
    elif  [ "$1" = "-f" ] || [ "$1" = "--force" ]; then
        force_override=true
        shift
    elif  [ "$1" = "-r" ] || [ "$1" = "--full" ]; then
        full_rerun=true
        shift
    fi
done

# make sure llvm is up to date
git submodule update --init --recursive
cd ./deps/llvm-project/

#if this dir exist we already precompiled
if [ -d "../llvm-project-prebuild" ]; then
    #if we have the same commit id as during the prebuild exit successfully
    if ! $force_override && [ "$(git rev-parse HEAD)" == "$(cat ../llvm-project-prebuild/prebuild_commit_id.txt 2>/dev/null || : )" ]; then
        echo "found existing prebuild"
        exit 0
    fi
    rm -rf ../llvm-project-prebuild
    if $full_rerun; then
        rm -rf ../../precomile_llvm
    fi
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
