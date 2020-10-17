#!powershell
$Config = "Debug"
$Arch = "x64"
$ScriptDir = Split-Path $script:MyInvocation.MyCommand.Path
cd $ScriptDir
git submodule update --init --recursive


#check for existing prebuild and delete it if it's too old
cd ./deps/llvm-project/
if ( Test-Path -Path "../llvm-project-prebuild" -PathType Container ) { 
    $curr_commit_id = git rev-parse HEAD
    $prebuild_commit_file = "../llvm-project-prebuild/prebuild_commit_id.txt"
    if ( (Test-Path -Path $prebuild_commit_file) -and ($curr_commit_id -ceq (Get-Content $prebuild_commit_file)) ) {
        echo "found existing prebuild"
        #exit 0
    }
    Remove-Item ../llvm-project-prebuild -Recurse
}
cd ../../

mkdir -p ./precompile_llvm > $null

cd ./precompile_llvm
cmake --config $Config -A $Arch -DTAU_COMPILE_LLVM:BOOL=ON ../
cmake --build . --config $Config
cd ..

# copy the resulting llvm libs to ./deps/llvm-project-prebuild 
$src = "./precompile_llvm/deps/llvm-project/llvm/"
$dest = "./deps/llvm-project-prebuild"
mkdir -p "$dest/lib"
cp "$src/$Config/lib/*.lib" "$dest/lib/"
cp -r "$src/$Config/bin" "$dest/bin"
cp -r "$src/include" "$dest/include"

# store the llvm commit id
cd ./deps/llvm-project
git rev-parse HEAD > ../llvm-project-prebuild/prebuild_commit_id.txt
cd ../..

# create empty files for the libs we don't need to stop llvm-config from complaining
# we later check for these during linking and ignore them
[string]$Output = Invoke-Expression "./deps/llvm-project-prebuild/bin/llvm-config.exe --libs 2>&1"

$Output.Split([Environment]::NewLine, [StringSplitOptions]::RemoveEmptyEntries) | ForEach-Object {
    if ("$_" -match ".*?error:\s*missing:\s*(.*lib)\s*$") {
        touch $Matches.1
    }
}
