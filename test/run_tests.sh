#!/bin/bash
set -euo pipefail

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"
cd "$SCRIPT_DIR/../"

TAUC="./build/tauc"

#run unit tests
$TAUC -U
# test other output modes with at least one run :) 
$TAUC -L -S -E ./test/branchtest.t 
./a.out

$TAUC ./test/bigtest/footest.t
./a.out

$TAUC ./test/multimoduletest.t
./a.out

$TAUC ./test/structtest.t 
./a.out

rm ./a.out
rm ./_.ll
rm ./_.asm

printf "\033[0;32mall tests passed\n"
# - valgrind --track-origins=yes --leak-check=full --error-exitcode=1 --suppressions test/valgrind_suppressions$TAUC 
