#!/bin/bash
set -euo pipefail

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"
cd "$SCRIPT_DIR/../"

TAUC="./build/tauc"

#run unit tests
$TAUC -U
# test other output modes with at least one run :) 
$TAUC -L -S -E ./test/branchtest.tau
./a.out

$TAUC ./test/bigtest/footest.tau
./a.out

$TAUC ./test/multimoduletest.tau
./a.out

$TAUC ./test/multimodulepptest.tau
./a.out

$TAUC ./test/structtest.tau 
./a.out

rm ./a.out
rm ./_.ll
rm ./_.asm

printf "\033[0;32mall tests passed\n"
# - valgrind --track-origins=yes --leak-check=full --error-exitcode=1 --suppressions test/valgrind_suppressions$TAUC 
