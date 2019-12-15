#!/bin/bash
set -euo pipefail

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"
cd "$SCRIPT_DIR/../"

TAUC="./build/tauc"

#run unit tests
$TAUC -U

#run ci tests
./test/ci_tests/run_ci_tests.sh

# - valgrind --track-origins=yes --leak-check=full --error-exitcode=1 --suppressions test/valgrind_suppressions$TAUC 
