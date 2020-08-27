#!/bin/bash
set -euo pipefail

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"
cd "$SCRIPT_DIR/../"

quiet=false
if [ $# -gt 0 ] && [ $1 = '-q' ]; then
    quiet=true
    shift
fi
TAUC="./build/tauc"
errors=false
#messages
pok () {
    printf -- "-------------------------------------PASSED-------------------------------------\n"
}
pfail() {
    export errors=true
    printf -- "\033[0;31m-------------------------------------FAILED-------------------------------------\033[0m\n"
}

#run unit tests
($TAUC --run-unit-tests $@ && pok) || pfail 



#run ci tests
(./tests/run_ci_tests.sh -q $@ && pok) || pfail
(./tests/run_error_tests.sh -q $@ && pok) || pfail

if $errors; then
    printf "\033[0;31msome tests failed\033[0m\n" >&2
    exit 1
else
    if [ $quiet == "false" ]; then
        printf "\033[0;32mall tests passed\033[0m\n"
    fi
    exit 0
fi

# - valgrind --track-origins=yes --leak-check=full --error-exitcode=1 --suppressions test/valgrind_suppressions$TAUC 
