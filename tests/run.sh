#!/bin/bash
set -euo pipefail

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"
cd "$SCRIPT_DIR/../"
ROOT_DIR="$(pwd -P)"
TAUC="$ROOT_DIR/build/tauc"
skip_tests=("__unreachable__")
only_tests=("")

pretty_print_time() {
    local before=$1
    local after=$2
    local time=$(bc <<< "( $after - $before ) * 1000 / 1")
    if [ $time -gt 1000 ]; then
        time=$(bc <<< "scale=2; ( $after - $before )  / 1")
        echo "$time s"
    else
        echo "$time ms"
    fi
}
print_seperator() {
    echo "--------------------------------------------------------------------------------"
}

print_err_out() {
    local message=$1

    print_seperator
    printf "\033[0;31m$message\033[0m"
    cat "$tmp_file"
    print_seperator
}

check_skip () {
    local taufile=$1
    local not_whitelisted=true
    for e in "${only_tests[@]}"; do 
        if [[ "$taufile" == *"$e"* ]];  then
            not_whitelisted=false
            break 
        fi
    done
    if $not_whitelisted; then
        return 0;
    fi
    for e in "${skip_tests[@]}"; do 
        if [[ "$taufile" == *"$e"* ]];  then
            echo "SKIPPED $taufile"
            return 0;
        fi
    done
    return 1
}

run_test_folder() {
    folder=$1
    out_ext=$2
    expect_err=$3
   
    error_count=0
    success_count=0
    if $expect_err; then
        TAUC_ARGS=("${TAUC_ARGS[@]}" "--ok-on-error")
    fi
    for taufile in "$folder"/*.tau ; do
        [ -e "$taufile" ] || continue
        if check_skip "$taufile"; then
            continue
        fi 
        expected_output_file=`echo "$taufile" | head  -c -4 && echo "$out_ext"`
        output_file_exists=`[ -f "$expected_output_file" ] && echo true || echo false`
        comp_crash=false
        ok=true

        txt="\033[0;33mCOMPILING $taufile\033[0m"
        printf "$txt"
        comp_time_before="$(date +%s.%3N)"
        "$TAUC" ${TAUC_ARGS[@]} "$taufile" >"$tmp_file" 2>&1 || comp_crash=true
        comp_time_after="$(date +%s.%3N)"
        printf "\r"
        printf "$txt" | sed "s/./ /g"
        printf "\r"

        if $comp_crash; then
            ok=false
            print_err_out "FAILED $taufile: compilation crashed:\n" 
        elif $expect_err; then
            if $output_file_exists; then
                if ! cmp --silent "$expected_output_file" "$tmp_file"; then 
                    ok=false
                    print_err_out "FAILED $taufile: wrong output:\n"
                fi
            else
                if ! grep -q 'error' "$tmp_file"; then
                    ok=false
                    print_err_out "FAILED $taufile: inconclusive error output:\n" >&2
                fi
            fi
        else
            if grep -q '.' "$tmp_file"; then
                #no output is good output
                ok=false
                print_err_out "FAILED $taufile: compiler printed:\n" >&2
            else
                txt="\033[0;33mRUNNING $taufile\033[0m"
                printf "$txt"
                run_time_before="$(date +%s.%3N)"
                "$ROOT_DIR/a.out" > "$tmp_file" 2>&1
                run_res=$?
                run_time_after="$(date +%s.%3N)"
                printf "\r"
                printf "$txt" | sed "s/./ /g"
                printf "\r"
                if ! [ $run_res -eq 0 ]; then
                    ok=false
                    print_err_out "FAILED $taufile: run returned $run_res:\n"
                else
                    if $output_file_exists; then
                        if ! cmp --silent "$expected_output_file" "$tmp_file"; then 
                            ok=false
                            print_err_out "FAILED $taufile: wrong run output:\n"
                        fi
                    else
                        if grep -q '.' "$tmp_file"; then
                            ok=false
                            print_err_out "FAILED $taufile: unexpected run output:\n" >&2
                        fi
                    fi
                fi
            fi
        fi
        if $ok; then
            comptime="$(pretty_print_time $comp_time_before $comp_time_after)" 
            if $expect_err; then
                if $output_file_exists || ! $expect_err; then
                    printf "PASSED $taufile [$comptime]\033[0m\n"
                else
                    printf "PASSED $taufile [$comptime] \033[0;33m(no $out_ext)\033\033[0m\n"
                fi
            else
                #runtime="$(pretty_print_time $run_time_before $run_time_after)"
                printf "PASSED $taufile [$comptime]\033[0m\n"
            fi
            success_count=$(($success_count + 1))
        else
            error_count=$(($error_count + 1))
        fi
    done
    "$SCRIPT_DIR/clean.sh"
    export errors=$(($errors + $error_count))
    export successes=$(($successes + $success_count))
    return $error_count
}

run_all=true
run_unit=false
run_regular=false
run_error=false
use_valgrind=false

while true; do
    if [ $# -eq 0 ]; then break; fi
    case $1 in
        -u|--run-unit-tests)
            run_all=false
            run_unit=true
            shift
        ;;
        -r|--run-regular-tests)
            run_all=false
            run_regular=true
            shift
        ;;
        -e|--run-error-tests)
            run_all=false
            run_error=true
            shift
        ;;
        -v|--valgrind)
            use_valgrind=true
            shift
        ;;
        -o|--only)
            shift
            if [[ "${only_tests[@]}" == "" ]]; then
                only_tests=()
            fi
            only_tests=("${only_tests[@]}" $1)
            shift
            run_all=false
            run_regular=true
            run_error=true
        ;;
        -s|--skip)
            shift
            skip_tests=("${skip_tests[@]}" $1)
            shift
        ;;
        --|*) 
            break
        ;;
    esac
done
export TAUC_ARGS="$@"

errors=0
successes=0
tmp_file="$(mktemp)"
if $use_valgrind; then
    TAUC_ARGS=("-q" "$TAUC" "${TAUC_ARGS[@]}")
    TAUC="valgrind"
fi
TAUC_ARGS="$TAUC_ARGS $@"

unit_tests_err=false
if $run_all || $run_unit; then
    "$TAUC" ${TAUC_ARGS[@]} --run-unit-tests || unit_tests_err=true
fi

if $run_all || $run_regular; then
    run_test_folder "./tests/regular" "out" false || :
fi

if $run_all || $run_error; then
    run_test_folder "./tests/errors" "err" true || :
fi
rm "$tmp_file"

if [ $errors -eq 0 ] && ! $unit_tests_err; then   
    printf "\033[0;32mall $successes tests passed\033[0m\n"
    exit 0
else
    printf "\033[0;31m"
    if $unit_tests_err; then
        printf "some unit tests failed," 
    fi
    printf "$errors tests failed and $successes tests passed"
    printf "\033[0m\n"
    exit 1
fi

# - valgrind --suppressions test/valgrind_suppressions$TAUC 
