#!/bin/bash
TESTS_DIR="$(dirname "$(readlink -f "$0")")/error_tests"
TESTS_REL="./tests/error_tests"
ROOT_DIR="$(dirname "$(readlink -f "$0")")/../"
ROOT_DIR="$(cd $ROOT_DIR && pwd -P)"
quiet=false
if [ $# -gt 0 ] && [ $1 = '-q' ]; then
    quiet=true
    shift
fi
TAUC="./build/tauc --ok-on-error $@"
cd "$TESTS_DIR" #change to directory of ci_tests

errors=0
success=0
tmp_file=`mktemp -p "$(pwd -P)"`
for taufile in *.tau ; do
    [ -e "$taufile" ] || continue
    errfile=`echo "$taufile" | sed "s/\.tau/.err/"`
    err_file_exists=`[ -f "$errfile" ] && echo true || echo false`
   
    ok=false
    crash=false
    cd "$ROOT_DIR"
    time_before="$(date +%s.%3N)"
    $TAUC "$TESTS_REL/$taufile" 2>"$tmp_file" || crash=true
    time_after="$(date +%s.%3N)"
    cd "$TESTS_DIR"
    if $crash; then
        printf "\033[0;31mFAILED $taufile: compilation crashed\033[0m\n"
    else
        if $err_file_exists; then
            cmp --silent "$errfile" "$tmp_file" && ok=true
        else
            grep -q 'error' "$tmp_file" && ok=true
        fi
        if $ok; then
            time=$(bc <<< "( $time_after - $time_before ) * 1000 / 1")
            if [ $time -gt 1000 ]; then
                time=$(bc <<< "scale=2; ( $time_after - $time_before )  / 1")
                time="$time s"
            else
                time="$time ms"
            fi
            if $err_file_exists; then
                printf "PASSED $taufile [$time]\033[0m\n"
            else
                printf "PASSED $taufile [$time] \033[0;33m(no .err)\033\033[0m\n"
            fi
        else
            printf "\033[0;31mFAILED $taufile: wrong output:\033[0m\n"
            cat "$tmp_file"
        fi
    fi
    if $ok; then
        success=$(($success + 1))
    else 
        errors=$(($errors + 1))
    fi
done

for output in *.out *.ll *.asm *.obj; do 
    [ -e "$output" ] || continue
    rm $output
done
rm "$tmp_file"


if [[ $errors -eq 0 ]]; then
    if [ "$quiet" == "false" ]; then
        printf "\033[0;32mall $success error test(s) passed\033[0m\n"
    fi
    exit 0
else
    printf "\033[0;32m$success error test(s) passed\033[0;31m but $errors test(s) failed\033[0m\n"
    exit 1
fi
