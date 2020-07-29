#!/bin/bash
cd "$(dirname "$(readlink -f "$0")")/error_tests" #change to directory of ci_tests
TAUC="../../build/tauc --ok-on-error"
errors=0
success=0
for taufile in *.tau ; do
    [ -e "$taufile" ] || continue
    time_before="$(date +%s.%3N)"
    
    ok=false
    $TAUC "$taufile" 2>&1 | grep -q error && ok=true

    if [ ! ${PIPESTATUS[0]} -eq 0 ]; then
        printf "\033[0;31mFAILED $taufile: compilation crashed\033[0m\n"
    elif $err_found; then
        time_after="$(date +%s.%3N)"
        time=$(bc <<< "( $time_after - $time_before ) * 1000 / 1")
        if [ $time -gt 1000 ]; then
            time=$(bc <<< "scale=2; ( $time_after - $time_before )  / 1")
            time="$time s"
        else
            time="$time ms"
        fi
        printf "PASSED $taufile [$time]\033[0m\n"
    else
        printf "\033[0;31mFAILED $taufile: unwanted success\033[0m\n"
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


if [[ $errors -eq 0 ]]; then
    if [ $# != 1 ] || [ "$1" != "-q" ]; then
        printf "\033[0;32mall $success error test(s) passed\033[0m\n"
    fi
    exit 0
else
    printf "\033[0;32m$success error test(s) passed\033[0;31m but $errors test(s) failed\033[0m\n"
    exit 1
fi
