#!/bin/bash
cd "$(dirname "$(readlink -f "$0")")/ci_tests" #change to directory of ci_tests
errors=0
success=0
quiet=false
if [ $# -gt 0 ] && [ $1 = '-q' ]; then
    quiet=true
    shift
fi
TAUC="../../build/tauc $@"

for taufile in *.tau ; do
    [ -e "$taufile" ] || continue
    ok=true
    time_before="$(date +%s.%3N)"
    $TAUC "$taufile" || ok=false
    time_after="$(date +%s.%3N)"
    time=$(bc <<< "( $time_after - $time_before ) * 1000 / 1")
    if [ $time -gt 1000 ]; then
        time=$(bc <<< "scale=2; ( $time_after - $time_before )  / 1")
        time="$time s"
    else
        time="$time ms"
    fi
    if $ok; then
        if grep -q "main().->.int" "$taufile"; then
            #printf "\033[0;33mrunning $taufile\033[0m\n"
            ./a.out
            res=$?
            if [ $res -eq 0 ]; then
                printf "PASSED $taufile [$time]\n"
            else
                printf "\033[0;31mFAILED $taufile: compile succeeded but the run returned $res!\033[0m\n"
                ok=false
            fi
            rm a.out
        else
            printf "\033[0;32mPASSED $taufile [$time] (\033[0;33mskipped run because of main format\033)\033[0m\n"
        fi
    else
        printf "\033[0;31mFAILED $taufile: compilation error\033[0m\n"
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
    if [ $quiet == "false" ]; then
        printf "\033[0;32mall $success test(s) passed\033[0m\n"
    fi
    exit 0
else
    printf "\033[0;32m$success test(s) passed\033[0;31m but $errors test(s) failed\033[0m\n"
    exit 1
fi
