#!/bin/bash
cd "$(dirname "$(readlink -f "$0")")/ci_tests" #change to directory of ci_tests
TAUC="../../build/tauc"
errors=0
success=0
for taufile in *.tau ; do
    [ -e "$taufile" ] || continue
    time_before="$(date +%s.%3N)"
    ok=true
    $TAUC "$taufile" || ok=false
    if $ok; then
        if grep -q "main().->.int" "$taufile"; then
            #printf "\033[0;33mrunning $taufile\033[0m\n"
            ./a.out
            res=$?
            if [ $res -eq 0 ]; then
                time_after="$(date +%s.%3N)"
                time=$(bc <<< "( $time_after - $time_before ) * 1000 / 1")
                if [ $time -gt 1000 ]; then
                    time=$(bc <<< "scale=2; ( $time_after - $time_before )  / 1")
                    time="$time s"
                else
                    time="$time ms"
                fi
                printf "\033[0;32m$taufile succeeded [$time] \033[0m\n"
            else
                printf "\033[0;32mcompiling $taufile succeeded\033[0;31m but the run returned $res!\033[0m\n"
                ok=false
            fi
            rm a.out
        else
            printf "\033[0;32mcompiling $taufile succeeded\033[0m\n"
            printf "\033[0;33mskipped running $taufile because of main format\033[0m\n"
        fi
    else
        printf "\033[0;31mcompiling $taufile failed\033[0m\n"
    fi
    if $ok; then
        success=$(($success + 1))
    else 
        errors=$(($errors + 1))
    fi
done

if [[ $errors -eq 0 ]]; then
    printf "\033[0;32mall $success test(s) passed\n"
else
    printf "\033[0;32m$success test(s) passed\033[0;31m but $errors test(s) failed\n"
fi

for output in *.out *.ll *.asm *.obj; do 
    [ -e "$output" ] || continue
    rm $output
done
