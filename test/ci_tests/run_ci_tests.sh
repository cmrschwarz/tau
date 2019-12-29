#!/bin/bash
cd "$(dirname "$(readlink -f "$0")")" #change to directory of the shell script
TAUC="../../build/tauc"
errors=0
success=0
for taufile in *.tau ; do
    [ -e "$taufile" ] || continue
    printf "\033[0;33mcompiling $taufile\033[0m\n"
    ok=true
    $TAUC "$taufile" || ok=false
    if $ok; then
        printf "\033[0;32mcompiling $taufile succeeded\033[0m\n"
        if grep -q "main().->.int" "$taufile"; then
            printf "\033[0;33mrunning $taufile\033[0m\n"
            ./a.out || ok=false
            if $ok; then
                printf "\033[0;32mrunning $taufile succeeded\033[0m\n"
            else
                printf "\033[0;31mrunning $taufile failed\033[0m\n"
            fi
            rm a.out
        else
            printf "\033[0;33mskipped running $taufile because of main format\033[0m\n"
        fi
    else
        printf "\033[0;31mcompiling $taufile failed\033[0m\n"
        ok=false
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
    printf "\033[0;31m$errors test(s) failed, $success test(s) passed\n"
fi

for output in *.out *.ll *.asm *.obj; do 
    [ -e "$output" ] || continue
    rm $output
done
