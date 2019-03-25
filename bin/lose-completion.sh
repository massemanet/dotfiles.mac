#!/bin/bash

declare -a l1=("list" "type" "date" "size")

lose_completions() {
    if [ "$COMP_CWORD" = "1" ]
    then COMPREPLY=($(compgen -W "${l1[*]}" "${COMP_WORDS[1]}"))
    else COMPREPLY=($(compgen -W "$(ls)" "${COMP_WORDS[2]}"))
    fi
}

complete -F lose_completions lose
