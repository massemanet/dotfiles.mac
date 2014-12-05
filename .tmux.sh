#!/bin/bash

nsession=$(2>/dev/null tmux list-session | wc -l)
if [ $nsession -gt 1 ]; then
    echo "many sessions"
    exit 0
elif [ $nsession -eq 0 ]; then
    tmux new-session -d
    tmux send-keys -t 0 "emacs" C-m
    tmux split-window
    tmux send-keys -t 1 "erl" C-m
    tmux split-window
fi

[ $(tput cols) -lt 240 ] && narrowp="t" || narrowp=""
if [ -n $narrowp ]; then
    tmux set-window-option main-pane-width 81
    tmux select-layout main-vertical
    tmux select-pane -t 0
else
    tmux select-layout even-vertical
    tmux resize-pane -t 1 -x 81
    tmux select-pane -t 1
fi

emacsen=$(tmux list-panes -F "#P:#{pane_current_command}" | grep emacs)
if [ $(echo $emacsen | wc -l) -eq 1 ]; then
    emacspane=$(echo $emacsen | cut -f1 -d":")
    if [ -n $narrowp ]; then
        tmux swap-pane -s $emacspane -t 0
    else
        tmux swap-pane -s $emacspane -t 1
    fi
fi

$(tmux list-session | grep -q attached) && attachedp="t" || attachedp=""
[ -z "$attachedp" ] && tmux attach && exit 0
