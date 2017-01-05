#!/bin/bash
# set up three panes; emacs, erl, bash.
# change layout depending on how wide the terminal is.

# make sure we have exactly one session
SESSION=masse

# if there is no session, start it.
# start emacs in pane 1, erl in pane 2, bash in pane 3
if $(tmux has-session -t =$SESSION); then
    echo "session exists."
else
    tmux new-session -d -s $SESSION
    tmux send-keys -t 1 "emacs" C-m
    tmux split-window
    tmux send-keys -t 2 "erl -sname erl@localhost -pa ~/git/eper/ebin" C-m
    tmux split-window
    sleep 4
fi

#width
W=$(tmux list-sessions | grep "$SESSION:" | cut -f2 -d"[" | cut -f1 -d"x")
[ $W -lt 240 ] && narrowp="t" || narrowp=""

# move emacs, if it exists, to pane 1 (narrrow) or pane 2 (wide)
# set emacs pane width to 81
panes=$(tmux list-panes -F "#P:#{pane_current_command}\n")
emacspane=$(echo $panes | grep -Eo "[0-9]*:emacs" | cut -f1 -d":" | head -1)
if [ -n "$narrowp" ]; then
    [ -n "$emacspane" ] && tmux swap-pane -s $emacspane -t 1
    tmux -q set-window-option main-pane-width 81  > /dev/null
    tmux -q select-layout main-vertical > /dev/null
else
    [ -n "$emacspane" ] && tmux swap-pane -s $emacspane -t 2
    tmux -q select-layout even-horizontal > /dev/null
    tmux -q resize-pane -t 2 -x 81 > /dev/null
fi

# select a bash window, if there is one
panes=$(tmux list-panes -F "#P:#{pane_current_command}\n")
bashpane=$(echo $panes | grep -Eo "[0-9]*:bash" | cut -f1 -d":" | head -1)
[ -n $bashpane ] && tmux select-pane -t $bashpane

# maybe attach
Q=$(tmux list-sessions | grep "$SESSION:" | grep "(attached)")
[ -z "$Q" ] && tmux attach -t $SESSION
