# -*- mode: shell-script -*-
# ~/.bashrc: executed by bash(1) for non-login shells.
#
# macos/homebrew style

[ -t 0 ] || exit 0

# clean up
unalias -a

# check for coreutils
GREP="$(which ggrep)"          || GREP="$(which grep)"
LS="$(which gls) --color=auto" || LS="$(which ls)"
DIRCOLORS="$(which gdircolors)"|| DIRCOLORS="$(which dircolors)"
eval "$($DIRCOLORS)"
[ -f /usr/local/etc/bash_completion ] && . /usr/local/etc/bash_completion

# define some git helpers
# shellcheck source=bin/gitfunctions
[ -f ~/bin/gitfunctions ] && . ~/bin/gitfunctions

# emacs
export EDITOR="emacsclient -a'' -c -t"

# PS1
if [ "$TERM" != "dumb" ]; then
    # set a fancy prompt
    export GIT_PS1_SHOWSTASHSTATE=true
    export GIT_PS1_SHOWUNTRACKEDFILES=true
    export GIT_PS1_SHOWDIRTYSTATE=true
    export PROMPT_COMMAND='LAST_EXIT=$? ; [ $LAST_EXIT == 0 ] && unset LAST_EXIT'
    export PS1='\[\e[33m\]\h'
    export PS1+='\[\e[36m\]${KUBECTX:+[${KUBECTX}]}'
    export PS1+='\[\e[35m\]($(mygitdir):$(mygitbranch))'
    export PS1+='\[\e[32m\]${LAST_EXIT:+\[\e[31m\]($LAST_EXIT)}$'
    export PS1+='\[\e[0m\] '
else
    export PS1="\\h\\$ "
fi

function grep()  { $GREP --color=auto "$@"; }
function dir()   { $LS -AlFh "$@"; }
function dirt()  { dir -rt "$@"; }
function dird()  { dir -d "$@"; }
function rea()   { history | grep -E "${@:-}"; }
function m()     { less "$@"; }
function c()     { cat "$@"; }

## history
# lots of history
export HISTSIZE=9999
export HISTFILESIZE=$HISTSIZE

# agglomerate history from multiple shells
export HISTCONTROL="ignoredups"
shopt -s histappend

export PROMPT_COMMAND+=" ; history -a"

#the below will make all commands visible in all shells
#PROMPT_COMMAND="$PROMPT_COMMAND ; history -a ; history -c; history -r"

# multi-line commands
shopt -s cmdhist

# machine-local file outside git
# shellcheck disable=SC1091
[ -f .localbashrc ] && . .localbashrc
uname -a
