# -*- mode: shell-script -*-
# ~/.bashrc: executed by bash(1) for non-login shells.
#
# macos/homebrew style

# make scp work by checking for a tty
[ -t 0 ] || return

# clean up
unalias -a

# check terminal resize
shopt -s checkwinsize

# pretty colors
eval "$(dircolors)"

#shellcheck disable=SC1090
. "$(brew --prefix bash-completion)/etc/bash_completion"

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
    export PS1+='\[\e[36m\]${AWS_PROFILE:+[${AWS_PROFILE}]}'
    export PS1+='\[\e[35m\]($(mygitdir):$(mygitbranch))'
    export PS1+='\[\e[32m\]${LAST_EXIT:+\[\e[31m\]($LAST_EXIT)}$'
    export PS1+='\[\e[0m\] '
else
    export PS1="\\h\\$ "
fi

function dir()   { ls -AlFh --color "$@"; }
function dirt()  { dir -rt "$@"; }
function dird()  { dir -d "$@"; }
function rea()   { history | grep -E "${@:-}"; }
function c()     { cat "$@"; }
function g()     { grep -nIHE --color "$@"; }
function m()     { less "$@"; }

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

# set up AWS and k8s if possible
unset AWS_PROFILE
if [ -f ~/.aws/config ] && grep -q "profile prod" ~/.aws/config ; then
    export AWS_PROFILE=prod
fi
if [ -f ~/.kube/config ] && test "$(which kubectl)" ; then
    export AWS_PROFILE
    PROMPT_COMMAND+=' ; AWS_PROFILE=$(grep -o "current-context.*" ~/.kube/config | cut -c18-)'
fi

# motd
uname -a
