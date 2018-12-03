#!/bin/bash
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
export EDITOR="emacsclient -ct -a ''"

# PS1
export GIT_PS1_SHOWSTASHSTATE=true
export GIT_PS1_SHOWUNTRACKEDFILES=true
export GIT_PS1_SHOWDIRTYSTATE=true
export PROMPT_COMMAND='prompt_exit LX; prompt_title ; prompt_history'
if [ "$TERM" != "dumb" ]; then
    # set a fancy prompt
    export PS1='\[\e[33m\]\h'
    export PS1+='\[\e[36m\]${AWS_PROFILE:+[${AWS_PROFILE}]}'
    export PS1+='\[\e[35m\]($(mygitdir):$(mygitbranch))'
    export PS1+='\[\e[32m\]${LX:+\[\e[31m\]($LX)}$'
    export PS1+='\[\e[0m\] '
else
    export PS1="\\h\\$ "
fi

dir()  { ls -AlFh --color "$@"; }
dirt() { dir -rt "$@"; }
dird() { dir -d "$@"; }
rea()  { history | grep -E "${@:-}"; }
c()    { cat "$@"; }
g()    { grep -nIHE --color "$@"; }
m()    { less "$@"; }

startcontainer() {
    local S=~/git/dockerfiles/$1/$1.sh
    if [ -x "$S" ]
    then eval "$S ${2:-""} ${3:-""}"
    else echo "fail - expected this file to exist: $S"
    fi
}

basemanet() { startcontainer "${FUNCNAME[0]}" "${1:-"bash"}" "${2:-"~"}"; }
wg2()    { startcontainer "${FUNCNAME[0]}" "${1:-"bash"}" "${2:-"~/wg2"}"; }
dotnet() { startcontainer "${FUNCNAME[0]}" "${1:-"bash"}" "${2:-"~/git"}"; }
erlang() { startcontainer "${FUNCNAME[0]}" "${1:-"bash"}" "${2:-"~/git"}"; }
go()     { startcontainer "${FUNCNAME[0]}" "${1:-"bash"}" "${2:-"~/git"}"; }
java()   { startcontainer "${FUNCNAME[0]}" "${1:-"bash"}" "${2:-"~/git"}"; }
julia()  { startcontainer "${FUNCNAME[0]}" "${1:-"bash"}" "${2:-"~/git"}"; }
python() { startcontainer "${FUNCNAME[0]}" "${1:-"bash"}" "${2:-"~/git"}"; }
rust()   { startcontainer "${FUNCNAME[0]}" "${1:-"bash"}" "${2:-"~/git"}"; }

prompt_exit() {
    eval "$1='$?'; [ \$$1 == 0 ] && unset $1"
}

prompt_title() {
    [ "$TERM_PROGRAM" = "Apple_Terminal" ] && printf "\\e]1;%s\\a" "$(mygitdir)"
}

prompt_history() {
    history -a
}

prompt_aws() {
    grep -o "current-context.*" ~/.kube/config | cut -c18-
}

## history
# lots of history
export HISTSIZE=9999
export HISTFILESIZE=$HISTSIZE

# agglomerate history from multiple shells
export HISTCONTROL="ignoredups"
shopt -s histappend

#the below will make all commands visible in all shells
#PROMPT_COMMAND="$PROMPT_COMMAND ; history -a ; history -c; history -r"

# multi-line commands
shopt -s cmdhist

# set up AWS and k8s if possible
unset AWS_PROFILE
if [ -f ~/.aws/config ] && grep -q "profile prod" ~/.aws/config ; then
    export AWS_PROFILE=prod
fi
if [ -f ~/.kube/config ] && test "$(command -v kubectl)" ; then
    source <(kubectl completion bash)
    export AWS_PROFILE
    PROMPT_COMMAND+=' ; AWS_PROFILE=$(prompt_aws)'
fi

# motd
uname -a
