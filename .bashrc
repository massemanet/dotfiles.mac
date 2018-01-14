# -*- mode: shell-script -*-
# ~/.bashrc: executed by bash(1) for non-login shells.
#
# macos/homebrew style

# clean up
unalias -a

# check for coreutils
GREP=$(which ggrep)            || GREP=$(which grep)
LS="$(which gls) --color=auto" || LS=$(which ls)
DIRCOLORS=$(gdircolors)        || DIRCOLORS=$(dircolors)

# Enable sane completion
bash_completion=$(brew --prefix)/etc/bash_completion
[ -f $bash_completion ] && . $bash_completion

# define some git helpers
[ -f ~/bin/gitfunctions ] && . ~/bin/gitfunctions

# emacs
SOCKET="$(lsof -U 2>/dev/null | grep Emacs | awk '{print $8}')"
export EDITOR="emacsclient -s $SOCKET"

PROMPT_COMMAND='[ "$?" == "0" ] && ERROR_FLAG= || ERROR_FLAG=1'
if type kubectl &> /dev/null ; then
    PROMPT_COMMAND+=' ; KUBE_CONTEXT=[$(kubectl config current-context)]'
fi
if [ "$TERM" != "dumb" ]; then
    # to get emacs -nw to use 256 colors
#    export TERM=xterm-256color
    eval $DIRCOLORS
    # set a fancy prompt
    export GIT_PS1_SHOWSTASHSTATE=true
    export GIT_PS1_SHOWUNTRACKEDFILES=true
    export GIT_PS1_SHOWDIRTYSTATE=true

    PS1='\[$(tput setaf 3)\]\h'
    PS1+='\[$(tput setaf 6)\]$KUBE_CONTEXT'
    PS1+='\[$(tput setaf 5)\]($(mygitdir):$(mygitbranch))'
    PS1+='\[$(tput setaf 2)\]${ERROR_FLAG:+\[$(tput setaf 1)\]}\$'
    PS1+='\[$(tput sgr0)\] '
else
    PS1="\h\$ "
fi
export PS1

# macos doesn't have pgrep/pkill
function grep()  { $GREP --color=auto "$@"; }
function pgrep() { ps -ef > $$ ; egrep -i "$1" $$ ; rm $$ ; }
function pkill() { pgrep "$1" | awk '{print $2}' | xargs sudo kill -9 ; }
function dir()   { $LS -lFh "$@"; }
function dirt()  { dir -rt "$@"; }
function dird()  { dir -d "$@"; }
function dira()  { for d in "${@:-.}"; do (cd "$d"; pwd; dird .*); done; }
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

PROMPT_COMMAND+=";history -a"

#the below will make all commands visible in all shells
#PROMPT_COMMAND="$PROMPT_COMMAND ; history -a ; history -c; history -r"

# multi-line commands
shopt -s cmdhist

# machine-local file outside git
[ -f ~/.localbashrc ] && . ~/.localbashrc || uname -a
