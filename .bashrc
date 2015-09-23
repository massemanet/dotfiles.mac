# -*- mode: shell-script -*-
# ~/.bashrc: executed by bash(1) for non-login shells.
#
# macos/homebrew style

CU=$(brew --prefix coreutils)
[ -n "$CU" ] && export PATH=$CU/libexec/gnubin:$PATH
GREP=grep ; [ $(which ggrep) ] && GREP=ggrep
LS=ls ; [ $(which gls) ] && LS=gls
DIRCOLORS=dircolors ; [ $(which gdircolors) ] && DIRCOLORS=gdircolors
bash_completion=$(brew --prefix)/etc/bash_completion

# one path to rule them all
export PATH=/usr/local/bin:/usr/bin:/bin:/usr/local/sbin:/usr/sbin:/sbin
[ -d /opt/bin ] && export PATH=$PATH:/opt/bin

# one locale to rule them all
unset  LC_ALL
unset  LANGUAGE
unset  LC_CTYPE
export LANG=`locale -a | grep -Ei "en.us.utf"`

# If not running interactively, don't do anything
[ -z "$PS1" ] && return

# force globbing on
set +f

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# Enable sane completion
[ -f $bash_completion ] && . $bash_completion

# define some git helpers
[ -f ~/.gitfunctions ] && . ~/.gitfunctions

# emacs
export EDITOR=emacs

# gitified prompt
function mygitps1() {
    if type __git_ps1 &> /dev/null ; then
        __git_ps1 "%s";
    else
        for b in `git log --format='%d' 2> /dev/null | head -1 | tr "(,)" " "`
        do echo $b | awk '
/HEAD/       {next}
/origin\//   {next}
/upstream\// {next}
/tag:/       {next}
             {print $1}'
        done | head -1
    fi
}

# find the basename of the dir that contains the current .git
function mygitdir () {
    local D;
    D=`git rev-parse --git-dir 2> /dev/null`
    [ "$D" == ".git" ] && D="$PWD/$D"
    D=`dirname "$D"`
    if [ "$D" == "." ]; then
        echo "";
    elif [ "$D" == ~ ]; then
        echo "~";
    else
        basename "$D"
    fi
}

PROMPT_COMMAND='if [ $? -ne 0 ]; then ERROR_FLAG=1; else ERROR_FLAG=; fi'
if [ "$TERM" != "dumb" ]; then
    # enable color support of ls
    lscols=auto
    [ -f $HOME/.dircolors ] && eval "`$DIRCOLORS -b $HOME/.dircolors`"
    # to get emacs -nw to use 256 colors
    export TERM=xterm-256color
    # set a fancy prompt
    export GIT_PS1_SHOWSTASHSTATE=true
    export GIT_PS1_SHOWUNTRACKEDFILES=true
    export GIT_PS1_SHOWDIRTYSTATE=true

    if [ "$USER" == "root" ];then
        PS1='\[$(tput setaf 5)\]\h\[$(tput setaf 3)\]'
        PS1+='($(mygitdir):$(mygitps1))\[$(tput setaf 2)\]'
        PS1+='${ERROR_FLAG:+\[$(tput setaf 1)\]}#\[$(tput sgr0)\] '
    else
        PS1='\[$(tput setaf 3)\]\h\[$(tput setaf 5)\]'
        PS1+='($(mygitdir):$(mygitps1))\[$(tput setaf 2)\]'
        PS1+='${ERROR_FLAG:+\[$(tput setaf 1)\]}\$\[$(tput sgr0)\] '
    fi
else
    lscols=none
    PS1="\h\$ "
fi

# macos doesn't have pgrep/pkill
function grep()  { $GREP --color=auto "$@"; }
function fgrep() { ~/.fgrep.sh "$@"; }
function tmx()   { ~/.tmux.sh; }
function pgrep() { ps -ef > $$ ; egrep -i "$1" $$ ; rm $$ ; }
function pkill() { pgrep "$1" | awk '{print $2}' | xargs sudo kill -9 ; }
function dir()   { $LS --color=$lscols -lFh "$@";}
function dirt()  { dir -rt "$@";}
function dird()  { dir -d "$@";}
function dira()  { for d in "${@:-.}"; do (cd "$d";pwd; dird .*); done;}
function rea()   { history | egrep "${@:-}";}
function m()     { less "$@";}
function e()     { emacs -nw "$@";}
function c()     { cat "$@";}

## history
# lots of history
export HISTSIZE=9999
export HISTFILESIZE=$HISTSIZE

# agglomerate history from multiple shells
export HISTCONTROL="ignoredups"
shopt -s histappend

PROMPT_COMMAND="$PROMPT_COMMAND;history -a"

#the below will make all commands visible in all shells
#PROMPT_COMMAND="$PROMPT_COMMAND ; history -a ; history -c; history -r"

# multi-line commands
shopt -s cmdhist

# machine-local file outside git
[ -f ~/.localbashrc ] && . ~/.localbashrc || uname -a
