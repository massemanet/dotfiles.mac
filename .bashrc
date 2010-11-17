# -*- mode: shell-script -*-
# ~/.bashrc: executed by bash(1) for non-login shells.

# one path to rule them all
export PATH=$HOME/bin:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin

# two locales to rule them all
unset  LC_ALL
unset  LANGUAGE
export LANG=en_US
export LC_CTYPE=sv_SE
export LC_TIME=en_DK
export LC_PAPER=sv_SE

# If not running interactively, don't do anything
[ -z "$PS1" ] && return

# Enable sane completion
. /etc/bash_completion

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# find the basename of the dir that contains the current .git
mygitdir () { 
    local g;
    g="`__gitdir`";
    if [ "$g" == "" ]; then
        g="";
    else
        if [ "$g" == ".git" ]; then
            g="`pwd`";
        else
            g=`dirname "$g"`;
        fi;
    fi;
    if [ "$g" == "`echo ~`" ]; then
        echo "~";
    else
        basename "$g";
    fi
}

if [ "$TERM" != "dumb" ]; then
# enable color support of ls
    lscols=auto
    eval "`dircolors -b $HOME/.dircolors`"
# to get emacs -nw to use 256 colors
    export TERM=xterm-256color
# set a fancy prompt
    export GIT_PS1_SHOWSTASHSTATE=true
    export GIT_PS1_SHOWUNTRACKEDFILES=true
    unset GIT_PS1_SHOWDIRTYSTATE
    PROMPT_COMMAND='if [ $? -ne 0 ]; then ERROR_FLAG=1; else ERROR_FLAG=; fi'
    if [ "$USER" == "root" ];then 
        PS1='\[$(tput setaf 5)\]\h\[$(tput setaf 3)\]($(mygitdir):$(__git_ps1 "%s"))\[$(tput setaf 2)\]${ERROR_FLAG:+\[$(tput setaf 1)\]}#\[$(tput sgr0)\] '
    else
        PS1='\[$(tput setaf 3)\]\h\[$(tput setaf 5)\]($(mygitdir):$(__git_ps1 "%s"))\[$(tput setaf 2)\]${ERROR_FLAG:+\[$(tput setaf 1)\]}\$\[$(tput sgr0)\] '
    fi
else
    lscols=none
    PS1="\h\$ "
fi

xterm(){ /usr/bin/xterm -xrm "XTerm*VT100.metaSendsEscape: True" -sl 9999 ;}
dir()  { ls --color=$lscols -lF "$@";}
dirt() { dir -rt "$@";}
dird() { dir -d "$@";}
dira() { for d in "${@:-.}"; do (cd "$d";pwd; dird .*); done;}
rea()  { history | egrep "${@:-}";}
m()    { less "$@";}
e()    { emacs -nw "$@";}
c()    { cat "$@";}
x()    { sed -e'/^%/d' "$@" | xmlstarlet fo;}
smg()  { sudo mg -n $1;}

## history
# lots of history
export HISTSIZE=9999
export HISTFILESIZE=$HISTSIZE

# agglomerate history from multiple shells
export HISTCONTROL="ignoredups"
shopt -s histappend
PROMPT_COMMAND="$PROMPT_COMMAND;history -a;history -c;history -r"

# multi-line commands
shopt -s cmdhist
