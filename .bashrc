# -*- mode: shell-script -*-
# ~/.bashrc: executed by bash(1) for non-login shells.

# one path to rule them all
export PATH=$HOME/bin:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin

# two locales to rule them all
export LANG=C
unset  LC_ALL
export LC_CTYPE=sv_SE
export LC_TIME=en_DK
export LC_PAPER=sv_SE
export LC_ALL="en_US"

# If not running interactively, don't do anything
[ -z "$PS1" ] && return

# Don't put duplicate lines in the history. See bash(1) for more options
export HISTSIZE=9999
export HISTFILESIZE=$HISTSIZE

# Enable sane completion
. /etc/bash_completion

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

if [ "$TERM" != "dumb" ]; then
# define useful aliases for color codes
    sh_norm="\[\033[0m\]"
    sh_black="\[\033[0;30m\]"
    sh_darkgray="\[\033[1;30m\]"
    sh_blue="\[\033[0;34m\]"
    sh_light_blue="\[\033[1;34m\]"
    sh_green="\[\033[0;32m\]"
    sh_light_green="\[\033[1;32m\]"
    sh_cyan="\[\033[0;36m\]"
    sh_light_cyan="\[\033[1;36m\]"
    sh_red="\[\033[0;31m\]"
    sh_light_red="\[\033[1;31m\]"
    sh_purple="\[\033[0;35m\]"
    sh_light_purple="\[\033[1;35m\]"
    sh_brown="\[\033[0;33m\]"
    sh_yellow="\[\033[1;33m\]"
    sh_light_gray="\[\033[0;37m\]"
    sh_white="\[\033[1;37m\]"
# enable color support of ls
    lscols=auto
    eval "`dircolors -b $HOME/.dircolors`"
# set a fancy prompt
    PROMPT_COMMAND='if [ $? -ne 0 ]; then ERROR_FLAG=1; else ERROR_FLAG=; fi'
    if [ "$USER" == "root" ];then 
        PS1=$sh_purple'\h'$sh_green'${ERROR_FLAG:+'$sh_red'}\$ '$sh_norm
    else
        PS1=$sh_brown'\h'$sh_green'${ERROR_FLAG:+'$sh_red'}\$ '$sh_norm
    fi
# to get emacs -nw to use 256 colors
    export TERM=xterm-256color
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
m()    { most "$@";}
e()    { emacs -nw "$@";}
c()    { cat "$@";}
x()    { sed -e'/^%/d' "$@" | xmlstarlet fo;}

#if which shellsink-client > /dev/null 2>&1 ; then
#    shopt -s histappend
#    export SHELL_SINK_COMMAND=shellsink-client
#    export SHELL_SINK_ID=33b45f5d8d284806993711230261a055
#    export SHELL_SINK_TAGS=$HOSTNAME
#    if [ -n "$PROMPT_COMMAND" ]; then
#        PROMPT_COMMAND="$PROMPT_COMMAND;history -a;$SHELL_SINK_COMMAND"
#    else
#        PROMPT_COMMAND="history -a;$SHELL_SINK_COMMAND"
#    fi
#fi
