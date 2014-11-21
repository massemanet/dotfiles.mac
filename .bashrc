# -*- mode: shell-script -*-
# ~/.bashrc: executed by bash(1) for non-login shells.
# macos/homebrew style

# one path to rule them all
export PATH=/usr/local/bin:/usr/bin:/bin:/usr/local/sbin:/usr/sbin:/sbin
[ -d /opt/bin ] && export PATH=$PATH:/opt/bin

# one locale to rule them all
unset  LC_ALL
unset  LANGUAGE
unset  LC_CTYPE
export LANG=en_US.UTF-8

# If not running interactively, don't do anything
[ -z "$PS1" ] && return

# check for GNU ls
LS=ls ; [ `which gls 2> /dev/null` ] && LS=gls
DIRCOLS=dircolors ; [ `which gdircolors 2> /dev/null` ] && DIRCOLS=gdircolors

# Enable sane completion
. `brew --prefix`/etc/bash_completion
. `brew --prefix git`/etc/bash_completion.d/git-completion.bash

# emacs
EMACS=`brew --prefix`/bin/emacs

# prompt
GITPROMPT=`brew --prefix`/etc/bash_completion.d/git-prompt.sh

# macos doesn't have pgrep/pkill
pgrep() { ps -ef > $$ ; egrep -i "$1" $$ ; rm $$ ; }
pkill() { pgrep "$1" | awk '{print $2}' | xargs sudo kill -9 ; }

# find-grep
function fgrep() {
    set -f
    [ -z "$1" ] && exit 1
    [ -n "$2" ] && d="$2" || d="."
    [ -n "$3" ] && n="-name $3"
    find "$d" -path "*/.svn" -prune -o \
              -path "*/.git" -prune -o \
              -path "*/.eunit" -prune -o \
              -path "*/.deps" -prune -o \
              -path "*/deps" -prune -o \
              -type f $n -exec grep -iIH "$1" {} \;
    set +f
}

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

gitstat ()
{
    if [ -z "$1" ]; then
        base=~/git/*;
    else
        base=$1;
    fi;
    for d in $base;
    do
        echo -n `basename $d`;
        echo -n " ";
        ( cd $d;
          stat=$(git status);
          branch=$(echo $stat | grep -Eo "On branch .*$" | cut -f3 -d" ");
          [ -z "$branch" ] && branch="!";
          uptodate=$($(echo $stat | grep -q "is behind") && echo "!");
          uptodate=$($(echo $stat | grep -q "is ahead") && echo "*");
          uptodate=$($(echo $stat | grep -Eq "# Change|# Untra") && echo "#");
          echo -n $branch;
          echo -n "  ";
          echo -n "("$uptodate")";
          echo -n "  ";
          echo $(2>/dev/null git describe --tags HEAD) );
    done | column -t
}

function mygitps1() {
    if type __git_ps1 &> /dev/null ; then
        __git_ps1 "%s";
    elif [[ -f $GITPROMPT ]]; then
        . $GITPROMPT
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
    # enable color support of grep
    export GREP_OPTIONS='--color=auto'
    # enable color support of ls
    lscols=auto
    [ -f $HOME/.dircolors ] && eval "`$DIRCOLS -b $HOME/.dircolors`"
    # to get emacs -nw to use 256 colors
    export TERM=xterm-256color
    # set a fancy prompt
    export GIT_PS1_SHOWSTASHSTATE=true
    export GIT_PS1_SHOWUNTRACKEDFILES=true
    export GIT_PS1_SHOWDIRTYSTATE=true

    if [ "$USER" == "root" ];then
        PS1='\[$(tput setaf 5)\]\h\[$(tput setaf 3)\]($(mygitdir):$(mygitps1))\[$(tput setaf 2)\]${ERROR_FLAG:+\[$(tput setaf 1)\]}#\[$(tput sgr0)\] '
    else
        PS1='\[$(tput setaf 3)\]\h\[$(tput setaf 5)\]($(mygitdir):$(mygitps1))\[$(tput setaf 2)\]${ERROR_FLAG:+\[$(tput setaf 1)\]}\$\[$(tput sgr0)\] '
    fi
else
    lscols=none
    PS1="\h\$ "
fi

dir()  { $LS --color=$lscols -lFh "$@";}
dirt() { dir -rt "$@";}
dird() { dir -d "$@";}
dira() { for d in "${@:-.}"; do (cd "$d";pwd; dird .*); done;}
rea()  { history | egrep "${@:-}";}
m()    { less "$@";}
e()    { $EMACS -nw "$@";}
c()    { cat "$@";}

export EDITOR=$EMACS

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
