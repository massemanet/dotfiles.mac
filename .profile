#!/bin/bash
# ~/.profile: executed by the command interpreter for login shells.
# This file is not read by bash(1), if ~/.bash_profile exists.

# one path to rule them all
function add_to_path() {
    [ -d "$1" ] && export PATH="$1":$PATH
}
add_to_path /opt/bin
add_to_path "$(brew --prefix coreutils)/libexec/gnubin"
add_to_path "$(brew --prefix)/bin"
add_to_path ~/bin

# one locale to rule them all
unset  LC_ALL
unset  LANGUAGE
unset  LC_CTYPE
LANG="$(locale -a | grep -Ei "en.us.utf")"
export LANG
