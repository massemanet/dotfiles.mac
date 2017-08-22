echo profile
# ~/.profile: executed by the command interpreter for login shells.
# This file is not read by bash(1), if ~/.bash_profile exists.

# go is a special snowflake
export GOROOT=/usr/local/opt/go/libexec
export GOPATH=~/go

# one path to rule them all
function add_to_path() {
    [ -d "$1" ] && export PATH="$1":$PATH
}
add_to_path $GOPATH/bin
add_to_path $GOROOT/bin
add_to_path /opt/bin
add_to_path $(brew --prefix coreutils)/libexec/gnubin
add_to_path ~/bin

# one locale to rule them all
unset  LC_ALL
unset  LANGUAGE
unset  LC_CTYPE
export LANG=`locale -a | grep -Ei "en.us.utf"`

which emacs && emacs --daemon || echo "no emacs"
