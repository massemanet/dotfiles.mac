# ~/.bash_profile: executed by bash(1) for login shells.

# include .bashrc if it exists
if [ -f ~/.bashrc ]; then
    . ~/.bashrc
fi

PATH=${HOME}/bin:${PATH}
