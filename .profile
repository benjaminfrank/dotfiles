#!/bin/sh

export PATH=$HOME/.fommil/bin:$PATH

# place local system fixes in here
if [ -f $HOME/.profile.local ] ; then
    . $HOME/.profile.local
fi

if [ ! -f ~/.inputrc ] && [ -f /etc/inputrc ] ; then
    export INPUTRC=/etc/inputrc
fi

export HISTCONTROL=ignoredups
export EDITOR="emacs -nw"

if [ "$PS1" ] && [ "$BASH" ]; then
    if [ -f $HOME/.bashrc ] ; then
        . $HOME/.bashrc
    elif [ -f /etc/bashrc ] ; then
        . /etc/bashrc
    fi
fi
