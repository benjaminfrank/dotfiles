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
export WINEARCH=win32
export WINEDEBUG=fixme-all,warn+cursor

if [ "$PS1" ] && [ "$BASH" ]; then
    if [ -f $HOME/.bashrc ] ; then
        . $HOME/.bashrc
    elif [ -f /etc/bashrc ] ; then
        . /etc/bashrc
    fi
fi

export JAVA_HOME=$(readlink -f /usr/bin/java | sed "s:bin/java::")
export JDK_HOME=$(readlink -f /usr/bin/javac | sed "s:bin/javac::")
