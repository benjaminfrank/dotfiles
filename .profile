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
export WINEARCH=win32
export WINEDEBUG=fixme-all,warn+cursor
export EDITOR="emacsclient -nw"

# using emacsclient will be super fast to load, but don't forget that
# changes to .emacs need to be manually loaded, or the daemon
# restarted.
DISPLAY="" emacs --daemon --with-x-toolkit=lucid &

if [ "$PS1" ] && [ "$BASH" ]; then
    if [ -f $HOME/.bashrc ] ; then
        . $HOME/.bashrc
    elif [ -f /etc/bashrc ] ; then
        . /etc/bashrc
    fi
fi

export JAVA_HOME=$(readlink -f /usr/bin/java | sed "s:bin/java::")
export JDK_HOME=$(readlink -f /usr/bin/javac | sed "s:bin/javac::")
