#!/bin/sh

export PATH=$HOME/.fommil/bin:$PATH

export EDITOR=emacs
export HISTCONTROL=ignoredups
export WINEARCH=win32
export WINEDEBUG=fixme-all,warn+cursor

export COURSIER_VERBOSITY=-1
export SCALACTIC_FILL_FILE_PATHNAMES=yes
export SONATYPE_USERNAME=fommil

export COWPATH=$HOME/.cows:/usr/share/cowsay/cows

function source_if_exists {
    if [ -f "$1" ] ; then
        source "$1"
    fi
}

# place local system fixes in here
source_if_exists $HOME/.profile.local
source_if_exists $HOME/.profile.sec

if [ ! -f ~/.inputrc ] && [ -f /etc/inputrc ] ; then
    export INPUTRC=/etc/inputrc
fi
