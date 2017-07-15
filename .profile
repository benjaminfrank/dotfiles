#!/bin/sh

export PATH=$HOME/.local/bin:$HOME/.fommil/bin:$PATH

export EDITOR=emacs
export HISTCONTROL=ignoredups
export WINEARCH=win32
export WINEDEBUG=fixme-all,warn+cursor

export COURSIER_VERBOSITY=-1
export SCALACTIC_FILL_FILE_PATHNAMES=yes
# WORKAROUND https://github.com/sbt/sbt-buildinfo/issues/96
export SBT_IGNORE_BUILDTIME=yes
export SONATYPE_USERNAME=fommil

export HTML_TIDY=$HOME/.tidyrc
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
