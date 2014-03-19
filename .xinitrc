#!/bin/bash

export JAVA_HOME=/opt/jdk1.7.0_45
export PATH=/opt/intellij13/bin/:$JAVA_HOME/bin:$PATH
export NSS_SSL_CBC_RANDOM_IV=0
#export _JAVA_OPTIONS='-Dawt.useSystemAAFontSettings=on'

if [ -f ~/.Xresources ] ; then
    xrdb -merge ~/.Xresources
    echo "URxvt.perl-lib:$HOME/.urxvt" | xrdb -merge -
    touch DONE_STUFF
fi
