#!/bin/bash

# top right corner stops screen lock
xautolock -locker "i3lock -c000000" -time 15 -corners 0-00 &

# clipboard
autocutsel &

if [ -f ~/.xsession.local ] ; then
  . ~/.xsession.local
fi

exec xmonad
