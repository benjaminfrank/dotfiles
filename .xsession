#!/bin/bash

# Some distributions (e.g. Ubuntu) need to have a lightdm
# menu entry to be able to login with this script. Add the
# following (without comments) to /usr/share/xsessions/Xsession.desktop
#  [Desktop Entry]
#  Name=Xsession
#  Exec=/etc/X11/Xsession

. ~/.profile

if [ -f ~/.Xresources ] ; then
  echo "URxvt.perl-lib:$HOME/.urxvt" | xrdb -merge -
  xrdb -merge ~/.Xresources
fi

# top right corner stops screen lock
xautolock -locker "i3lock -c000000" -time 15 -corners +--- &


# clipboard
autocutsel -fork
autocutsel -selection PRIMARY -fork

if [ -f ~/.xsession.local ] ; then
  . ~/.xsession.local
fi

# stops problems with virtual windows not re-rendering
# https://github.com/chjj/compton/wiki/perf-guide
#compton -b

exec xmonad
