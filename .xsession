#!/bin/bash

# Some distributions (e.g. Ubuntu) need to have a lightdm
# menu entry to be able to login with this script. Add the
# following (without comments) to /usr/share/xsessions/Xsession.desktop
#  [Desktop Entry]
#  Name=Xsession
#  Exec=/etc/X11/Xsession

. ~/.profile

# https://wiki.archlinux.org/index.php/Java#Applications_not_resizing_with_WM.2C_menus_immediately_closing
export _JAVA_AWT_WM_NONREPARENTING=1

if [ -f ~/.Xresources ] ; then
    xrdb -load ~/.Xresources
    echo "URxvt.perl-lib:$HOME/.urxvt" | xrdb -merge -
fi

if [ -f ~/.Xresources.local ] ; then
    xrdb -merge ~/.Xresources.local
fi

if [ -f ~/.Xmodmap ] ; then
    xmodmap ~/.Xmodmap
fi

if [ -f ~/.Xmodmap.local ] ; then
    xmodmap ~/.Xmodmap.local
fi

# top right corner stops screen lock
xautolock -locker "i3lock -c000000" -time 15 -corners +--- &


# clipboard
autocutsel -fork
autocutsel -selection PRIMARY -fork

if [ -f ~/.xsession.local ] ; then
  . ~/.xsession.local
fi

# http://askubuntu.com/questions/124815
gsettings set org.gnome.desktop.interface gtk-key-theme "Emacs"

# stops problems with virtual windows not re-rendering
# https://github.com/chjj/compton/wiki/perf-guide
#compton -b

exec xmonad
