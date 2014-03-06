#!/bin/sh

. $HOME/.xinitrc

#xrandr --auto --output DVI-I-1 --left-of DP-1
#xrandr --auto --output DVI-I-1 --right-of DP-0
#nautilus --no-desktop -n &
#synergys

#xscreensaver -no-splash &
# top right corner stops screen lock
xautolock -locker "i3lock -c000000" -time 1 -corners 0-00

exec xmonad
