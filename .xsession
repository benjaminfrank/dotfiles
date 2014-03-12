#!/bin/sh


#. $HOME/.xinitrc

xrandr --output DP-1 --brightness 0.7
xset m 2 0


#xrandr --auto --output DVI-I-1 --left-of DP-1
#xrandr --auto --output DVI-I-1 --right-of DP-0
#nautilus --no-desktop -n &
#synergys

#xscreensaver -no-splash &
# top right corner stops screen lock
xautolock -locker "i3lock -c000000" -time 1 -killer "sudo /usr/sbin/pm-suspend" -killtime 2 -corners 0-00 &

exec xmonad
