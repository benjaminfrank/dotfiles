#!/bin/sh

. $HOME/.xinitrc

#xrandr --auto --output DVI-I-1 --left-of DP-1
#xrandr --auto --output DVI-I-1 --right-of DP-0
#nautilus --no-desktop -n &
synergys

xscreensaver -no-splash &

exec xmonad
