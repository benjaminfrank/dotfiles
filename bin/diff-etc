#!/bin/bash

# don't forget to run under sudo

# type the following to get a list of all unexplained files
# (most of them are auto generated so don't need to be included)
#
#   sudo cruft -d /etc

manualFiles="/etc/modprobe.d/that-fucking-beep.conf
/etc/systemd/system/i3lock.service
/etc/systemd/system/sleep.target.wants/i3lock.service
/etc/default/keyboard
/etc/apt/sources.list.d/google-talkplugin.list
/etc/apt/sources.list.d/mono-xamarin.list
/etc/apt/sources.list.d/spotify.list
/etc/modprobe.d/intel-sound.conf
/etc/sudoers.d/apt
/etc/sudoers.d/pmsuspend
/etc/udev/rules.d/50-nvidia.rules
/etc/udev/rules.d/51-android.rules
/etc/X11/xorg.conf"

# TODO: list of files to ignore

name=etc
tmp=/tmp/$name.tmp
missingPattern='^debsums: missing file'

# get differing files using debsums
debsums -e| awk '$2 !~ /OK/{print $1}' > $tmp.1

# print missing
cat $tmp.1 | grep "$missingPattern"
# get non-missing
cat $tmp.1 | grep -v "$missingPattern" > $tmp.2

echo "$manualFiles" >> $tmp.2

# find the installed packages that contain the differing files
cat $tmp.2 | xargs dpkg -S | cut -d: -f1 | sort -u | xargs dpkg-query -W -f='${Package} ${Status}\n' | awk '$4 ~ /installed/{print $1}' > $tmp.3
# here we lose some information on which config files are in uninstalled packages... oh well

# download packages
cat $tmp.3 | xargs apt-get install -qdy --force-yes --reinstall

# extract packages to a temp dir
mkdir -p $tmp.4/
cat $tmp.3 | xargs -I%%% find /var/cache/apt/archives -name '%%%_*' | xargs -I%%% dpkg-deb --extract %%% $tmp.4/

# diff them!
cat $tmp.2 | xargs -I%%% diff -Nu $tmp.4/%%% %%% > $name.diff

# clean up
#echo "you might want to clean up $tmp"
rm -rf $tmp.*
