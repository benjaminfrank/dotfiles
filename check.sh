#!/bin/bash

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
LOCAL="$DIR/$HOSTNAME"

diff -ru --exclude .git --exclude check.sh "$DIR" "$HOME" | grep -v "Only in $HOME"
if [ -d "$LOCAL" ] ; then
    diff -ru "$LOCAL" "$HOME" | grep -v "Only in $HOME"
else
    echo "no local settings backup in $LOCAL"
fi
