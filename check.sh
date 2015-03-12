#!/bin/bash

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
LOCAL="${DIR}/Profiles/${HOSTNAME}"

function fdiff {
    diff -ru --exclude .git \
         --exclude .emacs-test.el \
         --exclude .travis.yml \
         --exclude check.sh \
         --exclude diff-etc-changes.sh \
         --exclude Profiles \
         --exclude bin \
         --exclude var \
         --exclude usr \
         --exclude crontab "$1" "$2" | \
        sed -e "s|Only in ${1}|No equivalent|" | \
        grep -v "Only in"
}

fdiff "$DIR" "$HOME"

if [ -d "$LOCAL" ] ; then
    fdiff "$LOCAL" "$HOME"
else
    echo "no local settings backup in $LOCAL"
fi
