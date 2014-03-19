# Sam's universal GNU bash ~/.bashrc

if [ "$PS1" ] ; then
    # for the Emacs shell
    if [ "$TERM" = "dumb" ] ; then
        PS1="\u@\h:\w:"
    elif [ "$BASH" ] ; then
        if [ `uname` = "SunOS" ] ; then
            export id=`/usr/xpg4/bin/id -u`
        else
            export id=`id -u`
        fi
        if [ $id = 0 ] ; then
            PS1="\[\\033[1;31m\]\u@\[\\033[${PS1COLOUR}m\]\h\[\\033[1;31m\]:\w:\[\\033[0;39m\]"
        else
            PS1="\[\\033[${PS1COLOUR}m\]\u@\h:\w:\[\\033[0;39m\]"
        fi
    elif [ $id = 0 ] ; then
        PS1='# '
    else
        PS1='$ '
    fi

# DISABLED
# if the current bash shell is not the locally installed one
# then exec the local one
#    if [ -x $LOCALROOT/bin/bash ] &&
#        [ ! "$BASH_VERSION" = "`$LOCALROOT/bin/bash --version | grep version \
#     | sed 's|^.*version ||' | sed 's| .*$||'`" ] ; then
#        if [ -x ~/scripts/x-bender ] ; then x-bender ; fi
#        if [ $0 = "-bash" ] ; then
#            exec $LOCALROOT/bin/bash --login $*
#        else
#            exec $LOCALROOT/bin/bash $*
#        fi
#    fi

# load the completion table if it exists
    if [ -z "$BASH_COMPLETION" ] ; then
        if [ -f /etc/bash_completion ] ; then
            . /etc/bash_completion
        elif [ -f $LOCALROOT/etc/bash_completion ] ; then
            BASH_COMPLETION=$LOCALROOT/etc/bash_completion
            . $BASH_COMPLETION
        fi
    fi

# we can now start bash interactively
    TTYTEMP=`tty`
    if [ ! "`echo $TTYTEMP | grep 'not a tty'`" ] ; then
        # using a tty
        #   clear
        if [ -x "`which dircolors 2>/dev/null`" ] ; then
            if [ -f ~/.dircolours ] ; then
                eval `dircolors ~/.dircolours`
            elif [ -f /etc/dircolors ] ; then 
                eval `dircolors /etc/dircolors`
            else
                eval `dircolors`
            fi
        fi
        if [ -x "`which fortune 2>/dev/null`" ] ; then fortune ; fi
        if [ -x ~/.login.scripts ] ; then ~/.login.scripts ; fi
        if [ -f ~/.reminders ] ; then cat ~/.reminders ; fi
    fi
fi

if [ -f ~/.bashrc.local ] ; then
    . ~/.bashrc.local
fi
if [ -f ~/.aliases ] ; then
    . ~/.aliases
fi
if [ -f ~/.aliases.local ] ; then
    . ~/.aliases.local
fi
