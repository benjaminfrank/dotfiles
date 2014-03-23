####################################################
# Sam's universal UNIX shell ~/.profile            #
####################################################

export PATH=$HOME/.fommil/bin:$PATH

# place local system fixes in here
if [ -f $HOME/.profile.local ] ; then
    . $HOME/.profile.local
fi

if [ ! -f ~/.inputrc ] && [ -f /etc/inputrc ] ; then
    export INPUTRC=/etc/inputrc
fi

export HISTCONTROL=ignoredups
export BROWSER=chromium
export EDITOR="emacs -nw"

if [ ! -z ${SSH_CLIENT} ] ; then
    export PS1COLOUR="0;31"
elif [ ${USER} = "root" ] ; then
    export PS1COLOUR="1;31"
else
    export PS1COLOUR="0;37"
fi

if [ "$PS1" ] && [ "$BASH" ]; then
    if [ -f $HOME/.bashrc ] ; then
        . $HOME/.bashrc
    elif [ -f /etc/bashrc ] ; then
        . /etc/bashrc
    fi
fi

