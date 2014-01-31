####################################################
# Sam's universal UNIX shell ~/.profile            #
####################################################

#ARCH="`echo $MACHTYPE | sed 's|\-.*||'`"

ARCH=`arch`

if [ -d $HOME/Library ] ; then
    export LOCALROOT=$HOME/Library/bin/$ARCH/`uname`
    SCRIPTS=$HOME/Library/scripts
else
    export LOCALROOT=$HOME/bin/$ARCH/`uname`
    SCRIPTS=$HOME/scripts
fi

# place local system fixes in here
if [ -f $HOME/.profile.local ] ; then
    . $HOME/.profile.local
fi

if [ ! -f ~/.inputrc ] && [ -f /etc/inputrc ] ; then
    export INPUTRC=/etc/inputrc
fi

PATH=$LOCALROOT/bin:$PATH

if [ ! `uname` = "SunOS" ] && [ `id -u` = 0 ] ; then
    umask 022
    PATH=/usr/local/sbin:/sbin:/usr/sbin:$PATH
else
    umask 002
fi
export PATH="$SCRIPTS:$PATH"
#export CPPFLAGS="-I$LOCALROOT/include $CPPFLAGS"
#export LDFLAGS="-L$LOCALROOT/lib $LDFLAGS"
export HISTCONTROL=ignoredups
export BROWSER=firefox
export EDITOR=nano
export CVS_RSH=ssh
export PYTHONSTARTUP=$HOME/.python

HOSTNAME=`hostname | sed 's|\..*$||'`
# PS1 colour
if [ `which domainname 2>/dev/null` ] && [ "`domainname`" = amsma.hw.ac.uk ] ; then
    PS1COLOUR="1;35"
else
    case $HOSTNAME in
        thinktankmaths)
            PS1COLOUR="1;36";;
        sampras)
            PS1COLOUR="1;37";;
	Sampo)
	    PS1COLOUR="1;37";;
	sampo)
	    PS1COLOUR="1;37";;
        sampras-eth)
            PS1COLOUR="0;37";;
        lcsamuel)
            PS1COLOUR="0;36";;
        Sampras)
            PS1COLOUR="1;37";;
        fommil)
            PS1COLOUR="1;33";;
    esac
fi
if [ ! $PS1COLOUR ] ; then
    PS1COLOUR="0;31"
fi
export PS1COLOUR

if [ "$PS1" ] && [ "$BASH" ]; then
    if [ -f $HOME/.bashrc ] ; then
        . $HOME/.bashrc
    elif [ -f /etc/bashrc ] ; then
        . /etc/bashrc
    fi
fi

#ulimit -c unlimited

# Setting PATH for MacPython 2.5
# The orginal version is saved in .profile.pysave
PATH="/Library/Frameworks/Python.framework/Versions/Current/bin:${PATH}"
PATH="/opt/mingw32/bin:/opt/mingw64/bin:$PATH"
export PATH
