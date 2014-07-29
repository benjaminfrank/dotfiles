#!/bin/bash

# exit early if not interactive
if [ "$PS1" = "" ] ; then return ; fi

if [ -z "$BASH_COMPLETION" ] ; then
    if [ -f /etc/bash_completion ] ; then
        . /etc/bash_completion
    fi
fi

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

# PS1 Colours
Black="\[\033[0;30m\]"
Red="\[\033[0;31m\]"
Green="\[\033[0;32m\]"
Yellow="\[\033[0;33m\]"
Blue="\[\033[0;34m\]"
Purple="\[\033[0;35m\]"
Cyan="\[\033[0;36m\]"
White="\[\033[0;37m\]"
# Bold
BBlack="\[\033[1;30m\]"
BRed="\[\033[1;31m\]"
BGreen="\[\033[1;32m\]"
BYellow="\[\033[1;33m\]"
BBlue="\[\033[1;34m\]"
BPurple="\[\033[1;35m\]"
BCyan="\[\033[1;36m\]"
BWhite="\[\033[1;37m\]"
# High Intensty
IBlack="\[\033[0;90m\]"
IRed="\[\033[0;91m\]"
IGreen="\[\033[0;92m\]"
IYellow="\[\033[0;93m\]"
IBlue="\[\033[0;94m\]"
IPurple="\[\033[0;95m\]"
ICyan="\[\033[0;96m\]"
IWhite="\[\033[0;97m\]"
# Reset
Color_Off="\[\033[0m\]"

if [ "$SSH_CLIENT" != "" ] || [ "$SSH_TTY" != "" ] ; then
    PS1PREFIX="$White\h$Color_Off "
fi

# TODO: can this be factored out into a clean function?
export PS1=$PS1PREFIX'$(git branch &>/dev/null;\
if [ $? -eq 0 ]; then \
  echo "$(echo `git status` | grep "nothing to commit" > /dev/null 2>&1; \
  if [ "$?" -eq "0" ]; then \
    # @4 - Clean repository - nothing to commit
    echo "'$IGreen'"$(__git_ps1 "%s "); \
  else \
    # @5 - Changes to working tree
    echo "'$IRed'"$(__git_ps1 "%s"); \
  fi) "
fi)'"$IBlack\\w$Color_Off "

# workaround for the Emacs shell
if [ "$TERM" = "dumb" ] ; then
    export PS1="\u@\h:\w:"
fi

# Local settings and overrides
if [ -f ~/.bashrc.local ] ; then
    . ~/.bashrc.local
fi
if [ -f ~/.aliases ] ; then
    . ~/.aliases
fi
if [ -f ~/.aliases.local ] ; then
    . ~/.aliases.local
fi
