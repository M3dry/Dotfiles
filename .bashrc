#
# ~/.bashrc
#

#Ibus settings if you need them
#type ibus-setup in terminal to change settings and start the daemon
#delete the hashtags of the next lines and restart
#export GTK_IM_MODULE=ibus
#export XMODIFIERS=@im=dbus
#export QT_IM_MODULE=ibus

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

export HISTCONTROL=ignoreboth:erasedups

PS1='[\u@\h \W]\$ '

if [ -d "$HOME/.bin" ] ;
  then PATH="$HOME/.bin:$PATH"
fi

if [ -d "$HOME/.local/bin" ] ;
  then PATH="$HOME/.local/bin:$PATH"
fi

#ignore upper and lowercase when TAB completion
bind "set completion-ignore-case on"

#list
alias la='lsd -AlhF'
alias ll='exa -lar'
alias l.="lsd -A | egrep '^\.'"
alias vifm="./.config/vifm/scripts/vifmrun"
alias egrep='egrep --color=auto'
alias fgrep='fgrep --color=auto'
alias df='df -h'
alias yta-mp3="youtube-dl --extract-audio --audio-format mp3 "
shopt -s autocd # change to named directory
shopt -s cdspell # autocorrects cd misspellings
shopt -s cmdhist # save multi-line commands in history as single line
shopt -s dotglob
shopt -s histappend # do not overwrite history
shopt -s expand_aliases # expand aliases
alias ssn="sudo shutdown now"
alias sr="sudo reboot"
alias mv='mv -i'
alias rm='rm -i'
alias vim='nvim'
alias vimW='vim -c VimwikiIndex'
alias config='/usr/bin/git --git-dir=$HOME/test/ --work-tree=$HOME' 

# # ex = EXtractor for all kinds of archives
# # usage: ex <file>
ex ()
{
  if [ -f $1 ] ; then
    case $1 in
      *.tar.bz2)   tar xjf $1   ;;
      *.tar.gz)    tar xzf $1   ;;
      *.bz2)       bunzip2 $1   ;;
      *.rar)       unrar x $1   ;;
      *.gz)        gunzip $1    ;;
      *.tar)       tar xf $1    ;;
      *.tbz2)      tar xjf $1   ;;
      *.tgz)       tar xzf $1   ;;
      *.zip)       unzip $1     ;;
      *.Z)         uncompress $1;;
      *.7z)        7z x $1      ;;
      *.deb)       ar x $1      ;;
      *.tar.xz)    tar xf $1    ;;
      *.tar.zst)   unzstd $1    ;;
      *)           echo "'$1' cannot be extracted via ex()" ;;
    esac
  else
    echo "'$1' is not a valid file"
  fi
}

#create a file called .bashrc-personal and put all your personal aliases
#in there. They will not be overwritten by skel.

[[ -f ~/.bashrc-personal ]] && . ~/.bashrc-personal

export PS1="\[$(tput bold)\]\[$(tput setaf 1)\][\[$(tput setaf 226)\]\u\[$(tput setaf 29)\]@\[$(tput setaf 25)\]\h \[$(tput setaf 177)\]\W\[$(tput setaf 1)\]]\[$(tput setaf 255)\]\\$ \[$(tput sgr0)\]"

export PATH="$HOME/.local/bin${PATH:+:${PATH}}"
export EDITOR="nvim"
export TERMINAL="st"
export BROWSER="librewolf"
eval "$(starship init bash)"
