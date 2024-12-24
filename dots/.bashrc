[[ $- != *i* ]] && return

export HISTCONTROL=ignoreboth:erasedups

bind "set completion-ignore-case on"

shopt -s autocd
shopt -s cmdhist
shopt -s dotglob
shopt -s histappend
shopt -s expand_aliases

bind -x '"\C-l": clear;'

ex ()
{
  if [ -f "$1" ] ; then
    case "$1" in
      *.tar.bz2)   tar xjf "$1"   ;;
      *.tar.gz)    tar xzf "$1"   ;;
      *.bz2)       bunzip2 "$1"   ;;
      *.rar)       unrar x "$1"   ;;
      *.gz)        gunzip "$1"    ;;
      *.tar)       tar xf "$1"    ;;
      *.tbz2)      tar xjf "$1"   ;;
      *.tgz)       tar xzf "$1"   ;;
      *.zip)       unzip "$1"     ;;
      *.Z)         uncompress "$1";;
      *.7z)        7z x "$1"      ;;
      *.deb)       ar x "$1"      ;;
      *.tar.xz)    tar xf "$1"    ;;
      *.tar.zst)   unzstd "$1"    ;;
      *)           echo "'$1' cannot be extracted via ex()" ;;
    esac
  else
    echo "'$1' is not a valid file"
  fi
}

export PS1="\[$(tput setaf 4)\][\[$(tput setaf 3)\]\u\[$(tput setaf 1)\]@\[$(tput setaf 6)\]\h \[$(tput setaf 3)\]\W\[$(tput setaf 4)\]]\[$(tput setaf 4)\]\\$ \[$(tput sgr0)\]"

# Vi mode
set -o vi

f() {
    if [[ $# -eq 1  && ( -d "$1" || "$1" == "-" ) ]]
    then
        builtin cd "$1" || return
        eza -gar --group-directories-first
    elif test $# -eq 0
    then
        builtin cd "$HOME" || return
    elif test -f "$1" || test ! -e "$1" || test $# -gt 1
    then
        $EDITOR "$@"
    else
        printf "t: case not accounted for\n"
    fi
}
