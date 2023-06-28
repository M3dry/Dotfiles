# Enable colors and change prompt:
autoload -U colors && colors    # Load colors
PS1="%{$fg[blue]%}[%{$fg[yellow]%}%n%{$fg[red]%}@%{$fg[cyan]%}%M %{$fg[yellow]%}%~%{$fg[blue]%}]%{$fg[blue]%}$ %{$reset_color%}"
setopt autocd
stty stop undef
setopt interactive_comments

# Basic auto/tab complete:
autoload -U compinit
zstyle ':completion:*' menu select
zstyle ':completion:*' matcher-list '' 'm:{a-zA-Z}={A-Za-z}' 'r:|[._-]=* r:|=*' 'l:|=* r:|=*'
zmodload zsh/complist
compinit
_comp_options+=(globdots)

# vi mode
bindkey -v
export KEYTIMEOUT=1

# Use vim keys in tab complete menu:
bindkey -M menuselect 'h' vi-backward-char
bindkey -M menuselect 'k' vi-up-line-or-history
bindkey -M menuselect 'l' vi-forward-char
bindkey -M menuselect 'j' vi-down-line-or-history
bindkey "^?" backward-delete-char

# Change cursor shape for different vi modes.
function zle-keymap-select {
  if [[ ${KEYMAP} == vicmd ]] ||
     [[ $1 = 'block' ]]; then
    echo -ne "\x1b[\x32 q"
  elif [[ ${KEYMAP} == main ]] ||
       [[ ${KEYMAP} == viins ]] ||
       [[ ${KEYMAP} = '' ]] ||
       [[ $1 = 'beam' ]]; then
    echo -e -n "\x1b[\x35 q"
  fi
}
zle -N zle-keymap-select
zle-line-init() {
    zle -K viins # initiate `vi insert` as keymap (can be removed if `bindkey -V` has been set elsewhere)
    echo -ne "\e[5 q"
}
zle -N zle-line-init
echo -ne '\e[5 q' # Use beam shape cursor on startup.
preexec() { echo -ne '\e[5 q' ;} # Use beam shape cursor for each new prompt.

bindkey '^[[P' delete-char

# Edit line in vim with ctrl-e:
autoload edit-command-line; zle -N edit-command-line

source "$HOME/.config/zsh/functions"

# Auto suggestions Plugin
ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE="fg=#3C435E,bold"
bindkey '^k' autosuggest-accept
bindkey '^ ' autosuggest-execute
bindkey '^b' autosuggest-clear

# You should use Plugin
export YSU_MESSAGE_POSITION="after"
export YSU_HARDCORE=0 # Hardcode mode

# opam configuration
[[ ! -r /home/m3/.opam/opam-init/init.zsh ]] || source /home/m3/.opam/opam-init/init.zsh  > /dev/null 2> /dev/null
