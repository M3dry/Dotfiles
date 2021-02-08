# Enable colors and change prompt:
autoload -U colors && colors	# Load colors
PS1="%{$fg[blue]%}[%{$fg[yellow]%}%n%{$fg[red]%}@%{$fg[cyan]%}%M %{$fg[yellow]%}%~%{$fg[blue]%}] %{$fg[green]%}%T %{$fg[red]%}Ret: %?
%{$fg[blue]%} λ "
setopt autocd
stty stop undef
setopt interactive_comments

# History in cache directory:
HISTSIZE=10000000
SAVEHIST=10000000
HISTFILE=~/.cache/zsh/history

# Basic auto/tab complete:
autoload -U compinit
zstyle ':completion:*' menu select
zstyle ':completion:*' matcher-list '' 'm:{a-zA-Z}={A-Za-z}' 'r:|[._-]=* r:|=*' 'l:|=* r:|=*'
zmodload zsh/complist
compinit
_comp_options+=(globdots)		# Include hidden files.

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
    echo -ne '\e[1 q'
  elif [[ ${KEYMAP} == main ]] ||
       [[ ${KEYMAP} == viins ]] ||
       [[ ${KEYMAP} = '' ]] ||
       [[ $1 = 'beam' ]]; then
    echo -ne '\e[5 q'
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
bindkey '^e' edit-command-line

PATH="$HOME/.local/bin${PATH:+:${PATH}}"
export PATH=$HOME/bin:/usr/local/bin:$PATH
export EDITOR="nvim"
export VISUAL="nvim"
export TERMINAL="st"
export BROWSER="librewolf"
export XENVIRONMENT="${HOME}/.config/x11/xresources"
export PASSWORD_STORE_DIR="${HOME}/my-stuff/pass"

# Load syntax highlighting; should be last.
source /home/m3/.config/zsh/aliasesrc
source /usr/share/zsh/plugins/fast-syntax-highlighting/fast-syntax-highlighting.plugin.zsh 2>/dev/null
eval "$(starship init zsh)"
