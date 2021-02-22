# Enable colors and change prompt:
autoload -U colors && colors	# Load colors
# PS1="%{$fg[blue]%}[%{$fg[yellow]%}%n%{$fg[red]%}@%{$fg[cyan]%}%M %{$fg[yellow]%}%~%{$fg[blue]%}] %{$fg[green]%}%T %{$fg[red]%}Ret: %?
# %{$fg[blue]%} λ "
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

PATH="$HOME/.local/bin${PATH:+:${PATH}}"
export PATH=$HOME/bin:/usr/local/bin:$PATH
export EDITOR="nvim"
export VISUAL="nvim"
export TERMINAL="st"
export BROWSER="librewolf"
export XENVIRONMENT="${HOME}/.config/x11/xresources"
export PASSWORD_STORE_DIR="${HOME}/my-stuff/pass"
export MANPAGER="nvim -c 'set ft=man' -"

bindkey '^e' edit-command-line

source "$HOME/.config/zsh/aliasesrc"

# Auto suggestions Plugin
source /usr/share/zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.plugin.zsh 2>/dev/null
ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE="fg=#8e3436,underline"
bindkey '^h' autosuggest-accept
bindkey '^ ' autosuggest-execute
bindkey '^b' autosuggest-clear

# You should use Plugin
source /usr/share/zsh/plugins/zsh-you-should-use/you-should-use.plugin.zsh
export YSU_MESSAGE_POSITION="after"
export YSU_HARDCORE=1 # Hardcode mode

# Starship
function set_win_title(){
    echo -ne "\033]0; $USER@$HOST $PWD \007"
}

precmd_functions+=(set_win_title)
eval "$(starship init zsh)"

# Syntax Highlight Plugin
source /usr/share/zsh/plugins/fast-syntax-highlighting/fast-syntax-highlighting.plugin.zsh 2>/dev/null

# Shorts
source "$HOME/.config/zsh/marks"
