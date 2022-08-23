#!/usr/bin/env zsh

# Settings
# --------
# Load aliases
# shellcheck source=/dev/null
[ -f "$ZDOTDIR/.aliasrc" ] && . "$ZDOTDIR/.aliasrc"

# History
HISTSIZE=10000

# Autocomplete
autoload -U compinit
zstyle ':completion:*' menu select
zmodload zsh/complist
compinit
_comp_options+=(globdots) # Include hidden files

# Alt+Backspace
alt-backspace-delete-word () {
    local WORDCHARS='~!#$%^&*(){}[]<>?+;'
    zle backward-delete-word
 }

 zle -N alt-backspace-delete-word
 bindkey    '\e^?' alt-backspace-delete-word

# Vim Mode
# --------
# bindkey -v
export KEYTIMEOUT=1

# Use vim keys in tab complete menu:
bindkey -M menuselect 'h' vi-backward-char
bindkey -M menuselect 'k' vi-up-line-or-history
bindkey -M menuselect 'l' vi-forward-char
bindkey -M menuselect 'j' vi-down-line-or-history
bindkey -v '^?' backward-delete-char

# Vim cursor shapes
zle-keymap-select() {
  if [ "$KEYMAP" = vicmd ] ||
     [ "$1" = 'block' ]; then
    printf '\e[1 q'
  elif [ "$KEYMAP" = main ] ||
       [ "$KEYMAP" = viins ] ||
       [ "$KEYMAP" = '' ] ||
       [ "$1" = 'beam' ]; then
    printf '\e[5 q'
  fi
}
zle -N zle-keymap-select
zle-line-init() {
    zle -K viins
    printf "\e[5 q"
}
zle -N zle-line-init
printf '\e[5 q'
preexec() { printf '\e[5 q' ;}

# Prompt
# ------
# Git branch
git_branch() {
  branch=$(git branch 2> /dev/null | sed -n -e 's/^\* \(.*\)/\1/p')
  if [ "$branch" != '' ]; then
    if [ "$(uname)" != 'Darwin' ]; then
      echo "  $branch"
    else
      echo " ⑂ $branch"
    fi
  fi
}

# Set prompt
autoload -U colors && colors
autoload -Uz add-zsh-hook
add-zsh-hook precmd set_prompt

set_prompt() {
    PROMPT="%{$fg[cyan]%}"
    PROMPT+="%M"
    PROMPT+="%{$fg[white]%}:%{$reset_color%}%~"
    PROMPT+="%{$fg[magenta]%}"
    PROMPT+="$(git_branch)"
    PROMPT+="%{$fg[green]%}
λ%{$reset_color%} "
}
