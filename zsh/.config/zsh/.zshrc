#!/usr/bin/env zsh

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

# Vim
export KEYTIMEOUT=1
# Vim in tab autocompletion
bindkey -M menuselect 'h' vi-backward-char
bindkey -M menuselect 'k' vi-up-line-or-history
bindkey -M menuselect 'l' vi-forward-char
bindkey -M menuselect 'j' vi-down-line-or-history
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

# Git branch
git_branch() {
  branch=$(git branch 2> /dev/null | sed -n -e 's/^\* \(.*\)/\1/p')
  if [ "$branch" != '' ]; then
    if [ "$UNAME" != 'Darwin' ]; then
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
    P_HOSTNAME="%{$fg[yellow]%}%M"
    P_SEPARATOR="%{$reset_color%}:"
    P_PWD="%{$fg[cyan]%}%~"
    P_GIT="%{$fg[magenta]%}$(git_branch)"
    P_NEWLINE="$(printf '\n ')" && P_NEWLINE="${P_NEWLINE% }"
    P_PROMPT="${P_NEWLINE}%{$fg[red]%}%Bλ%b%{$reset_color%} "

    PROMPT="${P_HOSTNAME}${P_SEPARATOR}${P_PWD}${P_GIT}${P_PROMPT}"
    export PROMPT
}

# Syntax highlighting
[ "$UNAME" = 'Darwin' ] && . "$HOMEBREW_PREFIX_VAR/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh" 2>/dev/null
