#!/bin/sh

export BROWSER='/usr/bin/google-chrome-stable'
export EDITOR='/usr/bin/vim'
export LANG='en_US.UTF-8'

export PASSWORD_STORE_DIR="$XDG_DATA_HOME/.password-store"

# Command Line
export TERMINAL='/usr/bin/alacritty'
export HISTFILE="$XDG_CACHE_HOME/zsh/history"
export LESSHISTFILE="-"
# GPG
export GPG_TTY=$(tty)
export GNUPGHOME="$XDG_DATA_HOME/gnupg"


export NPM_CONFIG_USERCONFIG="$XDG_CONFIG_HOME/npm/npmrc"
export CARGO_HOME="$XDG_DATA_HOME/cargo"
export GOPATH="$XDG_DATA_HOME/go"
export ANSIBLE_CONFIG="$XDG_CONFIG_HOME/ansible/ansible.cfg"
export RANDFILE="$XDG_CONFIG_HOME/openssl/rnd"
export VIMINIT="source $XDG_CONFIG_HOME/vim/vimrc"

# Adds `~/.bin` and all subdirectories to PATH
export PATH="$(find "$HOME/.bin" -type d | paste -sd: - ):$PATH"

# macOS
if [[ $OSTYPE == "darwin" ]]; then
    # Adds GNU coreutils to PATH
    export PATH="$(brew --prefix coreutils)/libexec/gnubin:$PATH"
    export MANPATH="$(brew --prefix coreutils)/libexec/gnuman:$MANPATH"
fi
