#!/bin/sh

export LANG='en_US.UTF-8'

# Command Line
export HISTFILE="$XDG_CACHE_HOME/zsh/history"
export LESSHISTFILE="-"

# GPG
export DOCKER_CONFIG="$XDG_CONFIG_HOME/docker"
export RANDFILE="$XDG_CONFIG_HOME/openssl/rnd"
export VIMINIT="source $XDG_CONFIG_HOME/vim/vimrc"

# Adds `~/.bin` and all subdirectories to PATH
[ -d "$HOME/.bin" ] && export PATH="$(find "$HOME/.bin" -type d | paste -sd: - ):$PATH"
