#!/bin/sh

# XDG Base Directories
export XDG_CONFIG_HOME="$HOME/.config"
export XDG_CACHE_HOME="$HOME/.cache"
export XDG_DATA_HOME="$HOME/.local/share"

# Command Line
export SHELL='/usr/bin/zsh'
export ZDOTDIR="$XDG_CONFIG_HOME/zsh"
[ -f "$ZDOTDIR/.zshenv" ] && source "$ZDOTDIR/.zshenv"
