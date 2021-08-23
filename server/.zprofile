#!/bin/sh

export EDITOR="vim"

# XDG Base Directories
export XDG_CONFIG_HOME="$HOME/.config"
export XDG_CACHE_HOME="$HOME/.cache"
export XDG_DATA_HOME="$HOME/.local/share"
# Set XDG_RUNTIME_DIR to /run/user/$UID if permitted or /tmp
[ ! -w ${XDG_RUNTIME_DIR:="/run/user/$UID"} ] && XDG_RUNTIME_DIR=/tmp
export XDG_RUNTIME_DIR

# Command Line
export ZDOTDIR="$XDG_CONFIG_HOME/zsh"
[ -f "$ZDOTDIR/.zshenv" ] && source "$ZDOTDIR/.zshenv"

# Disallow message from other users
mesg n || true
