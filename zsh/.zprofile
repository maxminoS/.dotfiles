#!/bin/sh

# XDG Base Directories
export XDG_CONFIG_HOME="$HOME/.config"
export XDG_CACHE_HOME="$HOME/.cache"
export XDG_DATA_HOME="$HOME/.local/share"
# Set XDG_RUNTIME_DIR to /run/user/$UID if permitted or /tmp
[ ! -w ${XDG_RUNTIME_DIR:="/run/user/$UID"} ] && XDG_RUNTIME_DIR=/tmp
export XDG_RUNTIME_DIR

# Command Line
export SHELL='/usr/bin/zsh'
export ZDOTDIR="$XDG_CONFIG_HOME/zsh"
# Adds Homebrew to PATH
[ $OSTYPE != 'linux-gnu' ] && eval "$(/opt/homebrew/bin/brew shellenv)"
[ -f "$ZDOTDIR/.zshenv" ] && source "$ZDOTDIR/.zshenv"
# Source Rustup
[ -f "$XDG_DATA_HOME/cargo/env" ] && source "$XDG_DATA_HOME/cargo/env"
