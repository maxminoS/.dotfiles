#!/bin/sh

# XDG Base Directories
export XDG_CONFIG_HOME="$HOME/.config"
export XDG_CACHE_HOME="$HOME/.cache"
export XDG_DATA_HOME="$HOME/.local/share"
# Set XDG_RUNTIME_DIR to /run/user/$UID if permitted or /tmp
[ ! -w ${XDG_RUNTIME_DIR:="/run/user/$UID"} ] && XDG_RUNTIME_DIR=/tmp
export XDG_RUNTIME_DIR

# Command Line
export ZDOTDIR="$XDG_CONFIG_HOME/zsh"
# Adds Homebrew to PATH
[ "$OSTYPE" != 'linux-gnu' ] && [ -e '/usr/local/bin/brew' ] && HOMEBREW_PREFIX_VAR='/usr/local'
[ "$OSTYPE" != 'linux-gnu' ] && [ -e '/opt/homebrew/bin/brew' ] && HOMEBREW_PREFIX_VAR='/opt/homebrew'
[ "$OSTYPE" != 'linux-gnu' ] && [ -z "$HOMEBREW_PREFIX_VAR" ] && echo 'Homebrew not found.'
[ "$OSTYPE" != 'linux-gnu' ] && eval "$($HOMEBREW_PREFIX_VAR/bin/brew shellenv)"

[ -f "$ZDOTDIR/.zshenv" ] && source "$ZDOTDIR/.zshenv"
# Cache SSH Credentials
eval $(ssh-agent | sed -n '1,2p')
# Source Rustup
[ -f "$XDG_DATA_HOME/cargo/env" ] && source "$XDG_DATA_HOME/cargo/env"

# macOS
if [ $OSTYPE != 'linux-gnu' ]; then
    # Load nvm
    [ -s "$(brew --prefix nvm)/nvm.sh" ] && . "$(brew --prefix nvm)/nvm.sh"
    [ -s "$(brew --prefix nvm)/etc/bash_completion.d/nvm" ] && . "$(brew --prefix nvm)/etc/bash_completion.d/nvm"
    # Load jenv
    if ! type "$jenv" > /dev/null; then
        eval "$(jenv init -)"
    fi
fi
