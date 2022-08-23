#!/bin/sh

# XDG Base Directories
export XDG_CONFIG_HOME="$HOME/.config"
export XDG_CACHE_HOME="$HOME/.cache"
export XDG_DATA_HOME="$HOME/.local/share"
# Set XDG_RUNTIME_DIR to /run/user/$UID if permitted or /tmp
[ ! -w "${XDG_RUNTIME_DIR:="/run/user/$(id -u)"}" ] && XDG_RUNTIME_DIR=/tmp
export XDG_RUNTIME_DIR

UNAME="$(uname)"
export UNAME

# macOS
if [ "$UNAME" = 'Darwin' ]; then
    # Adds Homebrew to PATH
    [ -e '/usr/local/bin/brew' ] && HOMEBREW_PREFIX_VAR='/usr/local'
    [ -e '/opt/homebrew/bin/brew' ] && HOMEBREW_PREFIX_VAR='/opt/homebrew'
    [ -z "$HOMEBREW_PREFIX_VAR" ] && echo 'Homebrew not found.'
    eval "$("$HOMEBREW_PREFIX_VAR"/bin/brew shellenv)"

    # Load nvm
    # shellcheck source=/dev/null
    [ -s "$HOMEBREW_PREFIX_VAR/opt/nvm/nvm.sh" ] && . "$HOMEBREW_PREFIX_VAR/opt/nvm/nvm.sh"
    # shellcheck source=/dev/null
    [ -s "$HOMEBREW_PREFIX_VAR/opt/nvm/etc/bash_completion.d/nvm" ] && . "$HOMEBREW_PREFIX_VAR/opt/nvm/etc/bash_completion.d/nvm"

    # Load jenv
    if type jenv > /dev/null; then
	eval "$(jenv init -)"
    fi
fi

# Command Line
export ZDOTDIR="$XDG_CONFIG_HOME/zsh"
# shellcheck source=/dev/null
[ -f "$ZDOTDIR/.zshenv" ] && . "$ZDOTDIR/.zshenv"
# Cache SSH Credentials
eval "$(ssh-agent | sed -n '1,2p')"
# Source Rustup
# shellcheck source=/dev/null
[ -f "$XDG_DATA_HOME/cargo/env" ] && . "$XDG_DATA_HOME/cargo/env"
