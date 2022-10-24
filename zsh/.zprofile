#!/usr/bin/env zsh

# XDG Base Directories
export XDG_CONFIG_HOME="$HOME/.config"
export XDG_CACHE_HOME="$HOME/.cache"
export XDG_DATA_HOME="$HOME/.local/share"
# Set XDG_RUNTIME_DIR to /run/user/$UID if permitted or /tmp
[ ! -w "${XDG_RUNTIME_DIR:="/run/user/$(id -u)"}" ] && XDG_RUNTIME_DIR=/tmp
export XDG_RUNTIME_DIR

UNAME="$(uname)"
export UNAME

export ZDOTDIR="$XDG_CONFIG_HOME/zsh"
# shellcheck source=/dev/null
[ -f "$ZDOTDIR/.zshenv" ] && . "$ZDOTDIR/.zshenv"

# macOS
if [ "$UNAME" = 'Darwin' ]; then
    # Adds Homebrew to PATH
    if [ -z "$HOMEBREW_PREFIX_VAR" ]; then
        [ -e '/usr/local/bin/brew' ] && HOMEBREW_PREFIX_VAR='/usr/local'
        [ -e '/opt/homebrew/bin/brew' ] && HOMEBREW_PREFIX_VAR='/opt/homebrew'
        [ -z "$HOMEBREW_PREFIX_VAR" ] && echo 'Homebrew not found.'
        export HOMEBREW_PREFIX_VAR
    fi
    eval "$("$HOMEBREW_PREFIX_VAR"/bin/brew shellenv)"

    # Load jenv
    # if type jenv > /dev/null; then
    #     eval "$(jenv init -)"
    # fi
fi

# Cache SSH Credentials
eval "$(ssh-agent | sed -n '1,2p')"
# Source Rustup
# shellcheck source=/dev/null
[ -f "$XDG_DATA_HOME/cargo/env" ] && . "$XDG_DATA_HOME/cargo/env"
