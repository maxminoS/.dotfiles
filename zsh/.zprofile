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
    [ -e '/usr/local/bin/brew' ] && HOMEBREW_PREFIX_VAR='/usr/local'
    [ -e '/opt/homebrew/bin/brew' ] && HOMEBREW_PREFIX_VAR='/opt/homebrew'
    [ -z "$HOMEBREW_PREFIX_VAR" ] && echo 'Homebrew not found.'
    export HOMEBREW_PREFIX_VAR
    eval "$("$HOMEBREW_PREFIX_VAR"/bin/brew shellenv)"

    # Lazy-load nvm
    __NODE_GLOBALS=$(find "$NVM_DIR/versions/node/"*/bin/ -maxdepth 1 -mindepth 1 -type l -print0 | xargs -n1 basename | sort --unique)
    __NODE_GLOBALS+=(node nvm npm yarn)

    _load_nvm() {
	# shellcheck source=/dev/null
	[ -s "$HOMEBREW_PREFIX_VAR/opt/nvm/nvm.sh" ] && . "$HOMEBREW_PREFIX_VAR/opt/nvm/nvm.sh"
	# shellcheck source=/dev/null
	[ -s "$HOMEBREW_PREFIX_VAR/opt/nvm/etc/bash_completion.d/nvm" ] && . "$HOMEBREW_PREFIX_VAR/opt/nvm/etc/bash_completion.d/nvm"
    }

    for cmd in "${__NODE_GLOBALS[@]}"; do
	eval "function ${cmd}(){ unset -f ${__NODE_GLOBALS[*]}; _load_nvm; unset -f _load_nvm; ${cmd} \"\$@\"; }"
    done
    unset cmd __NODE_GLOBALS

    # Load jenv
    if type jenv > /dev/null; then
	eval "$(jenv init -)"
    fi
fi

# Cache SSH Credentials
eval "$(ssh-agent | sed -n '1,2p')"
# Source Rustup
# shellcheck source=/dev/null
[ -f "$XDG_DATA_HOME/cargo/env" ] && . "$XDG_DATA_HOME/cargo/env"
