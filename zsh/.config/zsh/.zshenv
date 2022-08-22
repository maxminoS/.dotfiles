#!/bin/sh

export BROWSER='/usr/bin/google-chrome-stable'
export LANG='en_US.UTF-8'

export PASSWORD_STORE_DIR="$XDG_DATA_HOME/.password-store"

# Command Line
export HISTFILE="$XDG_CACHE_HOME/zsh/history"
export LESSHISTFILE="-"
# GPG
GPG_TTY=$(tty)
export GPG_TTY
export GNUPGHOME="$XDG_DATA_HOME/gnupg"

export PSQLRC="$XDG_CONFIG_HOME/pg/psqlrc"
export PSQL_HISTORY="$XDG_CACHE_HOME/pg/psql_history"
export PGPASSFILE="$XDG_CONFIG_HOME/pg/pgpass"
export PGSERVICEFILE="$XDG_CONFIG_HOME/pg/pg_service.conf"

export DOCKER_CONFIG="$XDG_CONFIG_HOME/docker"
export MACHINE_STORAGE_PATH="$XDG_DATA_HOME/docker-machine"
export NPM_CONFIG_USERCONFIG="$XDG_CONFIG_HOME/npm/npmrc"
export NVM_DIR="$XDG_DATA_HOME/nvm"
export CARGO_HOME="$XDG_DATA_HOME/cargo"
export RUSTUP_HOME="$XDG_DATA_HOME/rustup"
export GOPATH="$XDG_DATA_HOME/go"
export LEIN_HOME="$XDG_DATA_HOME/lein"
export BOOT_HOME="$XDG_DATA_HOME/boot"
export BOOT_LOCAL_REPO="$XDG_CACHE_HOME/m2/repository"
export LUAROCKS_CONFIG="$XDG_DATA_HOME/luarocks/config-5.4.lua"
export ANSIBLE_CONFIG="$XDG_CONFIG_HOME/ansible/ansible.cfg"
export RANDFILE="$XDG_CONFIG_HOME/openssl/rnd"
export VIMINIT="source $XDG_CONFIG_HOME/vim/vimrc"
export GRADLE_USER_HOME="$XDG_DATA_HOME/gradle"
export JENV_ROOT="$XDG_DATA_HOME/jenv"

# Adds `~/.bin` and all subdirectories to PATH
[ -d "$HOME/.bin" ] && PATH="$(find "$HOME/.bin" -type d | paste -sd: - ):$PATH"
# Adds Yarn binaries to PATH
PATH="$PATH:$(yarn global bin)"
# Adds Go binaries to PATH
PATH="$PATH:$GOPATH/bin"
# Add Java version management
PATH="$PATH:$JENV_ROOT/bin"

# macOS
if [ "$(uname)" = 'Darwin' ]; then
    # Adds GNU coreutils to PATH
    PATH="$(brew --prefix make)/libexec/gnubin:$PATH"
    PATH="$(brew --prefix coreutils)/libexec/gnubin:$PATH"
    MANPATH="$(brew --prefix coreutils)/libexec/gnuman:$MANPATH"
    export MANPATH

    PATH="$(brew --prefix llvm)/bin:$PATH"
    # Adds ssh-copy-id to PATH
    PATH="$(brew --prefix ssh-copy-id)/bin:$PATH"
    # Adds Homebrew-installed Ruby to PATH
    PATH="$(brew --prefix ruby)/bin:$PATH"

    # Exports load path for mu
    MU_LOAD_PATH="$(brew --prefix mu)"
    export MU_LOAD_PATH
fi

export PATH
