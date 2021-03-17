#!/usr/bin/sh

# Install Xcode Command Line Tools
xcode-select --install

# Install tools using Homebrew.
brew update
brew upgrade

# Install GNU core utilities
brew install coreutils
sudo ln -s "$(brew --prefix)/bin/gsha256sum" "${BREW_PREFIX}/bin/sha256sum"
brew install moreutils
brew install findutils
brew install gnu-sed
brew install grep
brew install stow

brew install gcc
brew install cmake
brew install automake
brew install autoconf
brew install libtool
brew install libvterm
brew install libpng
brew install zlib

# Password
brew install gnupg
brew install pass

# Command line
brew install bash

# Networks
brew install wget
brew install openssh
brew install ssh-copy-id
brew install htop
brew install windscribe

# Programming Languages
brew install python@3.9
brew install node
brew install yarn
brew install typescript
brew install go

# Others
brew install neofetch
brew install poppler
brew install imagemagick
brew install ffmpeg
brew install mu
brew install isync
brew install spotifyd
brew install youtube-dl

# Emacs
brew tap d12frosted/emacs-plus
brew install emacs-plus --HEAD --with-ctags --with-dbus --with-mailutils --with-no-titlebar --with-xwidgets
ln -s "$(brew --prefix emacs-plus)/Emacs.app" /Applications

# Services
brew services start dbus
brew services start spotifyd

# Casks
# Workflow
brew install --cask alacritty
brew install --cask dropbox

# Browsers
brew install --cask google-chrome
brew install --cask firefox
brew install --cask tor-browser
brew install --cask cloudflare-warp

# Media
brew install --cask gimp
brew install --cask inkscape
brew install --cask mpv
brew install --cask vlc

# Others
brew install --cask spotify
brew install --cask discord
brew install --cask qbittorrent
brew install --cask zoom

# Remove outdated versions
brew cleanup
