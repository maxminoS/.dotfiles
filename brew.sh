#!/usr/bin/sh

# Install tools using Homebrew.
brew update
brew upgrade

# Save Homebrew’s installed location. (look more here)
BREW_PREFIX=$(brew --prefix)

# Install GNU core utilities
brew install coreutils
ln -s "${BREW_PREFIX}/bin/gsha256sum" "${BREW_PREFIX}/bin/sha256sum"
brew install moreutils
brew install findutils
brew install gnu-sed --with-default-names
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
brew install zsh
brew install vim --with-override-system-vi

# Networks
brew install curl
brew install wget --with-iri
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
brew install unzip
brew install poppler
brew install imagemagick
brew install ffmpeg
brew install mu
brew install isync
brew install spotifyd
brew install youtube-dl

# Casks
# Essentials
brew install --cask emacs --with-modules
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
