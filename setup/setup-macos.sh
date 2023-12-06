#!/usr/bin/env bash

set -euo pipefail

# Nix
setup-nix () {
  if ! which nix-build &> /dev/null; then
    prompt "# Installing nix package manager..."
    curl -L https://nixos.org/nix/install | sh
  fi
}

# Setup nix-darwin
setup-nix-darwin () {
  host="$1"
  # TODO: Update and test to use flake
  if ! which darwin-rebuild &> /dev/null; then
    prompt "# Installing nix-darwin..."
    # if [ ! -e "/etc/shells.bak" ]; then
    #   echo "Moving /etc/shells to /etc/shells.bak"
    #   sudo mv /etc/shells /etc/shells.bak
    # fi
    nix run nix-darwin -- switch --flake "~/.config/nixpkgs/.#$host"
  else
    prompt "# Updating nix-darwin..."
    darwin-rebuild switch --flake "~/.config/nixpkgs/.#$host"
  fi

  if [ -e  "/run/current-system/Applications/iTerm2.app" ]; then
    ln -s /run/current-system/Applications/iTerm2.app /Applications
  fi
}

# Homebrew
setup-homebrew () {
  if ! which brew &> /dev/null; then
    prompt "# Installing homebrew..."
    /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
  fi
  if [ ! -e /Applications/Rectangle.app ]; then
    question "# Install rectangle?" "brew install --cask rectangle"
  fi
  if [ ! -e /Applications/Spotify.app ]; then
    question "# Install spotify?" "brew install --cask spotify"
  fi
}