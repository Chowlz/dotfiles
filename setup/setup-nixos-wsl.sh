#!/usr/bin/env bash

set -euo pipefail

# Nix
setup-nixos () {
  host="$1"
  prompt "# Updating nixos host \"$host\"..."
  sudo nixos-rebuild switch --flake "path:$HOME/.dotfiles/config/nixpkgs#$host"
}