#!/usr/bin/env bash

set -euo pipefail

dir="$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"
rcrc_file="$dir/rcrc"

# Source: add-to-path, prompt, prompt-success, prompt-error, question, setup-dotfiles
source "$dir/setup/util.sh"

case "$(uname -a)" in
  Darwin*)
    prompt-error "INCOMPLETE: Update using flakes"
    exit 1
    source "$dir/setup/setup-macos.sh"
    setup-nix
    setup-dotfiles "$rcrc_file"
    setup-nix-darwin "darwin"
    setup-homebrew
    ;;
  Linux*WSL2*)
    source "$dir/setup/setup-nixos-wsl.sh"
    setup-dotfiles "$rcrc_file"
    setup-nixos "nixos-wsl"
    ;;
  *)
    prompt-error "[ERROR] Could not bootstrap for OS"
    exit 1
    ;;
esac

prompt-success "Done!"