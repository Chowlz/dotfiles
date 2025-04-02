#!/usr/bin/env bash

set -eu -o pipefail

declare -A __DOTFILES_SOURCED
[[ ${__DOTFILES_SOURCED[nix.sh]:-false} != false ]] && return || __DOTFILES_SOURCED[nix.sh]=true

declare scripts_dir
scripts_dir=$(cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd)

# shellcheck source=/dev/null
source "$scripts_dir/_lib/log.sh"

function _nix/nix:install-nix-package-manager () {
  if ! which nix-build &> /dev/null; then
    log INFO "Installing nix package manager"
    curl -L https://nixos.org/nix/install | sh
  else
    log WARN "Nix package manager detected - install skipped"
  fi
}

function _nix/nix:gen-nix-conf () {
  local nix_dir=$HOME/.config/nix
  local nix_conf=$nix_dir/nix.conf

  if [[ ! -e "$nix_conf" ]]; then
    log INFO "Generating \"$nix_conf\" for flakes"
    mkdir -p "$nix_dir"
    echo "experimental-features = nix-command flakes" > "$nix_conf"
  else
    log WARN "$nix_conf detected - generation skipped"
  fi
}
