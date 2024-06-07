#!/usr/bin/env bash

set -euo pipefail

dir="$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"
rcrc_file="$dir/rcrc"

# Source: add-to-path, prompt, prompt-success, prompt-error, question, setup-dotfiles
source "$dir/setup/util.sh"

setup-home-manager () {
  setup-dotfiles "$rcrc_file"
}

setup-os () {
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
}

# Process args
opt_help=""
opt_unknown_args=""
opt_home_manager_only=""
opts=`getopt -o h,m --long help,home-manager-only -- "$@"`
eval set -- "$opts"
while true ; do
  case "$1" in
    -h|--help)
      opt_help="true"
      shift
      ;;
    -m|--home-manager-only)
      opt_home_manager_only="true"
      shift
      ;;
    --)
      shift
      if [[ -n "$@" ]]; then
        opt_unknown_args="true"
      fi
      break
      ;;
    *)
      prompt-error "[ERROR] Internal error!"
      exit 1
      ;;
  esac
done

[[ "$opt_help" == "true" ]] && help && exit 0
[[ "$opt_unknown_args" == "true" ]] && prompt-error "[ERROR] Unknown arguments $@" && exit 1
[[ "$opt_home_manager_only" == "true" ]] && setup-home-manager && exit 0
setup-os