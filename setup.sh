#!/usr/bin/env bash

set -euo pipefail

script=$(basename "$0")
root_dir=$(cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd)

help () {
  echo "$script - setup dotfiles"
  echo ""
  echo "$script [options]"
  echo ""
  echo "options:"
  echo "-h, --help                    show brief help"
  echo "-t, --type                    nix setup (valid values: nixos, nix-darwin, home-manaager)"
  echo "-c, --configuration           nixos/nix-darwin/home-manager configuration"
}

prompt () {
  echo -e "\033[0;35m$*\033[0m"
}

prompt-success () {
  echo -e "\033[0;32m$*\033[0m"
}

prompt-error () {
  echo -e "\033[0;31m[ERROR] $*\033[0m"
}

question () {
  local question_prompt="$1"
  local action="$2"
  while true; do
    read -p "$question_prompt (y/n) " -r yn
    case $yn in
      [Yy])
        eval "$action"
        break
        ;;
      [Nn])
        echo -e "\033[1;33mSkipping\033[0m"
        break
        ;;
      *)
        echo "Please answer yes or no."
        ;;
    esac
  done
}

set-type () {
  type="$1"
}

set-configuration () {
  configuration="$1"
}

setup-homebrew () {
  if ! which brew &> /dev/null; then
    prompt "# Installing homebrew..."
    /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
  else
    prompt "# homebrew detected. Skipping install..."
  fi
  if [ ! -e /Applications/Rectangle.app ]; then
    question "# Install rectangle?" "brew install --cask rectangle"
  else
    prompt "# Rectangle detected. Skipping install..."
  fi
  if [ ! -e /Applications/Spotify.app ]; then
    question "# Install spotify?" "brew install --cask spotify"
  else
    prompt "# Spotify detected. Skipping install..."
  fi
}

setup-nix-package-manager () {
  if ! which nix-build &> /dev/null; then
    prompt "# Installing nix package manager..."
    curl -L https://nixos.org/nix/install | sh
  else
    prompt "# Nix package manager detected. Skipping install..."
  fi
}

setup-nix-conf () {
  nix_dir=$HOME/.config/nix
  nix_conf=$nix_dir/nix.conf
  prompt "# Writing \"$nix_conf\" for flakes..."
  mkdir -p "$nix_dir"
  echo "experimental-features = nix-command flakes" > "$nix_conf"
}

setup-nix-darwin () {
  configuration="$1"
  # TODO: Update and test with flake
  if ! which darwin-rebuild &> /dev/null; then
    prompt "# Installing nix-darwin..."
    # if [ ! -e "/etc/shells.bak" ]; then
    #   echo "Moving /etc/shells to /etc/shells.bak"
    #   sudo mv /etc/shells /etc/shells.bak
    # fi
    nix run nix-darwin -- switch --flake "path:$root_dir#$configuration"
  else
    prompt "# Updating nix-darwin..."
    darwin-rebuild switch --flake "path:$root_dir#$configuration"
  fi

  if [ -e  "/run/current-system/Applications/iTerm2.app" ]; then
    ln -s /run/current-system/Applications/iTerm2.app /Applications
  fi
}

setup-nixos () {
  configuration="$1"
  prompt "# Updating nixos with configuration \"$configuration\"..."
  sudo nixos-rebuild switch --flake "path:$root_dir#$configuration"
}

setup-home-manager () {
  configuration="$1"
  prompt "# Updating home-manager with configuration \"$configuration\"..."
  home-manager switch --flake "path:$root_dir/#$configuration"
}

# Process args
opt_help=""
opt_unknown_args=""
opt_type=""
opt_configuration=""
opts=$(getopt -o h,t:,c: --long help,type:,configuration: -- "$@")
eval set -- "$opts"
while true; do
  case "$1" in
    -h|--help)
      opt_help="true"
      shift
      ;;
    -t|--type)
      opt_type="$2"
      shift 2
      ;;
    -c|--configuration)
      opt_configuration="$2"
      shift 2
      ;;
    --)
      shift
      if [[ -n "$*" ]]; then
        opt_unknown_args="true"
      fi
      break
      ;;
    *)
      prompt-error "Internal error!"
      exit 1
      ;;
  esac
done

# Arg validation
[[ "$opt_help" == "true" ]] && help && exit 0
[[ "$opt_unknown_args" == "true" ]] && prompt-error "Unknown arguments ${opt_unknown_args[*]}" && exit 1
[[ -z "$opt_type" ]] && prompt-error "Missing type" && exit 1
[[ -z "$opt_configuration" ]] && prompt-error "Missing configuration" && exit 1

set-type "$opt_type"
set-configuration "$opt_configuration"

case "$type" in
  nix-darwin)
    setup-homebrew
    setup-nix-package-manager
    prompt-error "Refactor does not cover setup-nix-darwin" && exit 1
    setup-nix-darwin "$configuration"
    ;;
  nixos)
    setup-nixos "$configuration"
    ;;
  home-manager)
    setup-nix-package-manager
    setup-nix-conf
    setup-home-manager "$configuration"
    ;;
  *)
    prompt-error "invalid type (valid values: nixos, nix-darwin, home-manaager)"
    exit 1
    ;;
esac

prompt-success "Done!"