#!/usr/bin/env bash

set -eu

RCRC="${1:-$HOME/.dotfiles/rcrc}"
NODEJS_MODULES=(es-lint shadow-cljs source-map-support tslint typescript ws)

prompt () {
  echo -e "\033[0;35m$*\033[0m"
}

question () {
  local question_prompt="$1"
  local action="$2"
  while true; do
    read -p "$question_prompt (y/n) " -r yn
    case $yn in
      [Yy]) eval "$action" && break ;;
      [Nn]) echo -e "\033[1;33mSkipping\033[0m" && break ;;
      * )   echo "Please answer yes or no." ;;
    esac
  done
}

# Dotfiles
setup-dotfiles () {
  prompt "Setting up dotfiles - rcrc directory: $RCRC"
  local dotfiles_dir
  dotfiles_dir=$(dirname "$RCRC")
  if which lsrc &> /dev/null; then
    RCRC=$RCRC lsrc | sort | grep --color -E "$dotfiles_dir|$"
    question "Apply dotfiles?" "RCRC=$RCRC rcup"
  else
    nix-env -iA nixpkgs.rcm
    RCRC=$RCRC lsrc | sort | grep --color -E "$dotfiles_dir|$"
    question "Apply dotfiles?" "RCRC=$RCRC rcup"
    nix-env -e rcm
  fi
}

# Home-manager
setup-home-manager () {
  prompt "Setting up home-manager"
  question "Update home-manager?" "nix-channel --update && home-manager switch"
}

# Doom-Emacs
install-emacs () {
  rm -fr ~/.emacs.d
  git clone --depth 1 https://github.com/hlissner/doom-emacs ~/.emacs.d
  ~/.emacs.d/bin/doom install
  ~/.emacs.d/bin/doom sync
  ~/.emacs.d/bin/doom build
}

setup-emacs () {
  prompt "Setting up doom-emacs .emacs.d"
  if [ -L "$HOME/.emacs.d" ] || [ -d "$HOME/.emacs.d" ]; then
    question ".emacs.d detected - skip?" install-emacs
  else
    install-emacs
  fi
}

# Nodejs
# shellcheck disable=SC2145
setup-nodejs () {
  prompt "Setting up nodejs"
  echo "Modules: ${NODEJS_MODULES[@]}"
  question "Install npm modules?" "npm install -g ${NODEJS_MODULES[@]}"
}

# Bootstrap
cd "$HOME"

setup-dotfiles
setup-home-manager
setup-emacs
setup-nodejs

echo -e "\033[0;32mDone!\033[0m"
