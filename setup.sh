#!/usr/bin/env bash

set -e

RCRC="${1:-$HOME/.dotfiles/rcrc}"
NODEJS_MODULES=(es-lint shadow-cljs source-map-support tslint typescript ws)

add-to-path () {
  while [ -n "$1" ]; do
    case ":$PATH:" in
      *":$1:"*) :;;
      *) PATH="$1:$PATH";;
    esac
    shift
  done
  export PATH
}

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

# Homebrew (MacOs only)
setup-homebrew () {
  if [[ "$(uname -s)" == "Darwin" ]]; then
    if ! which brew &> /dev/null; then
      prompt "Installing homebrew"
      /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
    fi

    if [ ! -e /opt/homebrew/opt/emacs-mac/Emacs.app ]; then
      question "Install emacs?" \
        "brew tap railwaycat/emacsmacport && brew install emacs-mac && ln -s /opt/homebrew/opt/emacs-mac/Emacs.app /Applications"
    fi

    if [ ! -e /Applications/Rectangle.app ]; then
      question "Install rectangle?" "brew install --cask rectangle"
    fi

    if [ ! -e /Applications/Spotify.app ]; then
      question "Install spotify?" "brew install --cask spotify"
    fi
  fi
}

# Nix (MacOs only)
setup-nix () {
  if [[ "$(uname -s)" == "Darwin" ]]; then
    if ! which nix-build &> /dev/null; then
      prompt "Installing nix package manager"
      curl -L https://nixos.org/nix/install | sh
    fi
  fi
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

# Nix-darwin
install-nix-darwin () {
  nix-build https://github.com/LnL7/nix-darwin/archive master.tar.gz -A installer
  if [ ! -e "/etc/shells.bak" ]; then
    echo "Moving /etc/shells to /etc/shells.bak"
    sudo mv /etc/shells /etc/shells.bak
  fi
  ./result/bin/darwin-installer
  rm -fr result
  darwin-rebuild switch -I darwin-config="$HOME/.config/nixpkgs/darwin-configuration.nix"
  rm -fr "$HOME/.nixpkgs"
  if [ -e  "/run/current-system/Applications/iTerm2.app" ]; then
    ln -s /run/current-system/Applications/iTerm2.app /Applications
  fi
}

update-nix-darwin () {
  nix-channel --update
  darwin-rebuild switch -I darwin-config="$HOME/.config/nixpkgs/darwin-configuration.nix"
  if [ -e  "/run/current-system/Applications/iTerm2.app" ]; then
    ln -s /run/current-system/Applications/iTerm2.app /Applications
  fi
}

setup-nix-darwin () {
  prompt "Setting up nix-darwin"
  if [[ "$(uname -s)" == "Darwin" ]]; then
    if ! which nix-env &> /dev/null; then
      question "Install nix darwin?" install-nix-darwin
    else
      question "Update nix-darwin?" update-nix-darwin
    fi
  fi
}

# Home-manager
install-home-manager () {
  nix-channel --add https://github.com/nix-community/home-manager/archive/master.tar.gz home-manager
  nix-channel --update
  nix-shell '<home-manager>' -A install
}

setup-home-manager () {
  prompt "Setting up home-manager"
  if ! which home-manager &> /dev/null; then
    question "Install home-manager?" install-home-manager
  else
    question "Update home-manager?" "nix-channel --update && home-manager switch"
  fi
}

# Doom-Emacs
install-doom-emacs () {
  if [ -L "$HOME/.emacs.d" ] || [ -d "$HOME/.emacs.d" ]; then
    cd "$HOME/.emacs.d"
    find . -name . -o -prune -exec rm -rf -- {} +
    git clone --depth 1 https://github.com/hlissner/doom-emacs .
    cd
  else
    git clone --depth 1 https://github.com/hlissner/doom-emacs "$HOME/.emacs.d"
  fi
  "$HOME/.emacs.d/bin/doom" install
  "$HOME/.emacs.d/bin/doom" sync
  "$HOME/.emacs.d/bin/doom" build
}

setup-doom-emacs () {
  prompt "Setting up doom-emacs .emacs.d"
  if [ -L "$HOME/.emacs.d" ] || [ -d "$HOME/.emacs.d" ]; then
    question ".emacs.d detected - install?" install-doom-emacs
  else
    install-doom-emacs
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

add-to-path /Users/ccruz/.nix-profile/bin
add-to-path /nix/var/nix/profiles/default/bin
add-to-path /run/current-system/sw/bin

setup-homebrew
setup-nix
setup-dotfiles
setup-nix-darwin
setup-home-manager
setup-doom-emacs
setup-nodejs

echo -e "\033[0;32mDone!\033[0m"
