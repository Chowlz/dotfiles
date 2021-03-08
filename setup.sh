#!/usr/bin/env bash

set -eu

cd $HOME

# Nix
nix-channel --update

# Doom-Emacs
setup-emacs () {
  rm -fr ~/.emacs.d
  git clone --depth 1 https://github.com/hlissner/doom-emacs ~/.emacs.d
  ~/.emacs.d/bin/doom install
  ~/.emacs.d/bin/doom sync
  ~/.emacs.d/bin/doom build
}

echo "Setting up doom-emacs .emacs.d"
if [[ -L ~/.emacs.d && -d ~/.emacs.d ]]; then
  while true; do
      read -p ".emacs.d detected - skip? (yes/no)" yn
      case $yn in
          [Yy]es) setup-emacs ;;
          [Nn]o)  echo "Skipping" && break ;;
          * )     echo "Please answer yes or no." ;;
      esac
  done
else
  setup-emacs
fi

# Nodejs deps
# setup-npm () {
#   npm install -g clj-kondo es-lint shadow-cljs source-map-support tslint typescript ws
# }