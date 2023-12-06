#!/usr/bin/env bash

set -e

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

prompt-success () {
  echo -e "\033[0;32m$*\033[0m"
}

prompt-error () {
  echo -e "\033[0;31m$*\033[0m"
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

setup-dotfiles () {
  local rcrc_file
  local dotfiles_dir
  rcrc_file="$1"
  dotfiles_dir=$(dirname "$rcrc_file")
  prompt "# Setting up dotfiles - rcrc directory: $rcrc_file"
  nix-shell -p rcm --command "RCRC=$rcrc_file lsrc | sort | grep --color -E \"$dotfiles_dir|$\""
  question "Apply dotfiles?" "nix-shell -p rcm --command \"RCRC=$rcrc_file rcup\""
}