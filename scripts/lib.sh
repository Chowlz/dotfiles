#!/usr/bin/env bash

set -eu -o pipefail

declare -A __DOTFILES_SOURCED
[[ ${__DOTFILES_SOURCED[lib.sh]:-false} != false ]] && return || __DOTFILES_SOURCED[lib.sh]=true

declare lib_dir
lib_dir=$(cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd)/_lib

# shellcheck source=/dev/null
source "$lib_dir/log.sh"
# shellcheck source=/dev/null
source "$lib_dir/opts.sh"
