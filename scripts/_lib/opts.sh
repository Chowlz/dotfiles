#!/usr/bin/env bash

set -eu -o pipefail

declare -A __DOTFILES_SOURCED
[[ ${__DOTFILES_SOURCED[_lib/opts.sh]:-false} != false ]] && return || __DOTFILES_SOURCED[_lib/opts.sh]=true

declare lib_dir
lib_dir=$(cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd)

# shellcheck source=/dev/null
source "$lib_dir/log.sh"

function _lib/opts:show-help () {
  local help=$1
  local opt_help=$2

  if [[ "$opt_help" == true ]]; then
    $help
    exit 0
  fi
}

function _lib/opts:validate-type () {
  local command=$1

  if [[ "$command" == null ]]; then
    log --minimal ERROR "Missing type"
    exit 1
  fi
}

function _lib/opts:validate-unknown-args () {
  local unknown_args=("$@")

  if (( ${#unknown_args[@]} != 0 )); then
    log ERROR "Unknown arguments: ${unknown_args[*]}"
    exit 1
  fi
}

function _lib/opts:opts-ensure-configuration () {
  # shellcheck disable=SC2178
  local -n opt_ref=$1
  # shellcheck disable=SC2154
  opt_ref[configuration]=${opt_ref[configuration]:-}

  if [[ -z "${opt_ref[configuration]:-}" ]]; then
    log --minimal ERROR "Configuration (-c/--configuration) is required"
    exit 1
  fi
}
