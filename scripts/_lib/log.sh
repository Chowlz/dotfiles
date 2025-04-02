#!/usr/bin/env bash

set -eu -o pipefail

declare -A __DOTFILES_SOURCED
[[ ${__DOTFILES_SOURCED[_lib/log.sh]:-false} != false ]] && return || __DOTFILES_SOURCED[_lib/log.sh]=true

declare log_level
declare -A log_levels=([TRACE]=0 [DEBUG]=1 [INFO]=2 [WARN]=3 [ERROR]=4)
declare -A log_level_labels=(
  [TRACE]="\e[0;32m[TRACE]\e[0m"
  [DEBUG]="\e[0;36m[DEBUG]\e[0m"
  [INFO]="\e[0;34m[INFO]\e[0m"
  [WARN]="\e[0;33m[WARN]\e[0m"
  [ERROR]="\e[0;31m[ERROR]\e[0m"
  [UNKNOWN]="\e[0;35m[???]\e[0m"
)
log_level=${LOG_LEVEL:-INFO}

# Default to WARN if log_level is invalid
if [[ ${log_levels[$log_level]:-false} == false ]]; then
  printf "%b | %-20b %s\n" \
    "\e[0;90m$(date --rfc-3339=seconds)\e[0m" \
    "${log_level_labels[WARN]}" \
    "Log level \"$log_level\" is invalid, defaulting to INFO" >&2
  log_level=WARN
fi

function log () {
  # Don't require -- after the priority
  local -a o=()
  for oo in "$@"; do
    shift
    o+=("$oo")
    # Stop at priority (first non-opt parameter will be considered the priority)
    [[ "${oo:0:1}" != "-" ]] && break
  done

  # Process args
  local -A opts
  local oo priority label
  local args=("$@")
  oo=$(getopt --name "log" -o m -l minimal -- "${o[@]}")
  eval set -- "$oo"
  while true; do
    case "$1" in
      # Used in arg validation
      -m|--minimal)
        # shellcheck disable=SC2154
        opts[minimal]=true
        shift
        ;;
      --)
        shift
        if [[ -n "$*" ]]; then
          # shellcheck disable=SC2034
          priority=$1
        fi
        break
        ;;
      *)
        echo "Internal logging error!"
        exit 1
        ;;
    esac
  done

  # Print message with ??? if priority is unknown
  if [[ ${log_levels[$priority]:-false} == false ]]; then
    label=${log_level_labels[UNKNOWN]}
  else
    # Ignore error if message priority is lower than set log_level
    (( ${log_levels[$priority]} < ${log_levels[$log_level]} )) && return 0
    label=${log_level_labels[$priority]}
  fi
  if [[ ${opts[minimal]:-false} == true ]]; then
    printf "%-20b %s\n" "$label" "${args[*]}" >&2
  else
    printf "%b | %-20b %b\n" "\e[0;90m$(date --rfc-3339=seconds)\e[0m" "$label" "${args[*]}" >&2
  fi
}
