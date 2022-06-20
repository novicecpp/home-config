#! /bin/bash

# copy from `pyenv init -` command

PATH="$(bash --norc -ec 'IFS=:; paths=($PATH); for i in ${!paths[@]}; do if [[ ${paths[i]} == "'/home/thanayut/.local/share/pyenv/shims'" ]]; then unset '\''paths[i]'\''; fi; done; echo "${paths[*]}"')"
export PATH="/home/thanayut/.local/share/pyenv/shims:${PATH}"
export PYENV_SHELL=bash
source '/home/thanayut/.local/share/pyenv/libexec/../completions/pyenv.bash'
# disable rehash, it too slow
# command pyenv rehash 2>/dev/null
pyenv() {
  local command
  command="${1:-}"
  if [ "$#" -gt 0 ]; then
    shift
  fi

  case "$command" in
  rehash|shell)
    eval "$(pyenv "sh-$command" "$@")"
    ;;
  *)
    command pyenv "$command" "$@"
    ;;
  esac
}
