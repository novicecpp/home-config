export PYENV_ROOT="$HOME/.local/share/pyenv"
if [ -d "$PYENV_ROOT" ]; then
    export PATH="$PYENV_ROOT/bin:$PATH"
    # from `pyenv init -`
    export PATH="$PYENV_ROOT/shims:${PATH}"
    export PYENV_SHELL=bash
    source "$PYENV_ROOT/libexec/../completions/pyenv.bash"
    pyenv() {
      local command
      command="${1:-}"
      if [ "$#" -gt 0 ]; then
        shift
      fi

      case "$command" in
      rehash|shell)
        eval "$(pyenv "sh-$command" "$@")";;
      *)
        command pyenv "$command" "$@";;
      esac
    }
fi
