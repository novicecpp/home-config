#! /bin/bash

f_pyenv_init() {
    export PYENV_ROOT="$HOME/.pyenv"
    command -v pyenv >/dev/null || export PATH="$PYENV_ROOT/bin:$PATH"
    eval "$(pyenv init -)"
}

f_pyenv_init_pipenv() {
    if [[ "$#" -ne 1 ]]; then
        echo 'Error: python version required.'
        echo "Usage: ${FUNCNAME[0]} <python_version>"
        return 1
    fi
    PY_VERSION=${1}
    if [[ -d .venv ]]; then
        echo 'Error: .venv exists.'
        return 1
    fi
    export PYENV_ROOT="$HOME/.pyenv"
    command -v pyenv >/dev/null || export PATH="$PYENV_ROOT/bin:$PATH"
    eval "$(pyenv init -)"
    pyenv shell "${PY_VERSION}" || rc=$?
    [[ $rc -ne 0 ]] && return "$rc"
    python -m venv .venv --prompt "$(basename "${PWD}")"
    source .venv/bin/activate
    pip install -U pip
    pip install pipenv
}


f_pyenv_pipenv_sync() {
    PY_VERSION=${1:-3.11}
    if [[ -d .venv ]]; then
        .venv/bin/activate
    else
        f_pyenv_init_pipenv "${PY_VERSION}"
    fi
    pipenv sync
}

f_pyenv_pipenv_source() {
    . .venv/bin/activate
}
