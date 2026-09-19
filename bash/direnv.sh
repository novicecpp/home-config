#! /bin/bash
eval "$(direnv hook bash)"

f_direnv_venv_init() {
    git_common_dir="$(dirname "$(git rev-parse --git-common-dir)")"
    WORKTREE_DIR=${1:-${git_common_dir:-.}}
    if [[ -e .envrc ]]; then
        2>&1 echo "Error: .envrc exists"
        cat .envrc
        return 1
    fi
    cat << EOF > .envrc
WORKTREE_DIR=${WORKTREE_DIR}

if [[ -f \${WORKTREE_DIR}/.venv/bin/activate ]]; then
    source \${WORKTREE_DIR}/.venv/bin/activate
    export DIRENV_PROMPT_PREFIX="(\${VIRTUAL_ENV_PROMPT}) "
fi
EOF
}
