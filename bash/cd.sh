# Copy from https://superuser.com/questions/299694/is-there-a-directory-history-for-bash/1395350#1395350
cdh () {
    local hnum=16;
    local new_dir index dir cnt bash_dirs_reverse bash_dirs;
    if ! [[ $# -eq 0 ]]; then
        # checking for "cd -- dir"
        # but why? some practice?
        if [[ $# -eq 2 && $1 = "--" ]]; then
            shift;
        else
            # use builtin cd if passing cd option like
            # "cd -L", "cd -L dir", but not this function's feature
            # like "cd -12", "cd dir", "cd --".
            if ! {
                [[ $# -eq 1 ]] && [[ $1 =~ ^(-[0-9]{,2}|-|--|[^-].*)$ ]]
            }; then
                builtin cd "$@";
                return;
            fi;
        fi;
    fi;
    # load dirs
    if [[ -f ~/.bashdirs ]]; then
        dirs -c
        bash_dirs=($(cat ~/.bashdirs))
        bash_dirs_reverse=($(printf '%s\n' "${bash_dirs[@]}" | tac | tr '\n' ' '; echo))
        for d in "${bash_dirs_reverse[@]}"; do
            if [[ "$PWD" == "$d" ]]; then
                continue
            fi
            pushd -n -- $d > /dev/null
        done
    fi

    [[ "$1" = "--" ]] && {
        dirs -v;
        return
    };
    new_dir=${1:-$HOME};
    if [[ "$new_dir" =~ ^-[0-9]{,2}$ ]]; then
        index=${new_dir:1};
        # check for "cd -"
        if [[ -z "$index" ]]; then
            new_dir=$OLDPWD;
        else
            new_dir=$(dirs -l +$index) || return;
        fi;
    fi;
    # NOTE: pushd always change dir
    pushd -- "$new_dir" > /dev/null || return;
    popd -n +$hnum &> /dev/null || true;
    # remove duplicate history
    new_dir=$PWD cnt=1;
    while dir=$(dirs -l +$cnt 2> /dev/null); do
        if [[ "$dir" = "$new_dir" ]]; then
            popd -n +$cnt > /dev/null;
            continue;
        fi;
        let cnt++;
    done
    # dump dirs
    dirs -l > ~/.bashdirs
}

cdr() {
    cd "$(realpath $1)"
}


cdg()
{
    cdh $(git rev-parse --show-toplevel)
}


cdd() {
    local INPUT_DIR SEARCH_STRING LSD DIR_SELECTED EXIT_CODE
    if [[ "$#" -eq 2 ]]; then
        SEARCH_STRING="${1}"
        INPUT_DIR="${2}"
    elif [[ "$#" -eq 1 ]]; then
        SEARCH_STRING="${1}"
        INPUT_DIR="."
    elif [[ "$#" -eq 0 ]]; then
        SEARCH_STRING="."
        INPUT_DIR="."
    else
        >&2 echo 'Error: wrong number argument.'
        >&2 echo "Usage: ${FUNCNAME} [REGEXP] DIR"
        return 1
    fi
    if [[ ! -d "${INPUT_DIR}" ]]; then
        >&2 echo "Error: ${INPUT_DIR}: No such file or directory."
        return 1
    fi
    while true; do
        LSD="$(cd "${INPUT_DIR}" && fd --type d "${SEARCH_STRING}" )"
        DIR_SELECTED="$(printf '%s\n..\n.\n' "${LSD[@]}" | fzf )"
        EXIT_CODE=$?
        INPUT_DIR="${INPUT_DIR}/${DIR_SELECTED}"
        if [[ $EXIT_CODE == 130 || ${DIR_SELECTED} == '.' ]]; then
            break
        fi
    done
    cdh "${INPUT_DIR}" &> /dev/null
}

cdf() {
    if [[ "$#" -eq 1 ]]; then
        FILEPATH=${1}
    else
        >&2 echo 'cdf Error: wrong number argument.'
        return 1
    fi
    cd "$(dirname "${FILEPATH}")" || return 1
}


alias cd=cdh
