f_allexport_old() {
    local filepath
    filepath=${1}
    set -o allexport
    source ${filepath}
    set +o allexport
}

f_allexport() {
    local filepath dirpath tmpfile k v oldv
    filepath=$(realpath ${1})
    dirpath="$(dirname "${filepath}")"
    tmpfile=$(mktemp)
    # if var has suffix `_FILE`, make it to fullpath
    while IFS="" read -r line || [ -n "${line}" ];  do
        IFS='=' read -r -a tmp <<< "$line"
        k=${tmp[0]}
        v=${tmp[1]}
        if [[ $k == *_FILE ]]; then
            oldv=${v}
            v="${dirpath}/${oldv}"
        fi
        printf "%s=%s\n" "${k}" "${v}" >> "${tmpfile}"
    done < "${filepath}"
    set -o allexport
    source "${tmpfile}"
    set +o allexport
    rm -rf "${tmpfile}"
    export ENV_FILE_DIR="${dirpath}"
}
