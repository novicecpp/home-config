f_allexport() {
    local filepath
    filepath=${1}
    set -o allexport
    source ${filepath}
    set +o allexport
}
