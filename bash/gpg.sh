f_open_gpg () {
    if [[ -f "$1" ]]; then
        timeout 180s emacs -Q $1 || xsel -cb
    else
        echo "$1: No such file or directory"
        return 1
    fi
}

c_gpg_ctx() {
    local gpghome_dir
    if [[ $# -ne 1 ]]; then
        echo "Usage: $FUNCNAME <machine_name>"
        return 1
    fi
    # $HOME_KEYS_DIR is defined in private/hidden_vars.sh
    if [[ ${HOME_KEYS_DIR+x} != 'x' ]]; then
        >&2 echo "Error: HOME_KEYS_DIR is not defined."
        return 1
    fi
    gpghome_dir="${HOME_KEYS_DIR}/${1}/gnupg"
    if [[ ! -d "${gpghome_dir}" ]]; then
        >&2 printf "Error: Cannot access \'%s\': No such file or directory.\n" "${gpghome_dir}"
        return 1
    fi
    &> /dev/null unlink "${HOME}"/.gnupg
    ln -s "${gpghome_dir}" "${HOME}"/.gnupg
    ls "${HOME}"/ -alh --color=force | grep --color=never .gnupg
}
