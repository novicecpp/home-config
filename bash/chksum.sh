f_checksum_createall() {
    local algo algo_ext checksum_dir filename filename_ext filesum dirfile filepath
    algo=b3sum
    algo_ext=b3
    checksum_dir="${1:-.}"
    (
        find "${checksum_dir}" -type f -print0 | while IFS= read -r -d $'\0' filepath; do
            filename="$(basename "${filepath}")"
            filename_ext="${filename##*.}"
            filesum="${filename}.${algo_ext}"
            dirfile="$(dirname "${filepath}")"
            pushd "${dirfile}" > /dev/null || return
            # skip checksum file
            if [[ "${filename_ext}" == "${algo_ext}" ]]; then
                popd > /dev/null || return
                continue;
            # skip when checksum of file exist
            elif [[ -f "${filesum}" ]]; then
                echo "Skip \"${filepath}\"; checksum file already exists."
                popd > /dev/null || return
                continue;
            fi
            echo "${filepath}"
            ${algo} "${filename}" > "${filesum}";
            popd > /dev/null || return
    done
    )
}

f_checksum_checkall() {
    local algo algo_ext checksum_dir dirfile
    algo=b3sum
    algo_ext=b3
    checksum_dir=${1:-.}
    (
        find "${checksum_dir}" -type d -print0 | while IFS= read -r -d $'\0' dirfile; do
            pushd "${dirfile}" > /dev/null || return
            cat ./*.${algo_ext} | ${algo} -c
            popd > /dev/null || return
        done
    )
}
