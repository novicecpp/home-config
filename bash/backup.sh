#! /bin/bash

f_backup() {
    local SRCDIR DSTDIR machine_name backup_path dir_name dirsize filename chksum_algo chksum_algo_ext hostname
    chksum_algo=b3sum
    chksum_algo_ext=b3
    if [[ $# != 2 ]]; then
       echo "Usage: "
       echo "bash backup.sh <src> <dst>"
       return 1
    fi
    hostname="$(hostnamectl hostname --static)"
    SRCDIR="${1}"
    DSTDIR="${2}"
    DRY_RUN="${DRY_RUN-x}"
    machine_name=${MACHINE_NAME:-${hostname}}
    dir_name="${dir_name:-$(basename "${SRCDIR}")}"
    if [[ ! -d "${DSTDIR}" ]]; then
        >&2 echo "Error: backup path \"${DSTDIR}\" is not directory."
        return 1
    fi

    printf -v filename '%(%Y-%m-%d)T_%s_%s.tar.zst' -1 "${machine_name}" "${dir_name}"
    backup_path="${DSTDIR%/}/${filename}"

    echo "Backup from $SRCDIR to ${backup_path}"
    if [[ -f "${backup_path}" && "${DRY_RUN}" == "x" ]]; then
        echo "Backup file exist: ${backup_path}"
        read -r -p "Override? (Y/n): " choice
        if [[ "$choice" != [Y] ]]; then
            echo "Exit..."
            return
        fi
    fi
    echo "Getting directory size: "
    dirsize=$(du -sb "${SRCDIR}" | awk '{print $1}' | xargs)
    echo "${dirsize} bytes"

    if [[ ${DRY_RUN} != "x" ]]; then
       echo "This following command will get execute:"
       echo "tar --ignore-failed-read -cf - \"${SRCDIR}\" | pv -brtp -s \"${dirsize}\" | zstd -T0 - > \"${backup_path}\""
       return
    fi
    tar --ignore-failed-read -cf - "${SRCDIR}" | pv -brtp -s "${dirsize}" | zstd -T0 - > "${backup_path}"
    echo "Running checksum..."
    (
        pushd "${DSTDIR%/}" > /dev/null || return
        ${chksum_algo} "${filename}" | tee "${filename}"."${chksum_algo_ext}"
        popd > /dev/null || return
    )
}
