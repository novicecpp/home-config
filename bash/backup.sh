#! /bin/bash

f_backup() {
    local SRCDIR DSTDIR machine_name backup_path dir_name dirsize filename chksum_algo chksum_algo_ext hostname ignorepath_rel exclude_args p
    chksum_algo=b3sum
    chksum_algo_ext=b3
    if [[ $# != 2 ]]; then
       echo "Usage: "
       echo "bash backup.sh <src> <dst>"
       return 1
    fi
    hostname="$(hostnamectl hostname --static)"
    SRCDIR="${1}"
    # sanitize path to match --exclude in tar, to add `./` in front and remove `/` in back
    [[ "${SRCDIR}" != ./* && "${SRCDIR}" != /* ]] && SRCDIR="./${SRCDIR%/}"
    DSTDIR="${2}"
    DRY_RUN="${DRY_RUN-x}"
    machine_name=${MACHINE_NAME:-${hostname}}
    dir_name="${dir_name:-$(basename "${SRCDIR}")}"
    if [[ ! -d "${DSTDIR}" ]]; then
        >&2 echo "Error: backup path \"${DSTDIR}\" is not directory."
        return 1
    fi

    if [[ -z ${BACKUP_SECOND+x} ]]; then
        printf -v filename '%(%Y-%m-%d)T_%s_%s.tar.zst' -1 "${machine_name}" "${dir_name}"
    else
        printf -v filename '%(%Y-%m-%d_%H%M%S)T_%s_%s.tar.zst' -1 "${machine_name}" "${dir_name}"
    fi
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
    dirhsize=$(numfmt --to=iec-i --suffix=B --format="%.3f" ${dirsize})
    echo "${dirsize} bytes, ${dirhsize}"
    # ignore the encrypted loopback
    declare -a exclude_args
    while read -r p; do
        ignorepath_rel="$(realpath -s --relative-to="${SRCDIR}" "${p}")"
        exclude_args+=("--exclude ${SRCDIR}/${ignorepath_rel}")
    done <<< "$(df -hT | grep -v /dev/mapper/root | grep /dev/mapper | awk '{print $7}')"
    cmd="tar --ignore-failed-read ${exclude_args[*]} -cf - ${SRCDIR} | pv -brtp -s ${dirsize} | zstd -T0 - > ${backup_path}"
    if [[ ${DRY_RUN} != "x" ]]; then
       echo "This following command will get execute:"
       echo "${cmd}"
       return
    fi
    eval "$(echo "${cmd}" | paste -s -d " " -)"
    #tar --ignore-failed-read -cf - "${SRCDIR}" | pv -brtp -s "${dirsize}" | zstd -T0 - > "${backup_path}"
    echo "Running checksum..."
    (
        pushd "${DSTDIR%/}" > /dev/null || return
        ${chksum_algo} "${filename}" | tee "${filename}"."${chksum_algo_ext}"
        popd > /dev/null || return
    )
}
