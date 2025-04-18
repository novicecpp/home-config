#! /bin/bash

f_backup() {
    local SRCDIR DSTDIR MACHINE_NAME DIR_NAME DIRSIZE
    if [[ $# != 2 ]]; then
       echo "Usage: "
       echo "bash backup.sh <src> <dst>"
       return 1
    fi

    SRCDIR="${1}"
    DSTDIR="${2}"
    MACHINE_NAME=${MACHINE_NAME:-any}
    DIR_NAME="${DIR_NAME:-$(basename "$1")}"
    if [[ ! -d "${DSTDIR}" ]]; then
        >&2 echo "Error: backup path \"${DSTDIR}\" is not directory."
        return 1
    fi

    printf -v BACKUP_PATH '%s/%(%Y-%m-%d)T_%s_%s.tar.zstd' "${DSTDIR%/}" -1 "${MACHINE_NAME}" "${DIR_NAME}"

    echo "Backup from $SRCDIR to ${BACKUP_PATH}"
    echo "Getting directory size: "
    DIRSIZE=$(du -sb "${SRCDIR}" | awk '{print $1}' | xargs)
    echo "$DIRSIZE bytes"

    if [[ -n $DRY_RUN ]]; then
       echo "This following command will get execute:"
       echo "tar --ignore-failed-read -cf - \"${SRCDIR}\" | pv -brtp -s \"${DIRSIZE}\" | zstd -T0 - > \"${BACKUP_PATH}\""
       return
    fi
    tar --ignore-failed-read -cf - "${SRCDIR}" | pv -brtp -s "${DIRSIZE}" | zstd -T0 - > "${BACKUP_PATH}"
    echo "Running checksum..."
    sha256sum "${BACKUP_PATH}" | tee "${BACKUP_PATH}.sha256"
}
