export GIT_LFS_SKIP_SMUDGE=1

f_git_junk() {
    local NO_DRY_RUN
    NO_DRY_RUN=${1:-n}
    mkdir -p junk
    if [[ ${NO_DRY_RUN} == 'y' ]]; then
        git ls-files -o --exclude-standard --exclude='junk' | rsync -vR --remove-source-files --files-from=- . junk/
    else
        git ls-files -o --exclude-standard --exclude='junk' | rsync --dry-run -vR --remove-source-files --files-from=- . junk/
    fi
}
