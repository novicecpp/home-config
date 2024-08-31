#!/bin/bash
set -euo
if [[ -z ${TRACE+x} ]]; then
    set -x
    export TRACE
fi

# source variables from private vars path
source $PRIVATE_VARS_PATH/firefoxprofile.sh
# hardcode firefox profile path
PROFILE_PATH=~/.mozilla/firefox
for profile in "${FIREFOX_PROFILE[@]}"; do
    profilepath=$(find $PROFILE_PATH -name "*.${profile}" -type d)
    tar --zstd -cvf ${HOME}/myhome/firefox_profiles/${profile}.tar.zst ${profilepath}
done
