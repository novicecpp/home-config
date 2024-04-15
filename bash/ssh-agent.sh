#! /bin/bash

SOCKDIR=$(ls /tmp | grep -E 'ssh-[A-Za-z0-9]{12}' | head -1)
if [[ -z ${SOCKDIR} ]]; then
    eval "$(ssh-agent)"
else
    SOCKPATH=$(find /tmp/"${SOCKDIR}" -type s)
    export SSH_AUTH_SOCK="${SOCKPATH}"
    export SSH_AGENT_PID=$(($(cut -d'.' -f2 <<< $sockpath) + 1))
fi
