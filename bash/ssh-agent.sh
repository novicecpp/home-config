#! /bin/bash

SOCKPATH=$(find /tmp/ssh-* -name 'agent.*' -type s | head -1)
if [[ -z ${SOCKPATH} ]]; then
    eval "$(ssh-agent)"
else
    export SSH_AUTH_SOCK="${SOCKPATH}"
    tmp=(${SOCKPATH/./ })
    agentpid=${tmp[1]}
    export SSH_AGENT_PID=$agentpid
fi
