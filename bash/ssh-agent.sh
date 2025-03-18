#! /bin/bash
pidfile=/run/user/1000/ssh-agent.pid
if [[ -f ${pidfile} ]]; then
    pid=$(cat /run/user/1000/ssh-agent.pid)
fi
if [[ ${pid-x} != "x" ]] && ps -p "$pid" > /dev/null; then
    SOCKPATH=$(find /tmp/ssh-* -name 'agent.*' -type s | grep "$((pid-1))")
    export SSH_AUTH_SOCK="${SOCKPATH}"
    export SSH_AGENT_PID=$pid
else
    eval "$(ssh-agent)"
    echo "$SSH_AGENT_PID" > ${pidfile}
    chmod 600 ${pidfile}
fi
