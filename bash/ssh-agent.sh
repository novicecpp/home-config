#! /bin/bash
#pidfile=/run/user/1000/ssh-agent.pid
#sockpath=/run/user/1000/ssh-agent.sock
#if [[ -f ${pidfile} ]]; then
#    pid=$(cat /run/user/1000/ssh-agent.pid)
#fi
#if [[ ${pid-x} != "x" ]] && ps -p "$pid" > /dev/null; then
#    export SSH_AUTH_SOCK="${SOCKPATH:-${sockpath}}"
#    export SSH_AGENT_PID=$pid
#else
#    eval "$(ssh-agent -a ${sockpath})"
#    echo "$SSH_AGENT_PID" > ${pidfile}
#    chmod 600 ${pidfile}
#fi
