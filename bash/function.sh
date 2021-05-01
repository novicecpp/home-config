#! /bin/bash

sshmux() {
    set -e
    local ipaddr=$(echo $(ssh "$1" "dig +short \"$2\" | grep '^[.0-9]*$'"))
    #ipaddr='10.251.1.161 10.251.1.44 10.251.1.23'
    read -ra ip <<< "$ipaddr"
    printf '%s\n' "${ip[@]}"
    local n=${#ip[@]}
    local split_cmd=$(for ((i=1;i<n;i++)); do echo -n ' split-window -v \; '; done)
    local sendkey_cmd=$(for ((i=0;i<n;i++)); do echo -n "send-key -t $((i+1)) \"ssh ${ip[i]}\" Enter \; "; done)
    local cmd="tmux new-window -a -t $(($(tmux display-message -p '#I') - 1 )) \; \
         $split_cmd \
         select-layout even-vertical \; \
         $sendkey_cmd \
         select-pane -t 1
    "
    eval "$cmd"
    #echo $cmd
}


f_randpasswd() {
    n=$([ -z "$1" ] && echo 64)
    < /dev/urandom tr -dc _A-Z-a-z-0-9@_A-Z-a-z-0-9'!@#$%^&*' | head -c${1:-$n};echo;
}

f_randpasswd_alphanum() {
    n=$([ -z "$1" ] && echo 64)
    < /dev/urandom tr -dc A-Za-z0-9 | head -c${1:-$n};echo;
}

f_init_agent () {

    sockdir=$(ls /tmp | grep -E 'ssh-[A-Za-z0-9]{12}' | head -1)
    if [[ -z $sockdir ]]; then
        eval $(ssh-agent)
    else
        local sockpath=$(find /tmp/$sockdir -type s)
        export SSH_AUTH_SOCK=$sockpath
        export SSH_AGENT_PID=$(($(cut -d'.' -f2 <<< $sockpath) + 1))
    fi
}
f_init_agent
f_digssh () {
    ssh bastion.wndv.co "dig +short $1"
}
# idea
## f_sshdig shell to target and ask option if dig +short provide more than 1 ip

# create tunnel with sshuttle and tunnel dns query only specific domain (by NM dnsmasq)
alias a_create_dns_tunnel_prod='f_tmux_dns_tunnel bastion 10.0.100.101 10.0.0.0/14 5300 22440 10.0.0.2'
f_tmux_dns_tunnel() {
    local SSH_HOST=$1
    local TUNNEL_HOST_IP=$2
    local SSH_TUNNEL_CIDR=$3
    local LOCAL_DNS_PORT=$4
    local TCP_TUNNEL_PORT=$5
    local REMOTE_DNS_HOST=$6
    if ! tmux has-session -t tunnel 2>/dev/null; then
        tmux new-session -s tunnel -d
    fi
    tmux new-window
    tmux send-key "sshuttle -v -r ${SSH_HOST} ${SSH_TUNNEL_CIDR}" Enter
    tmux split-window
    tmux send-key "socat -T5 -d -d udp4-listen:${LOCAL_DNS_PORT},reuseaddr,fork,bind=127.0.0.1 tcp:${TUNNEL_HOST_IP}:${TCP_TUNNEL_PORT}" Enter
    tmux split-window
    tmux send-key "ssh ${SSH_HOST} " Enter
    sleep 0.2
    tmux send-key "socat -T5 -d -d tcp4-listen:${TCP_TUNNEL_PORT},reuseaddr,fork,bind=${TUNNEL_HOST_IP} udp4:${REMOTE_DNS_HOST}:53" Enter
    sleep 0.2
    tmux move-window -t 'tunnel:'
    tmux switch-client -t tunnel
}

# bug bear
f_inotifywait_rsync () {
    if [[ "$#" < 1 ]]; then
	    echo "Usage: f_inotifywait_rsync <ssh_host:path>"
        return 1
    fi
    local ssh_opts="$2"
    IFS=':' read -ra ssh_args <<< "$1"
    host=${ssh_args[0]}
    path=${ssh_args[1]}
    ssh $ssh_opts $host "ls $(dirname $path)"
    exit_code=$?
    if [[ "$exit_code" -eq 2 ]]; then
	    echo "cannot access $path in host $ssh_args"
	    return 1
    elif [[ "$exit_code" -eq 255 ]]; then
	    echo "cannot access host $ssh_args"
	    return 1
    fi
    echo "sync current directory with $path"
    rsync -e "ssh $ssh_opts" -av . "$1"
    while inotifywait -r -e modify,create,delete,move .;
    do
	    rsync -e "ssh $ssh_opts" -av . "$1"
    done
}

f_sshdig() {
    #set -e
    local host=$1
    #https://stackoverflow.com/questions/11426529/reading-output-of-a-command-into-an-array-in-bash
    IFS=$'\n' read -r -d '' -a hostlist < <( f_digssh $host && printf '\0' )
    hostlen=${#hostlist[@]}
    if [[ "$hostlen" == 1 ]]; then
        hostssh=${hostlist[0]}
    else
        for ((i=0; i<$hostlen; i++))
        do
            printf '%d) %s\n' "$((i+1))" "${hostlist[i]}"
        done
        printf "Which host (1-${hostlen}): "
        IFS='\n' read -r hostnum
        hostssh=${hostlist[$((hostnum-1))]}
    fi
    tmux send-key "ssh $hostssh" Enter
}

f_sed_remove_comment_lines() {
    sed '/^[[:blank:]]*#/d;s/#.*//; /^[[:space:]]*$/d' $1
}


cdg()
{
    dir_path=$(git rev-parse --show-toplevel 2> /dev/null)
    if [[ "$?" == 0 ]]; then
        cd $dir_path
        select_dir=$(fd --hidden --type d . | fzf)
        command cd ${dir_path}/${select_dir}
    else
        echo 'Error. Not git directory.'
    fi
}


cdd() {
    cd "$1"
    [[ $? != 0 ]] && return 1
    while true; do
        local lsd="$(fd --type d . .)"
        local dir="$(printf '%s\n..\n.\n' "${lsd[@]}" | fzf )"
        if [[ ${dir} == '.' && ${#dir} != 0 ]]; then
            return 0
        fi
        builtin cd "$dir" &> /dev/null
    done
}

f_source_env () {
    if [[ -f "$1" ]]; then
        set -a; eval $(cat "$1"); set +a;
    else
        echo "cat: $1: No such file or directory"
    fi
}
