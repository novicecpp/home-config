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

f_source_env () {
    if [[ -f "$1" ]]; then
        set -a; eval $(cat "$1"); set +a;
    else
        echo "cat: $1: No such file or directory"
    fi
}


f_awk_cut () {
    awk "{print \$${1:-1}}"
}

f_awk_sum () {
    awk '{s+=$1} END {print s}'
}


f_open_gpg () {
    if [[ -f "$1" ]]; then
        timeout 180s emacs -Q $1 || xsel -cb
    else
        echo "$1: No such file or directory"
        return 1
    fi
}

f_ () {
    F_FUNCTION=$(declare -F | grep -P -- '-f f_.+' | cut -d' ' -f3)
    FUNCTION_NAME=$(printf '%s\n..\n.\n' "${F_FUNCTION[@]}" | fzf)
	(sleep 0.05 && tmux send-key "$FUNCTION_NAME" &)
}


f_ssh_add () {
    export SSH_ASKPASS=askpass.sh
    export SSH_ASKPASS_REQUIRE=force
    ssh-add "$1"
}

f_proc_environ () {
    if [[ $# != 1 ]]; then
        >&2 echo 'usage: f_proc_environ <pid>'
        return 1
    fi
    cat /proc/$1/environ | tr '\0' '\n'
}

f_pyenv_init() {
    export PYENV_ROOT="$HOME/.pyenv"
    command -v pyenv >/dev/null || export PATH="$PYENV_ROOT/bin:$PATH"
    eval "$(pyenv init -)"
}

# https://serverfault.com/a/755815
f_print_cert_bundle() {
    openssl crl2pkcs7 -nocrl -certfile $1 | openssl pkcs7 -print_certs -text -noout
}

f_rpm_gpg_pubkey() {
    rpm -q gpg-pubkey --qf '%{NAME}-%{VERSION}-%{RELEASE}\t%{SUMMARY}\n'
}


f_wine_explorer() {
   wine explorer /desktop=Line,1280x720
}

f_datetime() {
    printf '%(%Y%m%d_%H%M%S)T\n' -1
}
