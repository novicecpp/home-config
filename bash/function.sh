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


cdg()
{
    cdh $(git rev-parse --show-toplevel)
}


cdd() {
    local dir lsd input_dir search_string
    if [[ "$#" -le 2 ]]; then
        SEARCH_STRING="${1:-.}"
        INPUT_DIR="${2:-.}"
    else
        echo 'wrong number argument.'
        return 1
    fi
    while true; do
        LSD="$(cd ${INPUT_DIR} && fd --type d ${SEARCH_STRING} )"
        DIR_SELECTED="$(printf '%s\n..\n.\n' "${LSD[@]}" | fzf )"
        EXIT_CODE=$?
        INPUT_DIR="$INPUT_DIR/$DIR_SELECTED"
        if [[ $EXIT_CODE == 130 || ${DIR_SELECTED} == '.' ]]; then
            break
        fi
    done
    cdh "${INPUT_DIR}" &> /dev/null
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


f_ssh_add_gpg () {
    gpg --decrypt $1 | ssh-add -
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


# Copy from https://superuser.com/questions/299694/is-there-a-directory-history-for-bash/1395350#1395350
cdh () {
    local hnum=16;
    local new_dir index dir cnt;
    if ! [[ $# -eq 0 ]]; then
        # checking for "cd -- dir"
        # but why? some practice?
        if [[ $# -eq 2 && $1 = "--" ]]; then
            shift;
        else
            # use builtin cd if passing cd option like
            # "cd -L", "cd -L dir", but not this function's feature
            # like "cd -12", "cd dir", "cd --".
            if ! {
                [[ $# -eq 1 ]] && [[ $1 =~ ^(-[0-9]{,2}|-|--|[^-].*)$ ]]
            }; then
                builtin cd "$@";
                return;
            fi;
        fi;
    fi;
    [[ "$1" = "--" ]] && {
        dirs -v;
        return
    };
    new_dir=${1:-$HOME};
    if [[ "$new_dir" =~ ^-[0-9]{,2}$ ]]; then
        index=${new_dir:1};
        # check for "cd -"
        if [[ -z "$index" ]]; then
            new_dir=$OLDPWD;
        else
            new_dir=$(dirs -l +$index) || return;
        fi;
    fi;
    # NOTE: pushd always change dir
    pushd -- "$new_dir" > /dev/null || return;
    popd -n +$hnum &> /dev/null || true;
    # remove duplicate history
    new_dir=$PWD cnt=1;
    while dir=$(dirs -l +$cnt 2> /dev/null); do
        if [[ "$dir" = "$new_dir" ]]; then
            popd -n +$cnt > /dev/null;
            continue;
        fi;
        let cnt++;
    done
}
cd() {
    cdh "$@"
}

cdr() {
    cd "$(realpath $1)"
}

# https://serverfault.com/a/755815
f_print_cert_bundle() {
    openssl crl2pkcs7 -nocrl -certfile $1 | openssl pkcs7 -print_certs -text -noout
}

f_rpm_gpg_pubkey() {
    rpm -q gpg-pubkey --qf '%{NAME}-%{VERSION}-%{RELEASE}\t%{SUMMARY}\n'
}
