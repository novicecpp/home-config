# term

# use term command to ssh with tmux terminal
term () {
    if [[ "${1// }" ]]; then
        TMUX_TERM_NAME="${1}"
    fi
    if [[ -z "${TMUX// }" ]] && [[ $- == *i* ]] && [[ -n "${TMUX_TERM_NAME// }" ]]; then
        # create new tmux server with session name "init" if server has not started yet.
        tmux list-sessions -F '#{pid}' &> /dev/null
        if [[ $? -eq 1 ]]; then
            tmux new-session -d -s init
            [[ "$?" -ne 0 ]] && return 1
            tmux new-window -t init:1
        fi
        tmux has-session -t $TMUX_TERM_NAME &> /dev/null
        if [[ $? -eq 1 ]]; then
            tmux new-session -d -s $TMUX_TERM_NAME \; \
                 #new-window -t $TMUX_TERM_NAME:1 "emacsclient -t" \; \
                 #swap-window -s $TMUX_TERM_NAME:1 -t $TMUX_TERM_NAME:0
            tmux new-window -t $TMUX_TERM_NAME:1
            tmux attach-session -d -t $TMUX_TERM_NAME
        else
            #if [[ -z $(tmux list-clients -t $TMUX_TERM_NAME) ]]; then
            #    tmux attach-session -t $TMUX_TERM_NAME
            #else
            #    echo "Session $TMUX_TERM_NAME has already attached. Exit.."
            #fi
            tmux attach-session -d -t $TMUX_TERM_NAME
        fi
    fi
}

# open file in already open emacsclient in current session
tmux_emacsclient () {
    if [[ -z $TMUX ]]; then
        emacsclient -t $1
        echo 'Error. Not in TMUX session.'
        return 1
    fi

    if [[ -n "$1" ]]; then
        target="$1"
    else
        target="."
    fi

    found=false
    for i in $(tmux list-panes -t 0 -F "#{pane_pid},#{pane_id}");
    do
        pane_pid=${i%,*}
        pane_id=${i#*,}
        if [[ "$(pstree $pane_pid)" == *"emacsclient"*  ]]; then
            found=true
            break
        fi
    done

    if $found; then
        path=$([[ -f $target ]] && realpath $target || echo "")
        tmux send-keys -t $pane_id C-g C-g
        tmux send-keys -t $pane_id C-x C-f $path Enter
    else
        tmux new-window emacsclient -t $target
        tmux if-shell 'tmux move-window -t 0' '' 'swap-window -t 0'
    fi
    tmux select-window -t 0;

}

# swap to dedicated emacs-org window. using with tmux shortcut
tmux_show_org () {
    WINDOW_NUM=1322
    SESSION_NAME=ORGLIFE
    ORG_PATH=~/org/

    #check if in tmux session
    if [[ -z $TMUX ]]; then return 1; fi

    session_find=$(tmux list-sessions -F '#{session_name}' | grep -i $SESSION_NAME)
    if [[ -z $session_find ]]; then
        #create new session
        #SAVE_TMUX=$TMUX
        #unset TMUX
        env -i tmux new-session -d -s $SESSION_NAME \
             "emacsclient -t $ORG_PATH"
    fi

    window_number=$(tmux display-message -p '#I')
    if [[ $window_number == $WINDOW_NUM ]]; then
        export $(tmux show-environment -g TMUX_CUSTOM_PREVIOUS_WINDOW)
        tmux select-window -t $TMUX_CUSTOM_PREVIOUS_WINDOW
        tmux unlink-window -t $WINDOW_NUM
    else
        window_find=$(tmux list-windows -F "#{window_index}" | grep -i $WINDOW_NUM)
        if [[ -z $window_find ]]; then
            tmux link-window -d -s $SESSION_NAME:0 -t $WINDOW_NUM
        fi
        last_window=$(tmux display-message -p \
                           '#{window_index}.#{pane_index}')
        tmux set-environment -g TMUX_CUSTOM_PREVIOUS_WINDOW $last_window
        tmux select-window -t $WINDOW_NUM
    fi
}

# v2
tmux_show_org_v2 () {
    WINDOW_NUM=1322
    SESSION_NAME=ORGLIFE
    ORG_PATH=~/org/gtdv2

    #check if in tmux session
    if [[ -z $TMUX ]]; then return 1; fi

    session_find=$(tmux list-sessions -F '#{session_name}' | grep -i $SESSION_NAME)
    if [[ -z $session_find ]]; then
        #create new session
        env -i tmux new-session -d -s $SESSION_NAME \
             "emacsclient -t $ORG_PATH"
        env -i tmux set-option -t $SESSION_NAME status-bg red
    fi

    current_session=$(tmux display-message -p '#{session_name}')
    if [[ $current_session == $SESSION_NAME ]]; then
        export $(tmux show-environment TMUX_CUSTOM_PREVIOUS_SESSION)
        [[ -z $TMUX_CUSTOM_PREVIOUS_SESSION ]] && return 1
        tmux set-environment -g TMUX_CUSTOM_PREVIOUS_SESSION ""
        tmux switch-client -t $TMUX_CUSTOM_PREVIOUS_SESSION
    else
        tmux set-environment -g TMUX_CUSTOM_PREVIOUS_SESSION $current_session
        tmux switch-client -t $SESSION_NAME
    fi
}


tmux_swap_window () {
    set -o xtrace
    # https://stackoverflow.com/questions/806906/how-do-i-test-if-a-variable-is-a-number-in-bash
    ! [[ "$1" =~ ^[0-9]+$ ]] && return 1

    next_window=$1
    current_window=$(tmux display-message -p '#{window_index}')
    if [[ $current_window == $next_window ]]; then
        export $(tmux show-environment TMUX_CUSTOM_PREVIOUS_SWAP)
        tmux select-window -t :=$TMUX_CUSTOM_PREVIOUS_SWAP
    else
        tmux set-environment TMUX_CUSTOM_PREVIOUS_SWAP $current_window
        tmux select-window -t :=$next_window
    fi
    set +o xtrace
}

tmux_emacs_session() {
    set -o xtrace
    SESSION_NAME=EMACS
    SERVER_NAME=foo

    if [[ -n $TMUX ]]; then return 1; fi
    session_find=$(tmux list-sessions -F '#{session_name}' | grep -i $SESSION_NAME)
    if [[ -z $session_find ]]; then
        #echo "Start emacs server..."
        #emacs --eval "(setq server-name \"$SERVER_NAME\")" --daemon &> /dev/null
        tmux new-session -d -s $SESSION_NAME \
            "emacsclient -t $@"
            #"emacsclient -s $SERVER_NAME -t $@"
        tmux set-option -t $SESSION_NAME status-bg red
    fi
    set +o xtrace
}

emacs_global() {
    path=$PWD/$1
    SESSION_NAME=EMACS
    SERVER_NAME=foo
    if [[ $# != 1 ]]; then
        echo "Error: open more than one file and emacsclient option is not support yet."
        return 1
    fi

    #set -o xtrace
    session_find=$(tmux list-sessions -F '#{session_name}' | grep -i $SESSION_NAME)
    if [[ -n $session_find ]]; then
        current_command=$(tmux list-panes -t $SESSION_NAME:0 -F "#{pane_index}:#{pane_current_command}" 2>&1 | grep -i emacsclient | head -1)
        if [[ -z $current_command ]]; then
            tmux move-window -a -s $SESSION_NAME:0 -t $SESSION_NAME >/dev/null 2>&1
            tmux new-window -d -t $SESSION_NAME:0 emacsclient -t "$path"
            pane_index=1
        else
            pane_index=${current_command%:*}
            tmux send-keys -t $SESSION_NAME:0.$pane_index C-g C-g
            tmux send-keys -t $SESSION_NAME:0.$pane_index C-x C-f "$path" Enter
        fi
        tmux select-window -t $SESSION_NAME:0
        tmux select-pane -t $SESSION_NAME:0.$pane_index
    else
        echo "Error: no $SESSION_NAME session. Please start $SESSION_NAME session before call ${FUNCNAME[0]}"
    fi
    #set +o xtrace
}

t_reset_session () {
    # keep the current pane and kill all other pane and windows
    tmux kill-pane -a
    tmux kill-window -a
    tmux new-window
    tmux move-window -r
    tmux select-window -t 1
}

t_cp() {
    local file_src pane_no pane_current_path
    file_src=${1}
    pane_no=${2}
    re='^[0-9]+$'
    if [[ $pane_no =~ $re ]] ; then
        pane_target=":.${pane_no}"
    else
        pane_target=${pane_no}
    fi
    pane_current_path=$(tmux display-message -p -F "#{pane_current_path}" -t ${pane_target})
    cp -rp "${file_src}" "${pane_current_path}"
}
