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
