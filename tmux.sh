#! /bin/bash
tmux_custom_split_windows () {
    x=$(tmux list-panes -F '#{pane_active} #{pane_pid} #{pane_current_command}' | grep -i '^1' | grep -v bash)
    >2 echo $x
    if [[ "$1" == "horizontal" ]]; then
	    tmux split-window -h -c "#{pane_current_path}"
    else
	    tmux split-window -c "#{pane_current_path}"
    fi
    if [[ -n "$x" ]]; then
	    w=$(echo $x | awk -F ' ' '{print $2}')
        y=$(ps -o ppid= -o pid= -A | awk "\$1 == $w{print \$2}")
	    z=$(ps -p $y -o command=)
	tmux send-key "$z"
    fi
}

tmux_copy_buffer_to_clipboard() {
    tmux show-buffer | wl-copy --type text/plain
}

tmux_open_remote_file() {
    if [[ $1 == "" ]]; then
        DEBUG_TARGET=""
        #DEBUG_GREP_PID="^1"
    else
        DEBUG_TARGET="-t $1"
        #DEBUG_GREP_PID="0 $1"
    fi
    GREP_STRING='^##emacslocal'
    CAPTURED=$(tmux capture-pane -p -J ${DEBUG_TARGET} | grep -- "${GREP_STRING}" | tail -n1)
    if [[ "$CAPTURED" == "" ]]; then
        return
    fi
    #PANE_PID=$(tmux list-panes -F '#{pane_active} #{pane_index} #{pane_pid}' | grep "$DEBUG_GREP_PID" | cut -d' ' -f3)
    #PROCESS_PID=$(ps -o ppid= -o pid= -A | awk "\$1 == ${PANE_PID}{print \$2}")
    #IFS=' ' read P_COMMAND P_ARGS <<< $(ps --pid ${PROCESS_PID} -o args=)

    #if [[ $P_COMMAND != "ssh" ]]; then
    #    return
    #fi
    #FILEPATH=$(echo ${CAPTURED} | awk '{print $2}')
    arglist=(${CAPTURED})
    P_ARGS=${arglist[1]}
    FILEPATH=${arglist[2]}
    emacsclient -cn "/ssh:${P_ARGS}:${FILEPATH}"
}

tmux_copy_shell_output_to_clipboard() {
    if [[ -z $DEBUG_PANE ]]; then
        DEBUG_TARGET=""
    else
        DEBUG_TARGET="-t $DEBUG_PANE"
    fi
    CAPTURED=$(tmux capture-pane -p -S -1000 ${DEBUG_TARGET})
    if [[ "$CAPTURED" == "" ]]; then
        return
    fi
    readarray -t GREP_OUTPUT_ARRAY <<< $(printf "%s" "$CAPTURED" | grep -n -P '\[.+@.+ .+\]' | tail -n2)
    FIRST=$(( ${GREP_OUTPUT_ARRAY[0]%%[:]*} + 1 ))
    LAST=$(( ${GREP_OUTPUT_ARRAY[1]%%[:]*} - 1 ))
    printf "%s\n" "$CAPTURED" | sed -n "${FIRST},${LAST}p" | xsel -ib
}
