#! /bin/bash
tmux_custom_split_windows () {
    x=$(tmux list-panes -F '#{pane_active} #{pane_pid} #{pane_current_command}' | grep -i '^1' | grep -v bash)
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
    tmux show-buffer | xsel -ib
}
