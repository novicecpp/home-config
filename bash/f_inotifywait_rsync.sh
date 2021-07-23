f_inotifywait_rsync () {
    if [[ "$#" < 1 ]]; then
	    echo "Usage: f_inotifywait_rsync <path> <ssh_host:path>"
        return 1
    fi
    local EVENTS path
    # modify from: https://unix.stackexchange.com/questions/103858/inotify-and-rsync-on-large-number-of-files
    EVENTS="CREATE,DELETE,MODIFY,MOVED_FROM,MOVED_TO"
    path="$(realpath $1)"
    echo "path=$path"
    inotifywait -e "$EVENTS" -m -r --exclude 'flycheck_.+' --exclude '\.#.+' --format '%:e %f' $path | (
        last_sync=$EPOCHREALTIME
        sync_triggered=0
        while true ; do
            read -t 1 LINE
            if [[ $? == 0 && $sync_triggered == 0 ]]; then
                    sync_triggered=1
                    ts=$EPOCHREALTIME
            fi
            if [[ $sync_triggered == 1 && $(calc() { awk "BEGIN{print $*}"; }; calc $EPOCHREALTIME-$ts) > 1.00 ]]; then
                rsync --update -alvr --exclude '*.git*' $path $2
                echo rsync --update -alvr --exclude '*.git*' $path $2
                sync_triggered=0
            fi
        done
    )

#####
#    if [[ "$#" < 1 ]]; thens
#	    echo "Usage: f_inotifywait_rsync <ssh_host:path>"s
#        return 1
#    fi
#    local ssh_opts="$2"
#    IFS=':' read -ra ssh_args <<< "$1"
#    host=${ssh_args[0]}
#    path=${ssh_args[1]}
#    ssh $ssh_opts $host "ls $(dirname $path)"
#    exit_code=$?
#    if [[ "$exit_code" -eq 2 ]]; then
#	    echo "cannot access $path in host $ssh_args"
#	    return 1
#    elif [[ "$exit_code" -eq 255 ]]; then
#	    echo "cannot access host $ssh_args"
#	    return 1
#    fi
#    echo "sync current directory with $path"
#    rsync -e "ssh $ssh_opts" -av . "$1"
#    while inotifywait -r -e modify,create,delete,move .;
#    do
#	    rsync -e "ssh $ssh_opts" -av . "$1"
#    done
}
