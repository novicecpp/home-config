#!/bin/bash
#
# flock boilerplate from Przemyslaw Pawelczyk <przemoc@gmail.com>
# https://stackoverflow.com/questions/1715137/what-is-the-best-way-to-ensure-only-one-instance-of-a-bash-script-is-running

### HEADER ###

LOCKFILE="$HOME/.bashhist_lock"
LOCKFD="99"

# PRIVATE
_lock()             { flock -$1 $LOCKFD; }
_no_more_locking()  { _lock u; _lock xn && rm -f $LOCKFILE; }
_prepare_locking()  { eval "exec $LOCKFD>\"$LOCKFILE\""; trap _no_more_locking EXIT; }

# ON START
_prepare_locking

# PUBLIC
exlock_now()        { _lock xn; }  # obtain an exclusive lock immediately or fail
exlock()            { _lock x; }   # obtain an exclusive lock
shlock()            { _lock s; }   # obtain a shared lock
unlock()            { _lock u; }   # drop a lock

### BEGIN OF SCRIPT ###

exlock
lockstart=$(date '+%Y%m%d_%H%M%S.%3N')

# backup, exit immediately if .bash_history is blank
datetime=$(printf '%(%Y%m%d_%H%M%S)T\n' -1)
backuppath="$HOME/.bashhist_backup/bash_history_$datetime"

if [[ ! -d "$(dirname "$backuppath")" ]]; then
    mkdir -p "$(dirname "$backuppath")"
fi
# stats path
# oneliner total line "num_line_before num_line_after"
statspath="$HOME/.bashhist_stats/$datetime"
if [[ ! -d "$(dirname "$statspath")" ]]; then
    mkdir -p "$(dirname "$statspath")"
fi

hist_content="$(cat ~/.bash_history)"
hist_current_linum=$(echo "$hist_content" | wc -l)
echo "$hist_content" > "$backuppath"

# dedup history
# https://unix.stackexchange.com/questions/48713/how-can-i-remove-duplicates-in-my-bash-history-preserving-order
# sort to unique command, then sort it back to original order.
echo "$hist_content" | nl | sed 's/[[:space:]]*$//' | sort -k2 -k1,1nr | uniq -f1 | sort -n | cut -f2 > ~/.bashhist
hist_new_linum=$(wc -l < ~/.bashhist)
threshold=$(bc -l <<< "${hist_new_linum}/${hist_current_linum}>0.8")
if [[ ${hist_new_linum} -gt 20 && ${threshold} -eq 0  ]]; then
    echo "Error: New .bash_history has 80% less line than previous version. Exit immediately"
    echo "$(date): ${backuppath} ${hist_new_linum} ${hist_current_linum}" >> ~/.bashhist_errors_timestamp
    exit 1
fi
history -c
history -r ~/.bashhist
history -w

lockend=$(date '+%Y%m%d_%H%M%S.%3N')
unlock
>"$statspath" echo "$hist_current_linum $hist_new_linum $lockstart $lockend"
