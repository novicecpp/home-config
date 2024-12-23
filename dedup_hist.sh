#!/bin/bash
## backup, exit immediately if .bash_history is blank
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
if [[ ${hist_new_linum} -gt 20 && ${threshold} -eq 0  && ! ${hist_current_linum} -le 100 ]]; then
    echo "Error: New .bash_history has 80% less line than previous version. Exit immediately"
    echo "$(date): ${backuppath} ${hist_new_linum} ${hist_current_linum}" >> ~/.bashhist_errors_timestamp
    exit 1
fi
cp ~/.bashhist ~/.bash_history

>"$statspath" echo "$hist_current_linum $hist_new_linum"
