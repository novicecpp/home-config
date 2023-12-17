# disable ctrl-s to freeze tty
# https://stackoverflow.com/questions/24623021/getting-stty-standard-input-inappropriate-ioctl-for-device-when-using-scp-thro
[[ $- == *i* ]] && stty -ixon

# Dumb way to remove __vte_prompt_command from PROMPT_COMMAND
# It should be better way to prevent load it entirely
#PROMPT_COMMAND=$(echo $PROMPT_COMMAND | sed 's/__vte_prompt_command/ : /')

# save history immediately
# https://askubuntu.com/questions/67283/is-it-possible-to-make-writing-to-bash-history-immediate
shopt -s histappend
# do not save history when prefix comamnd with space
HISTCONTROL="ignorespace"
HISTSIZE=100000
PROMPT_COMMAND="history -a;"

# dedup history
# https://unix.stackexchange.com/questions/48713/how-can-i-remove-duplicates-in-my-bash-history-preserving-order

mv ~/.bashhist ~/.bashhist_previous
nl ~/.bash_history | sed 's/[[:space:]]*$//' | sort -k2 -k1,1nr | uniq -f1 | sort -n | cut -f2 > ~/.bashhist
bashhist_size=$(stat --printf="%s" ~/.bashhist)
bashhist_previous_size=$(stat --printf="%s" ~/.bashhist_previous)
if [[ "$bashhist_size" -le "$bashhist_previous_size" ]]; then
    datetime=$(printf '%(%Y%m%d_%H%M%S)T\n' -1)
    backuppath="$HOME/.bash_history_$datetime"
    backup_bashhist_path="$HOME/.bashhist_$datetime"
    >&2 echo "ERROR: ~/.bashhist is smaller than ~/.bashhist_previous"
    >&2 echo "Making ~/.bash_history backup at $backuppath"
    cp ~/.bash_history "$backuppath"
    >&2 echo "Making ~/.bashhist backup at $backup_bashhist_path"
    cp ~/.bash_history "$backup_bashhist_path"
fi
cp ~/.bashhist ~/.bash_history

# do not color PS1 when ssh connection (for localvm testing)
# https://stackoverflow.com/questions/3601515/how-to-check-if-a-variable-is-set-in-bash
# https://unix.stackexchange.com/questions/9605/how-can-i-detect-if-the-shell-is-controlled-from-ssh
# Also, if we are inside apptainer, do not override PS1.
if [[ -n ${SSH_CONNECTION} ]]; then
    :
elif [[ -d '/.singularity.d' ]]; then
    :
else
    export PS1='[\[\033[01;32m\]\u@\h\[\033[00m\] \W]\$ '
fi

export EDITOR='emacs'

# get ssh private passphrase from pass
# it does not work when ssh session need to hostkey confirmation or 2fa
#export SSH_ASKPASS_REQUIRE=prefer
#export SSH_ASKPASS=askpass.sh
