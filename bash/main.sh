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

# backup, exit immediately if .bash_history is blank
datetime=$(printf '%(%Y%m%d_%H%M%S)T\n' -1)
backuppath="$HOME/.bash_history_$datetime"
hist_content="$(cat ~/.bash_history)"
linum=$(echo "$hist_content" | wc -l)
if [[ $linum -lt 5 ]]; then
    >&2 echo "\~/.bash_history has line number less than 5 [${linum}]. Exit immediately"
    return 1
fi
echo "$hist_content" > "$backuppath"

# dedup history
# https://unix.stackexchange.com/questions/48713/how-can-i-remove-duplicates-in-my-bash-history-preserving-order
# sort to unique command, then sort it back to original order.
echo "$hist_content" | nl | sed 's/[[:space:]]*$//' | sort -k2 -k1,1nr | uniq -f1 | sort -n | cut -f2 > ~/.bashhist
cp ~/.bashhist ~/.bash_history

# do not color PS1 when ssh connection (for localvm testing)
# https://stackoverflow.com/questions/3601515/how-to-check-if-a-variable-is-set-in-bash
# https://unix.stackexchange.com/questions/9605/how-can-i-detect-if-the-shell-is-controlled-from-ssh
# Also, if we are inside apptainer, do not override PS1.
if [[ -n ${SSH_CONNECTION} ]]; then
    :
# also inside singularity
elif [[ -d '/.singularity.d' ]]; then
    :
else
    PS1='[\[\033[01;32m\]\u@\h\[\033[00m\] \W]\$ '
fi

# default TERM to xterm for compatibility with server
export TERM=xterm
# it is default in some /etc/profile.d files, but explicitly set it anyway.
export COLORTERM=truecolor

# editor
export EDITOR='emacs'

# path in home-config
PATH="${BASH_MYBASH_SCRIPT_DIR}"/bin:"$PATH"
PATH="${BASH_MYBASH_SCRIPT_DIR}"/private/bin:"$PATH"

# dedup PATH
# https://stackoverflow.com/questions/44232009/how-to-handle-duplicates-in-my-path-variable
PATH="$(perl -e 'print join(":", grep { not $seen{$_}++ } split(/:/, $ENV{PATH}))')"
export PATH

# local bash completion
if [[ -d ~/.bash_completion.d ]]; then
  for bcfile in ~/.bash_completion.d/*.bash ; do
    source "${bcfile}"
  done
fi
