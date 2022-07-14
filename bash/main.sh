# disable ctrl-s to freeze tty
# https://stackoverflow.com/questions/24623021/getting-stty-standard-input-inappropriate-ioctl-for-device-when-using-scp-thro
[[ $- == *i* ]] && stty -ixon

# save history immediately
# https://askubuntu.com/questions/67283/is-it-possible-to-make-writing-to-bash-history-immediate
shopt -s histappend
export PROMPT_COMMAND="history -a; $PROMPT_COMMAND"

# dedup history
# https://unix.stackexchange.com/questions/48713/how-can-i-remove-duplicates-in-my-bash-history-preserving-order
nl ~/.bash_history | sed 's/[[:space:]]*$//' | sort -k2 -k1,1nr | uniq -f1 | sort -n | cut -f2 > ~/.bashhist
cp ~/.bashhist ~/.bash_history
history -c
history -r

# do not save history when prefix comamnd with space
export HISTCONTROL="ignorespace"

# do not color PS1 when ssh connection (for localvm testing)
# https://stackoverflow.com/questions/3601515/how-to-check-if-a-variable-is-set-in-bash
# https://unix.stackexchange.com/questions/9605/how-can-i-detect-if-the-shell-is-controlled-from-ssh
if [[ -z ${SSH_CONNECTION} ]]; then
    export PS1='[\[\033[01;32m\]\u@\h\[\033[00m\] \W]\$ '
else
    export PS1='[\u@\h \W]\$ '
fi
export EDITOR='emacs'
export HISTSIZE=10000
