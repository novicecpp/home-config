#! /bin/bash

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

# disable ctrl-s to freeze tty
# https://stackoverflow.com/questions/24623021/getting-stty-standard-input-inappropriate-ioctl-for-device-when-using-scp-thro
stty -ixon

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

# dedep history
SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
bash $SCRIPT_DIR/../dedup_hist.sh

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
    #PS1='[\[\033[01;32m\]\u@\h\[\033[00m\] \W]\$ '
    # make PS1 play nice when source venv via direnv
    # export line below need to put in every python's direnv
    #   export VIRTUAL_ENV_PROMPT_CUSTOM=${VIRTUAL_ENV_PROMPT}
    PS1='${VIRTUAL_ENV_PROMPT_CUSTOM:+$VIRTUAL_ENV_PROMPT_CUSTOM}[\[\033[01;32m\]\u@\h\[\033[00m\] \W]\$ '
fi

# default TERM to xterm for compatibility with server
export TERM=xterm
# it is default in some /etc/profile.d files, but explicitly set it anyway.
export COLORTERM=truecolor

# editor
export EDITOR='emacs'

# path in home-config
#PATH="${BASH_MYBASH_SCRIPT_DIR}"/bin:"$PATH"
#PATH="${BASH_MYBASH_SCRIPT_DIR}"/private/bin:"$PATH"

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
