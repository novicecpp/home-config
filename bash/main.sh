# disable ctrl-s to freeze tty
# https://stackoverflow.com/questions/24623021/getting-stty-standard-input-inappropriate-ioctl-for-device-when-using-scp-thro
[[ $- == *i* ]] && stty -ixon


export PS1='[\[\033[01;32m\]\u@\h\[\033[00m\] \W]\$ '
export EDITOR='emacsclient -c'
export HISTSIZE=10000
