_fzf_compgen_path() {
  fd --hidden . "$1"
}

_fzf_compgen_dir() {
  fd --type d --hidden . "$1"
}

# enable C-k in fzf
export FZF_DEFAULT_OPTS="--bind ctrl-k:kill-line"

# C-r to find history with fzf
# copy from https://github.com/junegunn/fzf/wiki/examples#command-history
bind '"\C-r": "\C-x1\e^\er"'
bind -x '"\C-x1": __fzf_history';

__fzf_history ()
{
__ehc $(history | fzf --tac --tiebreak=index | perl -ne 'm/^\s*([0-9]+)/ and print "!$1"')
}

__ehc()
{
if
        [[ -n $1 ]]
then
        bind '"\er": redraw-current-line'
        bind '"\e^": magic-space'
        READLINE_LINE=${READLINE_LINE:+${READLINE_LINE:0:READLINE_POINT}}${1}${READLINE_LINE:+${READLINE_LINE:READLINE_POINT}}
        READLINE_POINT=$(( READLINE_POINT + ${#1} ))
else
        bind '"\er":'
        bind '"\e^":'
fi
}
