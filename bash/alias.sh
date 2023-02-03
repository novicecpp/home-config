alias a_tunnel_testing='sshuttle -v -r testing 10.251.0.0/16'
alias a_tunnel_bastion_shuttle='sshuttle -v -r bastion_shuttle 10.2.0.0/14'
alias a_tunnel_bastion_socket='autossh -M 0 -T -N -vv -D12345 bastion_socks5'
alias a_encrypt='gpg --armor --encrypt --sign -r '
alias a_xset='xset r rate 200 60'

alias e="emacsclient -t"
alias ec="emacsclient -c"
alias ecn="emacsclient -cn"
#alias emacs='e'

alias gpg='gpg2'

alias less='less -I'

alias r_pipewire='systemctl restart --user pipewire pipewire-pulse'
alias r_emacs='systemctl restart --user emacs'

alias diff='diff -u --color'

# alias a_gsts='gsts --aws-session-duration 43200 --aws-profile=default --aws-role-arn arn:aws:iam::005852626303:role/GSuite-Thanayut --sp-id 239920783439 --idp-id C029od40l --username thanayut@lmwn.com'
# deprecated
# start exwm with X server on term1
#alias startx="xinit -- vt01"

#alias eorg='/usr/local/bin/emacsclient -s /tmp/emacs1000/org -nc ~/org/gtd'
#alias plstorm='source ~/myhome/coding/storm/plstorm/env.sh'
