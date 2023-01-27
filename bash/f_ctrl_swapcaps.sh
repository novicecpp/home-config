f_ctrl_swapcaps () {
    dconf dump / | grep 'ctrl:swapcaps' > /dev/null
    if [[ $? == 0 ]]; then
        dconf load / <<EOF
[org/gnome/desktop/input-sources]
xkb-options=['']
EOF
    else
        dconf load / <<EOF
[org/gnome/desktop/input-sources]
xkb-options=['ctrl:swapcaps']
EOF
    fi
}
