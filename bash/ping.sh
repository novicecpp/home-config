#!/bin/bash
f_ping_notify() {

    set -u
    TARGET="${1}"

    until ping -c 1 -W 1 "$TARGET"; do
        sleep 1
    done

    notify-send -u normal -i network-reachable "Host reachable." "Successfully pinged $TARGET"
}


f_curl_notify() {

    set -u
    TARGET_TMP="${1}"
    PREFIX='https://'
    TARGET="${PREFIX}${TARGET_TMP#$PREFIX}"

    until curl -o /dev/null -v --connect-timeout 1s "$TARGET"; do
        sleep 1
    done

    # Send KDE Plasma notification once reachability succeeds
    notify-send -u normal -i network-reachable "HTTP reachable" "Successfully curl $TARGET"
}
