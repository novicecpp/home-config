#!/bin/bash
f_emacsclient_newtab() {
    emacsclient -n -e "(find-file-other-tab \"${1}\")"
}
