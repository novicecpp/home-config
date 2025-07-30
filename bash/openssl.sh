f_openssl_web() {
    local endpoint host port
    endpoint=${1}
    host="${endpoint%:*}"
    port="${endpoint#*:}"
    echo | openssl s_client -connect ${endpoint} -servername ${host} 2>/dev/null | openssl x509 -noout -text
}

f_openssl_inspect() {
    if [[ -p /dev/stdin ]]; then
       file=/dev/stdin
    else
       file=$1
    fi
    openssl x509 -in "${file}" -noout -text
}
