# shellcheck disable=SC2119,SC2120
f_openssl_inspect() {
    local file
    if [[ -p /dev/stdin ]]; then
       file=/dev/stdin
    else
       file=${1}
    fi
    openssl crl2pkcs7 -nocrl -certfile "$file" | openssl pkcs7 -print_certs -text -noout
}

f_openssl_s_client () {
    local endpoint host port pattern
    endpoint=${1}
    host="${endpoint%:*}"
    pattern='^[a-z.]+\:[0-9]+$'
    if [[ ${endpoint} =~ $pattern ]]; then
        port="${endpoint#*:}"
    else
        port=443
    fi
    echo | openssl s_client -connect ${host}:${port} -servername ${host} -showcerts
}

f_openssl_inspect_web() {
    local endpoint
    endpoint=${1}
    f_openssl_s_client ${endpoint} | awk '/-----BEGIN CERTIFICATE-----/,/-----END CERTIFICATE-----/' | f_openssl_inspect
}


f_openssl_inspect_k8s_secret() {
    secretname=${1}
    kubectl get secret -o jsonpath='{.data.tls\.crt}' ${secretname} | base64 -d | f_openssl_inspect
}

f_openssl_inspect_csr() {
    openssl req -text -noout -verify -in ${1}
}
