f_openssl_web() {
    local endpoint host port
    host=${1}
    port=${2:-443}
    endpoint=${host}:${port}
    echo | openssl s_client -connect ${endpoint}  2>/dev/null | openssl x509 -noout -text
}
