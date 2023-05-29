#! /bin/bash
f_randpasswd() {
    local num arg alphanum OPTIND
    while getopts ":an:" arg
    do
        case "${arg}" in
            n)
                num=${OPTARG}
                ;;
            a)
                alphanum=1
                ;;
            *)
                echo "Usage: f_randpasswd [-a] [-n <int>]" 1>&2
                return 1
                ;;
        esac
    done
    shift $((OPTIND-1))

    if [[ -z "$num" ]]; then
        num=64
    fi
    while true; do
        if [[ $alphanum == 1 ]]; then
            genpass=$(< /dev/urandom tr -dc "A-Za-z0-9" | head -c ${num}; echo);
        else
            genpass=$(< /dev/urandom tr -dc "A-Za-z0-9_/+\-@\$#%\.\!" | head -c${1:-$num};echo;)
        fi
        check_regex='(.)\1'
        if [[ $genpass =~ $check_regex ]]; then
            continue
        else
            echo "$genpass"
            break
        fi
    done
}
