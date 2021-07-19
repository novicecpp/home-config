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
    if [[ $alphanum == 1 ]]; then
        < /dev/urandom tr -dc "A-Za-z0-9" | head -c ${num}; echo;
    else
        < /dev/urandom tr -dc "A-Za-z0-9_/+?\-@#%^()[]{}\." | head -c${1:-$num};echo;
    fi
}
