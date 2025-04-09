_f_k8s_debug_completion() {
    local cur prev options actions used_options pod_name
    cur="${COMP_WORDS[COMP_CWORD]}"
    prev="${COMP_WORDS[COMP_CWORD-1]}"
    actions=$(kubectl get pods --no-headers -o custom-columns=":metadata.name" | tr '\n' ' ' 2> /dev/null)
    options="--target"
    pod_name=""
    for word in "${COMP_WORDS[@]}"; do
        case "${word}" in
            --target)
                used_options+=" ${word}"
                ;;
            *)
                :
                ;;
        esac
    done
    avaliable_options=""
    for opt in ${options}; do
        if [[ ! " ${used_options} " =~ " ${opt} " ]]; then
            avaliable_options+=" ${opt}"
        fi
    done
    if [[ "$COMP_CWORD" -ge 4 ]]; then
       COMPREPLY=()
       return
    fi
    COMPREPLY=($(compgen -W "${actions} ${avaliable_options}" -- "$cur"))
    pod_name="${COMP_WORDS[COMP_CWORD-2]}"
    if [[ "$prev" == "--target" ]]; then
        container_name=$(kubectl get pod ${pod_name}  -o jsonpath="{.spec.containers[*].name}")
        COMPREPLY=($(compgen -W "${container_name}" -- "$cur"))
    fi
}
complete -F _f_k8s_debug_completion f_k8s_debug
