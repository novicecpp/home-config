#! /bin/bash
s_kube_config () {
    if [[ -e "$HOME/.kube/conf.d" ]]; then
        KUBECONFIG=$KUBECONFIG:$(fd '\.(yaml|yml)$' ~/.kube/conf.d/ | tr '\n' ':';)
        # dedup KUBECONFIG
        KUBECONFIG="$(KUBECONFIG=${KUBECONFIG} perl -e 'print join(":", grep { not $seen{$_}++ } split(/:/, $ENV{KUBECONFIG}))')"
        export KUBECONFIG
    fi
}
s_kube_config

f_k8s_grep_image() {
    kubectl get pod -ojsonpath='{.items[*].spec.containers[*].image}' | sed 's/ /\n/g' | grep -P "${1:-.}"
}

h_k8s_ctx() {
    local kubeconfig_dir
    # $HOME_KEYS_DIR is defined in private/hidden_vars.sh
    if [[ $# -ne 1 ]]; then
        echo "Usage: $FUNCNAME <machine_name>"
        return 1
    fi
    kubeconfig_dir="${HOME_KEYS_DIR}/${1}/kubeconfig"
    if [[ ! -d "${kubeconfig_dir}" ]]; then
        >&2 printf "Cannot access \'%s\': No such file or directory.\n" "${kubeconfig_dir}"
        return 1
    fi
    &> /dev/null unlink "${HOME}"/.kube/conf.d
    ln -s "${kubeconfig_dir}" "${HOME}"/.kube/conf.d
    ls ~/.kube/ -alh --color=force | grep --color=never conf.d
    set -x
    unset KUBECONFIG
    s_kube_config
    set +x
}

f_k8s_delete_pod() {
    pattern=${1:-.}
    shift
    filtered=$(kubectl "$@" get pod --no-headers -o custom-columns=":metadata.name" | grep ${pattern})
    if [[ $? != 0 ]]; then
        >&2 echo "Error: not found pod with pattern \"${pattern}\"."
        return 1
    fi
    pods="$(echo "${filtered}" | tr '\n' ' ')"
    echo >&2 "Pod(s) to delete: ${pods}"
    kubectl "$@" delete pod ${pods}
}

f_k8s_get_obj_crd() {
    GREP_REGEXP=${1:-issuers.cert-manager.io}
    for i in $(kubectl get crd --no-headers -o custom-columns=":metadata.name" | grep -E "${GREP_REGEXP}"); do
        echo '---'
        echo $i
        kubectl get $i -A;
    done
}

f_k8s_view_secret() {
    secret=${1}
    kubectl get secret -o yaml $secret | yq -j '.data' | jq 'with_entries(.value |= @base64d)'
}


f_kubectl() {
    if [[ -n $KUBECONTEXT ]]; then
        command kubectl --context $KUBECONTEXT "$@"
    else
        command kubectl "$@"
    fi
}

alias kubectl="f_kubectl"
