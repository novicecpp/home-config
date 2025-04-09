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
