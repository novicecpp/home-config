f_k8s_grep_image() {
    PODNAME_GREP=$1
    kubectl get pod -ojsonpath='{.items[*].spec.containers[*].image}' | sed 's/ /\n/g' | grep -P ${1:-.}
}
