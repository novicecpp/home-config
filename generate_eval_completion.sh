#! /bin/bash
PREFIX_PATH=bash/generated

# pyenv
#PYENV_GENBASH_PATH=$PREFIX_PATH/pyenv.sh
#echo '' > $PYENV_GENBASH_PATH
#pyenv init - --no-rehash >> $PYENV_GENBASH_PATH

# kube
COMPLETION_FILENAME=$PREFIX_PATH/completion.sh
echo '' > $COMPLETION_FILENAME
kubectl completion bash >> $COMPLETION_FILENAME
helm completion bash >> $COMPLETION_FILENAME
kops completion bash >> $COMPLETION_FILENAME
if [ -f /opt/kubectx/completion/kubectx.bash ]; then
   ln -sf /opt/kubectx/completion/kubens.bash $PREFIX_PATH
   ln -sf /opt/kubectx/completion/kubectx.bash $PREFIX_PATH
fi
