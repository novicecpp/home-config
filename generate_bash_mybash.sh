#! /bin/bash
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"
echo "BASH_DIR=$DIR/bash" > $DIR/.bash_mybash
echo 'for f in $(ls $BASH_DIR/*.sh); do source $f; done' >> $DIR/.bash_mybash
echo 'for f in $(ls $BASH_DIR/generated/*.sh); do source $f; done' >> $DIR/.bash_mybash
echo 'for f in $(ls $BASH_DIR/generated/*.bash); do source $f; done' >> $DIR/.bash_mybash
ln -sf $DIR/.bash_mybash ~/.bash_mybash
