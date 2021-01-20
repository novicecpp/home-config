#! /bin/bash
LOGFILE=emacs-batch.log
TEMPDIR=$(mktemp -d)
GTD_PATH='~/org/gtd/'
cd $TEMPDIR
mkdir $TEMPDIR/src/
emacs -l ~/.emacs.d/init.el --batch --kill \
      --eval '(setq org-startup-folded "nofold")' \
      --eval "(htmlize-many-files (directory-files-recursively \"$GTD_PATH\" \"\\.org$\" nil) \"$TEMPDIR/src/\")" \
      --eval '(set-frame-size (selected-frame) 176 99)' \
      --eval '(org-batch-store-agenda-views)'
rsync -r --delete-before ./ orgmode:/opt/org-export
rm -rf $TEMPDIR

