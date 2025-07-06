;; -*- mode: emacs-lisp; lexical-binding: nil; -*-
(use-package org
  :ensure t
  :config
  (setq org-startup-indented t)
  (global-set-key (kbd "C-c l") 'org-store-link)
  (global-set-key (kbd "C-c c") 'org-capture)
  (global-set-key (kbd "C-c a") 'org-agenda)
  (setq org-hide-emphasis-markers t)
  (setq org-agenda-start-on-weekday 0)
  (setq org-agenda-window-setup 'current-window)
  (setq org-directory "~/myhome/org")

  (setq org-startup-with-inline-images t)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '(;; other Babel languages
     (plantuml . t)))
  (setq org-plantuml-jar-path
        (expand-file-name "/usr/share/java/plantuml/plantuml.jar"))
  (setq org-todo-keywords
        '((sequence "TODO" "|" "DONE" "CANCELED")))
  (setq org-todo-keyword-faces
        '(("CANCELED" . "peru"))))

  ;;(setq org-agenda-files
  ;;      (seq-filter (lambda(x) (not (string-match "\.deleted/"(file-name-directory x))))
  ;;       (directory-files-recursively "~/myhome/org" "\\.org$")
  ;;       )))

(use-package org-download
  :ensure t
  :config
  (add-hook 'dired-mode-hook ' org-download-enable))


;;(setq org-capture-templates '(("j" "Journal entry" plain (function org-journal-find-location)
;;                               (file "~/abc.orgcaptmpl")
;;                               :jump-to-captured t :immediate-finish t)))
