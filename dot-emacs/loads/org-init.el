(use-package org
  :ensure t
  :config
  (setq org-startup-indented t)
  (global-set-key (kbd "C-c l") 'org-store-link)
  (global-set-key (kbd "C-c c") 'org-capture)
  (global-set-key (kbd "C-c a") 'org-agenda))

;;(setq org-capture-templates '(("j" "Journal entry" plain (function org-journal-find-location)
;;                               (file "~/abc.orgcaptmpl")
;;                               :jump-to-captured t :immediate-finish t)))
