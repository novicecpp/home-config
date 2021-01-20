;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
(use-package org
  :ensure t
  :config
  (setq org-startup-indented t)
  (global-set-key (kbd "C-c l") 'org-store-link)
  (global-set-key (kbd "C-c c") 'org-capture))
