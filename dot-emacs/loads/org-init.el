;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
(use-package org
  :ensure t
  :config
  (setq org-startup-indented t)
  (global-set-key (kbd "C-c l") 'org-store-link)
  (global-set-key (kbd "C-c c") 'org-capture))




(use-package org-journal
  :ensure t)


;;(setq org-refile-targets '(("kkk.org" :maxlevel . 9)
;;                                (org-agenda-files :maxlevel . 9)))
;;(setq org-outline-path-complete-in-steps nil)         ; Refile in a single go
;;(setq org-refile-use-outline-path t)                  ; Show full paths for refiling
;;
;;
;;(setq org-capture-templates
;;      '(("t" "Todo" entry (file+headline "~/gtd.org" "Tasks")
;;         "* TODO %?\n  %i\n  %a")
;;        ("k" "my template" entry (file "~/abc.org")
;;         (file "~/abc.orgcaptmpl"))
;;        ("j" "Journal" entry (file+datetree "~/journal.org")
;;         "* %?\nEntered on %U\n  %i\n  %a")))

(defun org-journal-find-location ()
  ;; Open today's journal, but specify a non-nil prefix argument in order to
  ;; inhibit inserting the heading; org-capture will insert the heading.
  (org-journal-new-entry t)
  (unless (eq org-journal-file-type 'daily)
    (org-narrow-to-subtree))
  (goto-char (point-max)))

(setq org-capture-templates '(("j" "Journal entry" plain (function org-journal-find-location)
                               (file "~/abc.orgcaptmpl")
                               :jump-to-captured t :immediate-finish t)))
