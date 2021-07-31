;; https://stackoverflow.com/questions/11384516/how-to-make-all-org-files-under-a-folder-added-in-agenda-list-automatically
(setq org-agenda-custom-commands
      '(("s" "Sprint Schedule" agenda nil
         ((org-agenda-files (seq-filter (lambda(x) (string-match "/plan/" (file-name-directory x)))
                                        (directory-files-recursively "~/myhome/org/" "sprint.+\\.org$")))
          (org-agenda-prefix-format "  REVISE: %c, %s"))
         nil)))
