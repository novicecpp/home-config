;; org-mode
(require 'org)
(require 'ox-md)
(setq org-agenda-window-setup 'only-window)
(setq org-agenda-restore-windows-after-quit t)

(load-file "~/.emacs.d/custom/org-agenda-custom-command.el")

(setq org-directory "~/org")
;;(setq org-hide-emphasis-markers t)
(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map "\C-cl" 'org-store-link)
(setq org-log-done 'time)
(setq org-clock-into-drawer t)
(setq org-export-backends '(ascii html latex odt))
(setq org-startup-indented t)
(setq org-hide-leading-stars t)
;;;; clear child todo before parent can done
(setq org-enforce-todo-dependencies t)
;;(setq org-tags-match-list-sublevels 'indented)

;;;; org todo state
(setq org-todo-keywords
  '((sequence "ASK(a)" "|" "DONE(d)")
    (sequence "TODO(t)" "|" "DONE(d)" "CANCELED(c)")
    (sequence "TODAY(d)" "START(s)" "STOP(p)" "|")
    (sequence "RETRY(r)" "WAIT" "|" )))


(setq org-todo-keyword-faces
      '(("Today" . (:weight ultra-bold :foreground "brightyellow"))
        ("WAIT" . (:weight ultra-bold :foreground "pink"))))

;;;; set effort
(setq org-global-properties
      '(("Effort_ALL" .
         "0:15 0:30 1:00 1:30 2:00 3:00 4:00 5:00 6:00 0:00")))

;;;; org clock-in/out when state change
(defun novicecpp/state-change-hook()
  "clock-in when change to state 'START' and clock-out when change to state 'WAIT'"
    (let ((element (org-element-at-point))) 
      (let ((tags (org-element-property :tags element))
            (state (org-element-property :todo-keyword element)))        
        (when (member "clock" tags) 
         (cond ((string= state "START") 
                (org-clock-in))
               ((and (string= state "WAIT") (org-clock-is-active)) 
                (org-clock-out))))
        (when (string= state "TODAY")
          (print t)
          (unless (member "clock" tags)
            (org-set-tags-to (concat (org-get-tags-string) ":clock:")))
          (org-set-effort)))))

(add-hook 'org-after-todo-state-change-hook 'novicecpp/state-change-hook)


;; org-capture
(setq org-default-notes-file (concat org-directory "/notes.org"))
(define-key global-map "\C-cc" 'org-capture)
(setq org-capture-templates
 '(("n" "Note" entry (file+headline "~/org/notes.org" "Thinking Captured")
        "* %? %^g\n")
   ("s" "Scheduling" entry (file+headline "~/org/notes.org" "Scheduling")
        "* %?\n")))
;;;; org-refile
(setq org-refile-use-outline-path t)
(setq org-refile-allow-creating-parent-nodes t)
;;;; agenda custom command

;; add org directory to agenda
(add-to-list 'org-agenda-files (expand-file-name "~/org/gtd"))
;; enter to follow link
;;(setq org-return-follows-link t)
;; habit
(add-to-list 'org-modules 'org-habit)
