;;(require 'org) 
;;;;set effort to add tags
;;(defun org-set-effort-with-tags-clock()
;;  "set effort and tags :clock:"
;;  (interactive)
;;;;    (let ((element (org-element-at-point))) 
;;;;      (let ((tags (org-element-property :tags element)))
;;;;        (unless (member "clock" tags)
;;;;          (org-set-tags-to (concat (org-get-tags-string) ":clock:"))))))
;;  (let ((tags (org-get-tags-string)))
;;    (unless (string-match-p ":clock:" tags)
;;      (org-set-tags-to (concat tags ":clock:"))))
;;  (org-set-effort))
;;
;;
;;(org-defkey org-mode-map "\C-c\C-xe" 'org-set-effort-with-tags-clock)


;; config org-notify-email for run
;;(setq novicecpp/org-notify-rule '((test (:time "1m" :actions novicecpp/org-notify-sendmail))
;;                                  (next-day (:time "-6h" :actions novicecpp/org-notify-sendmail))
;;                                  (default (:time "-2m" :actions novicecpp/org-notify-sendmail))))
;;(setq novicecpp/org-notify-agenda-files '("~/org/gtdv2/calendar.org" "~/org/gtdv2/tickler.org"))



;; refile target
(defun novicecpp/refile-target (file headline)
  "refile fixed location to FILE and HEADLINE in the file

src: https://emacs.stackexchange.com/questions/8045/org-refile-to-a-known-fixed-location"
  (let ((pos (save-excursion
               (find-file file)
               (org-find-exact-headline-in-buffer headline))))
    (switch-to-buffer (current-buffer))
    (org-refile nil nil (list headline file nil pos))))

;; shortcut for link and refile project/nextaction

(defun novicecpp/set-hash()
  "test"
  (interactive)
  (let* ((element (org-element-at-point))
        (title (org-element-property :title element))
        (current (format-time-string "%Y-%m-%d %H:%M:%S"))
        (hash (secure-hash 'sha256 (concat title current))))
    (progn 
      (org-set-property "CUSTOM_ID" hash)
      (message (concat "set hash property: " hash))))  
)
(defun novicecpp/set-and-refile-project-nextaction()
  "test2"
  (interactive)
  (progn
    (novicecpp/set-child-hash-property)
    (novicecpp/refile-target "~/org/gtdv2/projectnextaction.org" "Project")))


(defun novicecpp/set-child-hash-property()
  "test2"
  (interactive)
  (let ((hash-current (org-entry-get nil "CUSTOM_ID"))
        (hash-parent (org-entry-get nil "CUSTOM_ID" t))
        (parent-title (save-excursion 
                        (progn
                          (org-up-heading-safe)
                          (replace-regexp-in-string "\\]" "}" (replace-regexp-in-string "\\[" "{" (nth 4 (org-heading-components))))))))
    (if hash-current
        (message "current headline have properties \"hash\"")
      (progn 
        
        (org-set-property "parent_topic" (format 
                                          "[[file:%s::#%s][%s]]"
                                          (buffer-file-name) hash-parent parent-title))
        (org-set-property "parent_hash" hash-parent)
        (org-set-property "CATEGORY" (subseq hash-parent 0 12))
        (message (concat "set parent_hash property:" hash-parent))))))

(defun novicecpp/set-link-and-refile()
  "test2"
  (interactive)
  (let ((hash-current (org-entry-get nil "CUSTOM_ID")))
    (if hash-current
        (let* ((title (replace-regexp-in-string "\\]" "}" (replace-regexp-in-string "\\[" "{" (nth 4 (org-heading-components)))))
               (link-title (format "[[file:%s::#%s][%s]]" (buffer-file-name) hash-current title)))          
          (save-excursion      
            (progn
              (org-insert-heading-respect-content)
              (org-do-demote)
              (insert link-title)
              (novicecpp/refile-target "~/org/gtd/daily.org" "Work"))))
      (message "current headline didn't have CUSTOM_ID property"))))

(defun novicecpp/org-refile-support-project-file()
  "test2"
  (interactive)
  (let ((hash-current (org-entry-get nil "CUSTOM_ID")))
    (if hash-current
        (let* ((title (nth 4 (org-heading-components)))
               (title-replaced (replace-regexp-in-string "\\]" "}" (replace-regexp-in-string "\\[" "{" title)))
               (link-title (format "[[file:%s::#%s][%s]]" (buffer-file-name) hash-current title-replaced))
               (custom-id-file (subseq hash-current 0 12)))
          (save-excursion      
            (progn
              (org-insert-heading-respect-content)
              (org-do-demote)
              (insert link-title)
              (save-excursion
                (progn
                  (find-file (format "~/org/gtd/support-material/%s.org" custom-id-file))
                  (insert "* HEADER")
                  (save-buffer)))
              (novicecpp/refile-target (format "~/org/gtd/support-material/%s.org" custom-id-file) "HEADER"))))
      (message "current headline didn't have CUSTOM_ID property"))))

(defun novicecpp/set-and-refile-habit()
  (interactive)
  (progn
    (novicecpp/set-child-hash-property)
    (novicecpp/refile-target "~/org/gtdv2/habits.org" "Habits")))

(defun novicecpp/set-and-refile-waiting-for()
  (interactive)
  (progn
    (novicecpp/set-child-hash-property)
    (novicecpp/refile-target "~/org/gtdv2/waiting.org" "Waiting For")))


;; view next action of fproject
(defun novicecpp/org-tag-view-nextaction-of-project()
  "novicecpp/org-tag-view-nextaction-of-project"
  (interactive)
  
  (let* ((org-agenda-files '("~/org/gtd/projectlist.org"))
         (org-enforce-todo-dependencies nil)
         (parent-hash (org-entry-get nil "CUSTOM_ID"))
         (title  (nth 4 (org-heading-components)))
         (org-agenda-overriding-header (format "\n%s\n------------------\n" title)))
    (if org-agenda-restrict
        (org-agenda-remove-restriction-lock)
      (progn
        (org-agenda-set-restriction-lock)
        (funcall 'org-tags-view nil "+LEVEL>0")))))

(defun novicecpp/org-open-project-file()
  (interactive)
    (let ((custom-id (org-entry-get nil "CUSTOM_ID")))
      (if custom-id 
        (find-file-other-window (format "~/org/gtd/support-material/%s.org" (subseq custom-id 0 12)))
        (message "No CUSTOM_ID for this headline."))))

(defun novicecpp/org-open-indirect-and-support-file()
  "TODO check if support-file is exist"
  (interactive)
  (let ((custom-id-file (subseq (org-entry-get nil "CUSTOM_ID") 0  12)))
    (if custom-id-file
        (progn
          (delete-other-windows)
          (org-tree-to-indirect-buffer)
          (other-window 1)
          (delete-other-windows)
          (find-file-other-window (format "~/org/gtd/support-material/%s.org" custom-id-file)))
      (message "No CUSTOM_ID for this headline."))))
    



;;(define-key org-mode-map (kbd "C-c C-x C-g C-r h") 'novicecpp/set-and-refile-habit)
;;(define-key org-mode-map (kbd "C-c C-x C-g C-r n") 'novicecpp/set-and-refile-project-nextaction)
;;(define-key org-mode-map (kbd "C-c C-x C-g C-r w") 'novicecpp/set-and-refile-waiting-for)
(define-key org-mode-map (kbd "C-c C-M-c") 'novicecpp/copy-work)
(define-key org-mode-map (kbd "C-c C-x S") 'novicecpp/set-hash)
;;(define-key org-mode-map (kbd "C-c C-x s") 'novicecpp/set-child-hash-property)
(define-key org-mode-map (kbd "C-c C-x s") 'novicecpp/set-link-and-refile)
(define-key org-mode-map (kbd "C-c C-M-s")  'novicecpp/org-refile-support-project-file)
(define-key org-mode-map (kbd "C-c C-M-p")  'novicecpp/org-open-indirect-and-support-file)
