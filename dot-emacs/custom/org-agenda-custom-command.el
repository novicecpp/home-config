(setq org-agenda-custom-commands 
      '(("A" "Summary" 
         ((agenda ""
                  ((org-agenda-span 'day)
                   (org-agenda-overriding-header "Today concern")
                   (org-agenda-files '("~/org/gtd/calendar.org" "~/org/gtd/tickler.org"))
                   (org-habit-graph-column 60)))
          (tags-todo "+CATEGORY=\"waiting\""
                     ((org-agenda-overriding-header "Waiting")
                      (org-agenda-prefix-format '((tags . "  ")))
                      (org-agenda-files '("~/org/gtd/waiting.org"))
                      (org-agenda-sorting-strategy '((tags tag-up)))
                      (org-agenda-max-entries 10)))
          (agenda ""
                  ((org-agenda-overriding-header "This week")
                   (org-agenda-files '("~/org/gtd/calendar.org" "~/org/gtd/tickler.org"))
                   (org-habit-graph-column 60)
                   (org-agenda-start-day "+1d")
                   (org-agenda-span 6)
                   (org-agenda-start-on-weekday nil)
                   (org-deadline-warning-days 7)))
          (tags-todo "LEVEL>1" 
                ((org-agenda-overriding-header "Tasklist")
                 (org-agenda-prefix-format '((tags . "%-12:T  %(concat \"[ \"(org-format-outline-path (org-get-outline-path)) \" ]\") -> ")))
                 ;;(org-agenda-sorting-strategy '((tags tag-up)))
                 (org-agenda-files '("~/org/gtd/projectlist.org"))
                 (org-agenda-dim-blocked-tasks 'invisible)
                 (org-agenda-hide-tags-regexp "@"))))
         nil "summary.html")
                
        ("v" . "View")
        ("vm" "Calendar" agenda "" 
         ((org-agenda-start-day "-7d") 
          (org-agenda-overriding-header "\nCalendar\n------------------\n")
          (org-agenda-span 'month)
          (org-agenda-files '("~/org/gtd/projectlist.org" "~/org/gtd/tickler.org"))
          (org-agenda-skip-function
           (lambda () (or (funcall 'novicecpp/org-skip-unless-category "weekly" t) (funcall 'novicecpp/org-skip-unless-category "tickler" t)))))
         nil)         
        ("vt" "Tickler" agenda "" 
         ((org-agenda-start-day "-7d") 
          (org-agenda-overriding-header "\nTickler\n------------------\n")
          (org-agenda-span 'month)
          (org-agenda-files '("~/org/gtd/tickler.org")))
         nil)
        ("vh" "Habits" agenda "" 
         ((org-agenda-start-day "-7d") 
          (org-agenda-overriding-header "\nHabits\n------------------\n")
          (org-agenda-span 'month)
          (org-habit-graph-column 60)
          (org-agenda-files '("~/org/gtd/projectlist.org"))
          (org-agenda-skip-function 
           (lambda () (funcall 'novicecpp/org-skip-unless-category "habits"))))
         nil)
        ("vs" "Someday/Maybe" tags "LEVEL>0" 
         ((org-agenda-overriding-header "\nSomeday/Maybe\n------------------\n")
          (org-agenda-prefix-format '((tags . "  ")))          
          (org-agenda-files '("~/org/gtd/someday.org")))
         nil)
        ("vp" "Project List" tags "LEVEL=1" 
         ((org-agenda-overriding-header "\nProject List\n------------------\n")
          (org-agenda-prefix-format '((tags . "  ")))          
          (org-agenda-files '("~/org/gtd/projectlist.org")))
         nil)
        ("x" . "Export")
        ("xc" "All Schedule/Deadline" agenda "" 
         ((org-agenda-start-day "-7d") 
          (org-agenda-overriding-header "\nSchedule/Deadline\n------------------\n")
          (org-agenda-span 'month)
          (org-habit-graph-column 90)
;;          (org-agenda-skip-function 
;;           (lambda () (funcall 'novicecpp/org-agenda-skip-category "habits")))
          (org-agenda-files '("~/org/gtd/projectlist.org" "~/org/gtd/tickler.org")))
         "calendar.html")
        ("xp" "Project List." tags "+LEVEL>0" 
         ((org-agenda-overriding-header "\nProject with all child.\n------------------\n")
          (org-tags-match-list-sublevels 'indented)
          (org-agenda-files '("~/org/gtd/projectlist.org")))
         "project.html")
        ("xs" "Someday/Maybe List" tags "LEVEL>0" 
         ((org-agenda-overriding-header "\nSomeday/Maybe\n------------------\n")
          (org-agenda-prefix-format '((tags . "  ")))          
          (org-agenda-files '("~/org/gtd/someday.org")))
         "someday.html")

 ))

(defun novicecpp/org-skip-unless-category(CMP &optional REVERSE)
  "Skip trees that are not CMP"
  (let ((subtree-end (save-excursion (org-end-of-subtree t)))
        (category (org-entry-get nil "CATEGORY")))
    ; return nil if category found
    ; return subtree-end if not found
    (if (string= CMP category) 
        (if REVERSE
            subtree-end
          nil)
      (if REVERSE 
          nil                               
        subtree-end))))


(defun novicecpp/org-agenda-skip-category(CMP &optional REVERSE)
  "Skip trees that are not CMP"
  (let ((subtree-end (save-excursion (org-end-of-subtree t)))
        (category (org-entry-get nil "CATEGORY")))
    ; return nil if category found
    ; return subtree-end if not found
    (if (string= CMP category) 
        (if REVERSE
            subtree-end
          nil)
      (if REVERSE 
          nil                               
        subtree-end))))
