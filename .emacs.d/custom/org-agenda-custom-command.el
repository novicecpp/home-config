(setq org-agenda-custom-commands 
      '(("c" . "Custom Commands")
        ("ct" "Today Concern" agenda ""
         ((org-agenda-span 'day)
          (org-agenda-overriding-header "\nToday concern\n------------------\n")
          (org-agenda-files '("~/org/gtdv3/projectlist.org" "~/org/gtdv3/tickler.org"))
          (org-habit-graph-column 60))
         nil)
        ("cn" "Next?" tags-todo 
         (concat "+CATEGORY=\"next\"+" (read-from-minibuffer "Context? "))
         ((org-agenda-overriding-header "\nNext Action\n------------------\n")
          (org-agenda-prefix-format '((tags . "  ")))
          (org-agenda-files '("~/org/gtdv3/projectlist.org")))
         nil)
        ("cw" "Waiting?" tags-todo
         (concat "+CATEGORY=\"waiting\"+" (read-from-minibuffer "Name? ")) 
         ((org-agenda-overriding-header "\nWaiting For\n------------------\n")
          (org-agenda-prefix-format '((tags . "  ")))
          (org-agenda-files '("~/org/gtdv3/projectlist.org")))
         nil)
        ("v" . "View")
        ("vm" "Calendar" agenda "" 
         ((org-agenda-start-day "-7d") 
          (org-agenda-overriding-header "\nCalendar\n------------------\n")
          (org-agenda-span 'month)
          (org-agenda-files '("~/org/gtdv3/projectlist.org"))
          (org-agenda-skip-function 
           (lambda () (funcall 'novicecpp/org-skip-unless-category "calendar"))))       
         nil)
        ("vt" "Tickler" agenda "" 
         ((org-agenda-start-day "-7d") 
          (org-agenda-overriding-header "\nTickler\n------------------\n")
          (org-agenda-span 'month)
          (org-agenda-files '("~/org/gtdv3/tickler.org")))
         nil)
        ("vh" "Habits" agenda "" 
         ((org-agenda-start-day "-7d") 
          (org-agenda-overriding-header "\nHabits\n------------------\n")
          (org-agenda-span 'month)
          (org-habit-graph-column 60)
          (org-agenda-files '("~/org/gtdv3/projectlist.org"))
          (org-agenda-skip-function 
           (lambda () (funcall 'novicecpp/org-skip-unless-category "habits"))))
         nil)
        ("vs" "Someday/Maybe" tags "LEVEL>0" 
         ((org-agenda-overriding-header "\nSomeday/Maybe\n------------------\n")
          (org-agenda-prefix-format '((tags . "  ")))          
          (org-agenda-files '("~/org/gtdv3/someday.org")))
         nil)
        ("vp" "Project List" tags "LEVEL>0" 
         ((org-agenda-overriding-header "\nProject List\n------------------\n")
          (org-agenda-prefix-format '((tags . "  ")))          
          (org-agenda-files '("~/org/gtdv3/projectlist.org")))
         nil)))

        


(defun novicecpp/org-skip-unless-category(CMP)
  "Skip trees that are not CMP"
  (let ((subtree-end (save-excursion (org-end-of-subtree t)))
        (category (org-entry-get nil "CATEGORY")))
    (if (string= CMP category) 
        nil ; tag found, do not skip
      subtree-end)))
       ; tag not found, continue after end of subtree
