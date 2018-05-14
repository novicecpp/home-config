(setq org-agenda-custom-commands 
      '(("c" . "Custom Commands")
        ("ct" "Today Concern" agenda ""
         ((org-agenda-span 'day)
          (org-agenda-overriding-header "\nToday concern\n------------------\n")
          (org-agenda-files '("~/org/gtd/projectlist.org" "~/org/gtd/tickler.org"))
          (org-habit-graph-column 60))
         nil)
        ("cs" "Today Schedule" agenda ""
         ((org-agenda-span 'day)
          (org-agenda-overriding-header "\nToday Schedule\n------------------\n")
          (org-agenda-files '("~/org/gtd/daily.org"))
          (org-agenda-use-time-grid t)
          (org-agenda-prefix-format '((agenda ."    %?-12t% s")))
          (org-agenda-overriding-columns-format "%60ITEM %effort %CLOCKSUM"))
          ;;(org-agenda-time-grid '((daily today)
          ;;                        (0 100 200 300 400 500 600 700 800 900 1000 1200 1400 1600 1800 2000)
          ;;                        "......" "----------------")))
         nil)
        ("cn" "Next?" tags-todo 
         (concat "+CATEGORY=\"next\"+" (read-from-minibuffer "Context? "))
         ((org-agenda-overriding-header "\nNext Action\n------------------\n")
          (org-agenda-prefix-format '((tags . "  ")))
          (org-agenda-files '("~/org/gtd/projectlist.org")))
         nil)
        ("cw" "Waiting?" tags
         (concat "+CATEGORY=\"waiting\"+" (read-from-minibuffer "Name? ")) 
         ((org-agenda-overriding-header "\nWaiting For\n------------------\n")
          (org-agenda-prefix-format '((tags . "  ")))
          (org-agenda-files '("~/org/gtd/projectlist.org")))
         nil)
        ("v" . "View")
        ("vm" "Calendar" agenda "" 
         ((org-agenda-start-day "-7d") 
          (org-agenda-overriding-header "\nCalendar\n------------------\n")
          (org-agenda-span 'month)
          (org-agenda-files '("~/org/gtd/projectlist.org"))
          (org-agenda-skip-function 
           (lambda () (funcall 'novicecpp/org-skip-unless-category "calendar"))))
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
        ("xt" "Today Concern" agenda ""
         ((org-agenda-span 'day)
          (org-agenda-overriding-header "\nToday concern\n------------------\n")
          (org-agenda-files '("~/org/gtd/projectlist.org" "~/org/gtd/tickler.org"))
          (org-habit-graph-column 90)
          (org-agenda-use-time-grid t))
         "today.html")
        ("xc" "All Schedule/Deadline" agenda "" 
         ((org-agenda-start-day "-7d") 
          (org-agenda-overriding-header "\nSchedule/Deadline\n------------------\n")
          (org-agenda-span 'month)
          (org-agenda-files '("~/org/gtd/projectlist.org" "~/org/gtd/tickler.org")))
         "calendar.html")
        ("xp" "Project List." tags "+LEVEL>0" 
         ((org-agenda-overriding-header "\nProject with all child.\n------------------\n")
          (org-agenda-prefix-format '((tags . "  ")))
          (org-tags-match-list-sublevels 'indented)
          (org-agenda-files '("~/org/gtd/projectlist.org")))
         "project.html")
        ("xs" "Someday/Maybe List" tags "LEVEL>0" 
         ((org-agenda-overriding-header "\nSomeday/Maybe\n------------------\n")
          (org-agenda-prefix-format '((tags . "  ")))          
          (org-agenda-files '("~/org/gtd/someday.org")))
         "someday.html")

 ))

(defun novicecpp/org-skip-unless-category(CMP)
  "Skip trees that are not CMP"
  (let ((subtree-end (save-excursion (org-end-of-subtree t)))
        (category (org-entry-get nil "CATEGORY")))
    (if (string= CMP category) 
        nil ; tag found, do not skip
      subtree-end)))
       ; tag not found, continue after end of subtree
