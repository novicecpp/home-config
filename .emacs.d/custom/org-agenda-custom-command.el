(setq org-agenda-custom-commands 
      '(("c" . "Custom Commands")
        ("ct" "Today Concern" agenda "" 
         ((org-agenda-span 'day)
          (org-agenda-overriding-header "\nToday concern\n------------------\n")
          (org-agenda-files '("~/org/gtdv2/calendar.org" "~/org/gtdv2/habits.org" "~/org/gtdv2/tickler.org"))
          (org-habit-graph-column 60)))
        ("cn" "Next Action by Tags" tags-todo (concat (read-from-minibuffer "TAGS: ") "+LEVEL>0")
         ((org-agenda-overriding-header "\nNext Action\n------------------\n")
          (org-agenda-files '("~/org/gtdv2/next.org" "~/org/gtdv2/projectnextaction.org")))
         nil)
        ("cw" "Waiting For" tags-todo (concat (read-from-minibuffer "TAGS: ") "+LEVEL>0")
         ((org-agenda-overriding-header "\nWaiting For\n------------------\n")
          (org-agenda-files '("~/org/gtdv2/waiting.org")))
         nil)
        ("v" . "Org List")
        ("vp" "Project" tags "+LEVEL>0" 
         ((org-tags-match-list-sublevels 'indented)
          (org-agenda-overriding-header "\nProject List\n------------------\n")
          (org-agenda-prefix-format '((tags . "  ")))
          (org-agenda-files '("~/org/gtdv2/projectlist.org")))
         nil)
        ("vm" "Calendar" agenda "" 
         ((org-agenda-start-day "-7d") 
          (org-agenda-overriding-header "\nCalendar\n------------------\n")
          (org-agenda-span 'month)
          (org-agenda-files '("~/org/gtdv2/calendar.org"))) 
         nil)
        ("vk" "Tickler" agenda "" 
         ((org-agenda-start-day "-7d") 
          (org-agenda-overriding-header "\nTickler\n------------------\n")
          (org-agenda-span 'month)
          (org-agenda-files '("~/org/gtdv2/tickler.org")))
         nil)
        ("vs" "Someday" tags "+LEVEL>0" 
         ((org-tags-match-list-sublevels 'indented)
          (org-agenda-overriding-header "\nSomeday/Maybe\n------------------\n")
          (org-agenda-files '("~/org/gtdv2/someday.org")))
         nil)
        ("vt" "get today todo" todo "TODAY|START|STOP|RETRY" 
         ((org-agenda-overriding-columns-format "%60ITEM(item) %8Effort(estimate) %8CLOCKSUM(time)") (org-enforce-todo-dependencies nil)))))
        
