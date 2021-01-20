(defun novicecpp/exwm-start-app() 
  ""
  (interactive) 
  (progn 
    (message "sleep 1 secs")
    (sleep-for 1)
    (exwm-workspace-switch-create 11)
    (split-window-right)
    (sleep-for 1)
    (start-process-shell-command
     "s1" nil "google-chrome-stable --new-window https://messenger.com")
    (sleep-for 2)
    (other-window 1)
    (sleep-for 1)
    (start-process-shell-command
     "s2" nil "google-chrome-stable --new-window chrome-extension://ophjlpahpchlmihnnnihgmmeilfjmjjc/index.html")
    (sleep-for 2)
    (exwm-workspace-switch-create 9)
    (sleep-for 1) 
    (start-process-shell-command
     "" nil "gnome-terminal")
    (sleep-for 2)
    (exwm-input-release-keyboard)
    (sleep-for 1)
    (exwm-workspace-switch-create 14)
    (sleep-for 1)
    (find-file "~/org/gtd/")
    (sleep-for 2)
    (exwm-workspace-switch-create 6)
    (sleep-for 1) 
    (start-process-shell-command
     "" nil "firefox")
    (sleep-for 2)
    (exwm-workspace-switch-create 5)
    (sleep-for 1) 
    (start-process-shell-command
     "" nil "google-chrome-unstable --new-window")
    (sleep-for 2)
    (exwm-workspace-switch-create 12)
    (sleep-for 1)
    (start-process-shell-command
     "" nil "bash -c 'eval $(gnome-keyring-daemon -r -d --components=secrets); nextcloud'")
    (sleep-for 2)
    (start-process-shell-command
     "" nil "ibus-setup")
    (sleep-for 2)
))

  
