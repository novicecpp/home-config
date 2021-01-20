(require 'org-notify)

(defvar novicecpp/org-notify-rule nil
  "test1")
(defvar novicecpp/org-notify-agenda-files nil
  "test2")


(org-notify-start)
;;(org-notify-add 'test
;;                '(:time "1m" :actions novicecpp/org-notify-sendmail))
(dolist (tmp-list novicecpp/org-notify-rule)
  (apply 'org-notify-add tmp-list))

(defun novicecpp/org-notify-todo-list-advice (old-function &rest arguments)
  "test var"
  (let ((org-agenda-files (or novicecpp/org-notify-agenda-files (quote ("~/test.org")))))
    (apply old-function arguments)))
(advice-add 'org-notify-todo-list :around #'novicecpp/org-notify-todo-list-advice)

(defun  novicecpp/org-notify-make-todo-advice (old-function &rest arguments)
  "test var"
    (apply 'novicecpp/org-notify-make-todo arguments))
(advice-add 'org-notify-make-todo :around #'novicecpp/org-notify-make-todo-advice)


(defun novicecpp/org-notify-make-todo (heading &rest ignored)
  "Create one todo item."
  ;;(message "hello there")
  (cl-macrolet ((get (k) `(plist-get list ,k))
             (pr (k v) `(setq result (plist-put result ,k ,v))))
    (let* ((list (nth 1 heading))      (notify (or (get :NOTIFY) "default"))
           (deadline (org-notify-convert-deadline (or (get :scheduled) (get :deadline))))
           (heading (get :raw-value))
           (content (buffer-substring-no-properties 
                     (get :contents-begin) (get :contents-end)))
           (category (get :CATEGORY))
           result)
      (when (and heading deadline)
        (pr :heading heading)        
        (pr :notify (intern notify))
        (pr :content content)
        (pr :category category)
        (pr :begin (get :begin))
        (pr :file (nth org-notify-parse-file (org-agenda-files 'unrestricted)))
        (pr :timestamp deadline)  (pr :uid (md5 (concat heading deadline)))
        (pr :deadline (- (org-time-string-to-seconds deadline)
                         (float-time))))
      result)))

(defun novicecpp/org-notify-sendmail (plist)
  "Send email to user."
  ;;(print (plist-get plist :content))
  (require 'gnus-art)
  (let ((mail-header (format "%s: %s" 
                 (upcase (plist-get plist :category)) 
                 (plist-get plist :heading)))
        (mail-body (format "%s\n\n%s"
                           (plist-get plist :content)
                           (replace-regexp-in-string 
                            " in the future" ""
                            (article-lapsed-string
                             (time-add 
                              (current-time)
                              (seconds-to-time (plist-get plist :deadline))) 2)))))
    (compose-mail user-mail-address mail-header)
    (insert mail-body)
    (let ((inhibit-message t))
      (funcall send-mail-function))
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (x) t)))
      (kill-buffer))))

(setq send-mail-function 'smtpmail-send-it)

(setq user-full-name "Thanayut Seethongchuen"
      user-mail-address "thanayut.se@ku.th"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587
      smtpmail-stream-type 'starttls)
