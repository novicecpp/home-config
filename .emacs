;; add melpa

(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))

(package-initialize)

;; manage auto-save file
;; http://snarfed.org/gnu_emacs_backup_files
;; Put autosave files (ie #foo#) and backup files (ie foo~) in ~/.emacs.d/.
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/autosaves/\\1" t)) )
(setq backup-directory-alist '((".*" . "~/.emacs.d/backups/")) )
(make-directory "~/.emacs.d/autosaves/" t)

(setq c-default-style "linux"
      c-basic-offset 4)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default tab-stop-list (number-sequence 4 200 4))
(when (fboundp 'electric-indent-mode) (electric-indent-mode -1))

;; set-font
(set-face-attribute 'default nil :font "Liberation Mono 12" )


;; disable menubar in terminal
;;(if window-system nil
;;  (menu-bar-mode -1)
;;)
(menu-bar-mode -1)
(tool-bar-mode -1)

;; theme
;; (add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(load-theme 'zenburn t)



;; build-in plugin
;; saveplace
;; https://www.emacswiki.org/emacs/SavePlace
(setq-default save-place t)
(setq save-place-file "~/.emacs.d/saved-places")
(setq save-place-forget-unreadable-files nil)

;; auto refresh when file change
;; http://stackoverflow.com/questions/1480572/how-to-have-emacs-auto-refresh-all-buffers-when-files-have-changed-on-disk
(global-auto-revert-mode t)

;; highlight parenthesis
(show-paren-mode 1)

;; linum-mode
;; https://www.systutorials.com/qa/666/how-add-space-between-the-line-numbers-and-text-content-emacs
(setq linum-format (lambda (line) (propertize (format (let ((w (length (number-to-string (count-lines (point-min) (point-max)))))) (concat "%" (number-to-string w) "d ")) line) 'face 'linum)))
(global-linum-mode 1)
;; https://stackoverflow.com/questions/3626632/right-align-line-numbers-with-linum-mode
;;(defadvice linum-update-window (around linum-dynamic activate)
;;  (let* ((w (length (number-to-string
;;                     (count-lines (point-min) (point-max)))))
;;         (x (if (> w 3) w 4))
;;         (linum-format (concat "%" (number-to-string x) "d ")))
;;    ad-do-it))
;;

;; tramp mode
;; https://stackoverflow.com/questions/3465567/how-to-use-ssh-and-sudo-together-with-tramp-in-emacs
(setq tramp-default-method "ssh")
(set-default 'tramp-default-proxies-alist (quote ((".*" "\\`root\\'" "/ssh:%h:"))))


;; installed plugin

;; smart mode-line
;; (setq sml/no-confirm-load-theme t)
;; (setq sml/theme 'respectful)
;; (sml/setup)


;; ace-jump-mode
(require 'ace-jump-mode)
(define-key global-map (kbd "C-c C-SPC") 'ace-jump-mode)


;; auto-complete
;;(require 'auto-complete)
;;(ac-config-default)

;; jedi mode
;;(require 'jedi)

;; flycheck
;;(require 'flycheck)

;; yasnipped
;; (require 'yasnippet)
;; (yas-reload-all)

;; (require 'neotree)
;; (global-set-key (kbd "C-x n t") 'neotree)
;; (setq-default neo-window-width 20)

(require 'undo-tree)
(global-undo-tree-mode)

;; bug when use with emacs25
;; (require 'csharp-mode)

;;(require 'py-autopep8)
;;(add-to-list 'exec-path "~/.emacs.d/python2.7/bin/")

;; ido-mode config
;; base ido
;;(ido-mode 1)
;;(ido-everywhere 1)
;; ido-ubiquitous
;;(require 'ido-ubiquitous)
;;(ido-ubiquitous-mode 1)
;;;; ido-yes-or-no
;;(require 'ido-yes-or-no)
;;(ido-yes-or-no-mode 1)
;;;; smex (ido-M-x)
;;(require 'smex)
;;(global-set-key (kbd "M-x") 'smex)
;;(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)
;;;; flx-ido
;;(require 'flx-ido)
;;(flx-ido-mode 1)
;;;; disable ido faces to see flx highlights.
;;(setq ido-enable-flex-matching t)
;;(setq ido-use-faces t)
;;

;;;; language hook
(require 'flycheck)
(add-hook 'python-mode-hook 'flycheck-mode)
(add-hook 'python-mode-hook 'py-yapf-enable-on-save)
;;(add-hook 'python-mode-hook 'yapf-mode)

;;(defun my-python-hook ()
;;  "python mode hook"
;;  (interactive)
;;  (flycheck-mode)
;;  (flycheck-select-checker 'python-flake8)
;;  (py-autopep8-enable-on-save)
;;  (setq flycheck-flake8-maximum-line-length 120)
;;  (jedi:setup)
;;  (setq jedi:complete-on-dot t)
;;)
;;
;;(add-hook 'python-mode-hook 'my-python-hook)
;; python
;;(add-hook 'python-mode-hook 
;;          (lambda ()
;;            (flycheck-mode)
;;            (flycheck-select-checker 'python-flake8)
;;;;            (yas-minor-mode)
;;;;            (jedi:setup)
;;            (py-autopep8-enable-on-save)))


;;(setq python-shell-interpreter "ipython3" 
;;      python-shell-interpreter-args "-i"
;;      )

;;yaml-mode
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-mode))

;;.bash_mybash
(add-to-list 'auto-mode-alist '("\\.bash_mybash\\'" . sh-mode))

;; org-mode
;;(setq org-hide-emphasis-markers t)
(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map "\C-cl" 'org-store-link)
(setq org-log-done 'note)
(setq org-clock-into-drawer t)
(setq org-todo-keywords
  '((sequence "TODO(t)" "START(s)" "WAIT(w)" "|" "DONE(d)")
    (sequence "|" "CANCELED(c)")))
(setq org-export-backends '(ascii html latex odt))
(setq org-startup-indented t)
(setq org-hide-leading-stars t)
;;;; set effort
(setq org-global-properties
      '(("Effort_ALL" .
         "0:15 0:30 1:00 1:30 2:00 3:00 4:00 5:00 6:00 0:00")))

;;;; org clock-in/out when state change
(defun clock-inout-after-state-change-hook()
  "clock-in when change to state 'START' and clock-out when change to state 'WAIT'"
    (let ((element (org-element-at-point))) 
      (let ((tags (org-element-property :tags element))
            (state (org-element-property :todo-keyword element)))        
        (when (member "clock" tags) 
          (cond ((string= state "START") (org-clock-in))
                ((and (string= state "WAIT") (org-clock-is-active)) (org-clock-out)))))))

(add-hook 'org-after-todo-state-change-hook 'clock-inout-after-state-change-hook)


;; browse-url
(setq browse-url-browser-function 'eww-browse-url)

;; magit
(define-key global-map (kbd "C-x g") 'magit-status)


;; test


(add-to-list 'default-frame-alist '(font . "Liberation Mono-12" ))
(set-face-attribute 'default t :font "Liberation Mono-12" )




(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files (quote ("~/org/work.org")))
 '(package-selected-packages
   (quote
    (magit org-beautify-theme dockerfile-mode yaml-mode nhexl-mode py-yapf yapfify flycheck zenburn-theme undo-tree ace-jump-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
