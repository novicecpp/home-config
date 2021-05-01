;; NOTE: not using org-babel yet because it not compat with flycheck

;; for debug
;; https://stackoverflow.com/questions/1322591/tracking-down-max-specpdl-size-errors-in-emacs/1322978
;;(setq max-specpdl-size 5)  ; default is 1000, reduce the backtrace level
;;(setq debug-on-error t)    ; now you should get a backtrace

;; add melpa
(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;; auto install use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; manage auto-save file
;; http://snarfed.org/gnu_emacs_backup_files
;; Put autosave files (ie #foo#) and backup files (ie foo~) in ~/.emacs.d/.
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/autosaves/\\1" t)) )
(setq backup-directory-alist '((".*" . "~/.emacs.d/backups/")) )
(make-directory "~/.emacs.d/autosaves/" t)


;; indent setting
(setq c-default-style "linux"
      c-basic-offset 4)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default tab-stop-list (number-sequence 4 200 4))
(when (fboundp 'electric-indent-mode) (electric-indent-mode -1))

;; set-font
(add-to-list 'default-frame-alist '(font . "Liberation Mono-11" ))
(set-face-attribute 'default t :font "Liberation Mono-11" )

;; disible bell noti
;; (setq visible-bell t)
(setq ring-bell-function 'ignore)

;; disable menubar toolbar scrollbar
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; enable column show
;; by default emacs only show line number
(setq column-number-mode t)

;; theme
(use-package zenburn-theme
  :ensure t
  :config
  (load-theme 'zenburn t))

;; https://www.emacswiki.org/emacs/SavePlace
;; emacs < 25.1
;;(use-package saveplace
;;  :config
;;  (setq-default save-place t)
;;  (setq save-place-file "~/.emacs.d/saved-places")
;;  (setq save-place-forget-unreadable-files nil))
(save-place-mode 1)

;; auto refresh when file change
;; http://stackoverflow.com/questions/1480572/how-to-have-emacs-auto-refresh-all-buffers-when-files-have-changed-on-disk
(global-auto-revert-mode t)

;; highlight parenthesis
(show-paren-mode 1)

;; tramp mode
;; https://stackoverflow.com/questions/3465567/how-to-use-ssh-and-sudo-together-with-tramp-in-emacs
(use-package tramp
  :config
  (setq tramp-default-method "ssh")
  (setq tramp-use-ssh-controlmaster-options nil) ; Don't override SSH config.
  (add-to-list 'tramp-default-proxies-alist '(nil "\\`root\\'" "/ssh:%h:"))
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))

;; delete trailing whitespace when save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; for doom-modeline, also resize icon to normal text
;; need to install fonts manually by execute `M-x all-the-icons-install-fonts`
(use-package all-the-icons
  :ensure t
  :config
  (setq all-the-icons-scale-factor 1.0))

(use-package doom-modeline
  :ensure t
  :config
  (setq doom-modeline-icon t)
  (setq doom-modeline-height 10)
  (set-face-attribute 'mode-line nil :height 100)
  (set-face-attribute 'mode-line-inactive nil :height 100)
  :hook (after-init . doom-modeline-mode))

;; jump window like tmux+ace-jump-mode
(use-package ace-window
  :ensure t
  :bind
  ("C-x q" . ace-window)
  :config
  (setq aw-keys '(?a ?s ?d ?f ?j ?k ?l ?\;)))


;; fix exec path for mac os, but don't use mac anymore
;;(require 'exec-path-from-shell)
;;(when (memq window-system '(mac ns x))
;;  (exec-path-from-shell-initialize))

;; show ssh list in counsel
(use-package counsel-tramp
  :ensure t
  :config
  (define-key global-map (kbd "C-c s") 'counsel-tramp))

;; key helper
(use-package which-key
  :ensure t
  :config (which-key-mode))

;; avy: alternative ace-jump-mode
(use-package avy
  :bind* ("C-." . avy-goto-char-timer)
  :config
  (avy-setup-default))

(use-package undo-tree
  :ensure t
  :config
  (global-undo-tree-mode))

(use-package dockerfile-mode
  :ensure t)

;;ivy
(use-package counsel
  :ensure t)

(use-package ivy
  :ensure t
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  :bind
  (("C-s" . swiper)
   ("C-c g" . counsel-git)
   ("C-c j" . counsel-git-grep)
   ("C-c k" . counsel-ag)
   ("C-c C-c C-l" . counsel-locate)
   ("C-c C-r" . ivy-resume)))

(use-package sudo-edit
  :ensure t
  :bind
  (("C-c C-r" . sudo-edit)))

;; DISABLED: train before use
;;(use-package origami
;;  :ensure t
;;  :bind
;;  (("C-c C-c" . origami-recursively-toggle-node)
;;   ("C-c o" . origami-show-only-node)
;;   ("C-c S" . origami-open-all-nodes))
;;  :hook
;;  (prog-mode . (lambda () (origami-mode))))

(use-package lsp-mode
  :ensure t
  :init (setq lsp-headerline-breadcrumb-enable nil)
  :hook (python-mode . lsp)
        (lsp-mode . lsp-enable-which-key-integration)
  :commands lsp)

(use-package lsp-ui
  :ensure t
  :bind
  (:map lsp-ui-mode-map
        ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
        ([remap xref-find-references]  . lsp-ui-peek-find-references))
  :commands lsp-ui-mode)

(use-package lsp-ivy
  :commands lsp-ivy-workspace-symbol)
(use-package lsp-treemacs
  :commands lsp-treemacs-errors-list)


(use-package company
  :ensure t
  :diminish
  :bind
  ("M-/" . company-complete)
  :hook
  (after-init . global-company-mode)
  :config
  (use-package company-quickhelp
    :ensure t
    :config
    (company-quickhelp-mode 1))
  (setq company-idle-delay 0.2))

(use-package company-ansible
  :ensure t
  :config
  (push 'company-ansible company-backends))

(use-package flycheck
  :ensure t
  :init
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))
  :hook
  (prog-mode . flycheck-mode))

(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1))


(use-package projectile
  :ensure t
  :config
  (projectile-mode)
  (setq projectile-completion-system 'ivy)
  :bind
  (("C-c p" . projectile-command-map)))

(use-package counsel-projectile
  :ensure t
  :config
  (counsel-projectile-mode))

;; DEPRECATED: delete-trailing-whitespace is enough
;;(use-package yapfify
;;  :ensure t)
;;  :hook
;;  (python-mode . (lambda () (yapf-mode))))


(use-package pdf-tools
  :ensure t)

(setq python-shell-interpreter "python3"
      python-shell-interpreter-args "-i")

;; DEPRECATED(?): try linum package instead
;;(use-package display-line-numbers
;;  :hook
;;  (prog-mode . (lambda () (display-line-numbers-mode))))

(use-package linum
  :hook
  (prog-mode . linum-on)
  (yaml-mode . linum-on))



;;yaml-mode
(use-package yaml-mode
  :ensure t)

;; magit
(use-package magit
  :ensure t
  :bind
  ("C-x g" . magit-status))

(use-package jsonnet-mode
  :ensure t
  :config
  (setq jsonnet-use-smie t)
  :hook
  (jsonnet-mode . (lambda ()
                    (setq indent-tabs-mode t
                          tab-width 2))))
;;

;;.auto-mode & interpreter-mode
(push '("python" . python-mode) interpreter-mode-alist)
(push '("bash" . sh-mode) interpreter-mode-alist)
(push '("\\.yaml\\'" . yaml-mode) auto-mode-alist)
(push '("\\.emacs_exwm\\'" . emacs-lisp-mode) auto-mode-alist)
(push '("\\.env\\'" . sh-mode) auto-mode-alist)
(push '("Dockerfile\\'" . dockerfile-mode) auto-mode-alist)

;; set default emacs web browser
;; browse-url-generic-program value is browser executable file
;; (setq-default browse-url-browser-function 'browse-url-generic
;;               browse-url-generic-program "google-chrome-unstable")
;; follow symlink
(setq find-file-visit-truename t)

;;
(setq custom-file "~/.emacs.d/custom-set-variable.el")

;; interact with gpg file
(use-package epa-file
  :config
  (epa-file-enable)
  (setq epg-gpg-program  "/usr/bin/gpg2"
        epa-pinentry-mode 'loopback))

(use-package dumb-jump
  :ensure t
  :bind
  (("M-g o" . dumb-jump-go-other-window)
   ("M-g j" . dumb-jump-go)
   ("M-g b" . dumb-jump-back)
   ("M-g i" . dumb-jump-go-prompt)
   ("M-g x" . dumb-jump-go-prefer-external)
   ("M-g z" . dumb-jump-go-prefer-external-other-window))
  :config
  (setq dumb-jump-selector 'ivy))

(use-package go-mode
  :ensure t)

(use-package rust-mode
  :ensure t)

(require 'auth-source-pass)
(auth-source-pass-enable)

(use-package impatient-mode
  :ensure t)

(defun markdown-html (buffer)
  (princ (with-current-buffer buffer
    (format "<!DOCTYPE html><html><title>Impatient Markdown</title><xmp theme=\"united\" style=\"display:none;\"> %s  </xmp><script src=\"http://strapdownjs.com/v/0.2/strapdown.js\"></script></html>" (buffer-substring-no-properties (point-min) (point-max))))
  (current-buffer)))




;; load multicursors
(load-file "~/.emacs.d/loads/mc.el")

;; load org config
(load-file "~/.emacs.d/loads/org-init.el")

;; ================== testing section ======================
;; load test
;; (load-file "~/test.el")

;; (setq-default persp-keymap-prefix (kbd "C-c o"))
;; (require 'persp-mode)

(use-package persp-mode
  :ensure t
  :init
  (setq-default persp-keymap-prefix (kbd "C-c z"))
  :hook
  (window-setup-hook . (lambda () (persp-mode 1)))
  :config
  (setq persp-autokill-buffer-on-remove 'kill-weak)
  (persp-mode 1))
