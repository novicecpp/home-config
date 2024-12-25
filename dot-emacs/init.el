;; Startup time
;; https://www.reddit.com/r/emacs/comments/m8d55l/what_is_your_startup_time/
(defun efs/display-startup-time ()
  (message
   "Emacs loaded in %s with %d garbage collections."
   (format
    "%.2f seconds"
    (float-time
     (time-subtract after-init-time before-init-time)))
   gcs-done))
(add-hook 'emacs-startup-hook #'efs/display-startup-time)

;; disable splash screen
(setq inhibit-startup-screen t)

;; package manager
;; ============================================
;;;; straight.el
;;(defvar bootstrap-version)
;;(progn (setq straight-repository-branch "develop")
;;       (let ((bootstrap-file
;;              (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
;;             (bootstrap-version 6))
;;         (unless (file-exists-p bootstrap-file)
;;           (with-current-buffer
;;               (url-retrieve-synchronously
;;                "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
;;                'silent 'inhibit-cookies)
;;             (goto-char (point-max))
;;             (eval-print-last-sexp)))
;;         (load bootstrap-file nil 'nomessage)))

;; add melpa
;;(require 'package)
;;(add-to-list 'package-archives
;;	     '("melpa" . "https://melpa.org/packages/"))
;;(package-initialize)
;;
;;;; auto install use-package
;;(unless (package-installed-p 'use-package)
;;  (package-refresh-contents)
;;  (package-install 'use-package))
;;(setq straight-use-package-by-default t)
;;(straight-use-package 'use-package)
;; =========================================
;; elpaca
(defvar elpaca-installer-version 0.7)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                 ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                 ,@(when-let ((depth (plist-get order :depth)))
                                                     (list (format "--depth=%d" depth) "--no-single-branch"))
                                                 ,(plist-get order :repo) ,repo))))
                 ((zerop (call-process "git" nil buffer t "checkout"
                                       (or (plist-get order :ref) "--"))))
                 (emacs (concat invocation-directory invocation-name))
                 ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                       "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                 ((require 'elpaca))
                 ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

(elpaca elpaca-use-package
  ;; Enable use-package :ensure support for Elpaca.
  (elpaca-use-package-mode))


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
(add-to-list 'default-frame-alist '(font . "Liberation Mono-12" ))
(set-face-attribute 'default nil :font "Liberation Mono-12" )

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

;; do not show native-compile's warning buffer
(setq native-comp-async-report-warnings-errors 'silent)

;; follow symlink
(setq find-file-visit-truename t)

;; custom-set-variable file, to prevent emacs automatically write it in this file
(setq custom-file "~/.emacs.d/custom-set-variable.el")

;; https://www.emacswiki.org/emacs/SavePlace
(save-place-mode 1)

;; https://www.gnu.org/software/emacs/manual/html_node/efaq/Displaying-the-current-line-or-column.html
(setq line-number-mode t
      column-number-mode t)

;; auto refresh when file change
;; http://stackoverflow.com/questions/1480572/how-to-have-emacs-auto-refresh-all-buffers-when-files-have-changed-on-disk
(global-auto-revert-mode t)
(setq auto-revert-interval 2)

;; highlight parenthesis
(show-paren-mode 1)

;; delete trailing whitespace when save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; disable lockfiles feature
(setq create-lockfiles nil)

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
;;               browse-url-generic-program "firefox")
;; theme
(use-package solarized-theme
  :ensure t)
(use-package zenburn-theme
  :ensure t
  :config
  ;; load solarized theme when run in test machine
  (if (file-exists-p "~/.emacs_test")
      (load-theme 'solarized-light t)
    (load-theme 'zenburn t)))

;; built-in packages.
;; use emacs's pinentry
(use-package epa-file
  :config
  (epa-file-enable)
  (setq epg-gpg-program  "/usr/bin/gpg2"
        epg-pinentry-mode 'loopback))

;; built-in packages.
;; show line num on the left hand side
(use-package display-line-numbers
  :hook
  (prog-mode . display-line-numbers-mode)
  (yaml-mode . display-line-numbers-mode))

;; tramp mode
;; https://stackoverflow.com/questions/3465567/how-to-use-ssh-and-sudo-together-with-tramp-in-emacs
(use-package tramp
  :ensure t
  :config
  (setq tramp-default-method "ssh")
  (setq tramp-use-ssh-controlmaster-options nil) ; Don't override SSH config.
  (add-to-list 'tramp-default-proxies-alist '(nil "\\`root\\'" "/ssh:%h:"))
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))

;; mark-active will break when move cursor vertically in emacs-master.
;; disable for now until it fix.
;; Note: install nerd-icon in ansible instead
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :config
  (setq doom-modeline-icon t)
  (setq doom-modeline-height 1)
  (set-face-attribute 'mode-line nil :height 0.9)
  (set-face-attribute 'mode-line-inactive nil :height 0.9))

;;;; use minion and enable some minor modes
;;(use-package minions
;;  :ensure t
;;  :config
;;  (minions-mode 1)
;;  (setq minions-prominent-modes '(flycheck-mode)))

;; jump window like tmux
(use-package ace-window
  :ensure t
  :defer t
  :bind
  ("C-x q" . ace-window)
  :config
  (setq aw-keys '(?a ?s ?d ?f ?j ?k ?l ?\;)))

;; key helper
(use-package which-key
  :ensure t
  :config (which-key-mode))

(use-package undo-tree
  :ensure t
  :config
  (global-undo-tree-mode)
  (setq undo-tree-auto-save-history nil))

(use-package company
  :ensure t
  :bind
  ("M-/" . company-complete)
  :hook
  (after-init . global-company-mode)
  :config
  (setq company-idle-delay 0.2
        company-backends '((company-capf company-dabbrev-code))))

(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1))

(use-package yasnippet-snippets
  :ensure t)

(use-package projectile
  :ensure t
  :init
  (projectile-mode +1)
  :config
  (setq projectile-project-search-path '("~/myhome/home-config/"
                                         "~/myhome/home-config-ansible/"
                                         "~/myhome/org/"
                                         "~/myhome/org_work/"
                                         ("~/myhome/playground" . 2)
                                         ("~/myhome/coding" . 5)))
  (setq projectile-auto-discover nil)
  :bind  (:map projectile-mode-map ("C-c p" . projectile-command-map)))


(use-package transient
  :ensure t)

(use-package magit
  :ensure t
  :bind
  ("C-x g" . magit-status))

(use-package zoom-window
  :ensure t
  :bind
  ("C-x z" . zoom-window-zoom))


(setq python-shell-interpreter "python3"
      python-shell-interpreter-args "-i")


(use-package cargo
  :ensure t
  :hook
  (rust-mode . cargo-minor-mode))


(use-package emamux
  :ensure t
  :bind (("C-c t C-s" . emamux:send-command)
         ("C-c t C-y" . emamux:yank-from-list-buffers)
         ("C-c t C-k" . emamux:close-runner-pane)
         ("C-c t c" . emamux:new-window)
         ("C-c t C" . emamux:clone-current-frame)
         ("C-c t 2" . emamux:split-window)
         ("C-c t 3" . emamux:split-window-horizontally)))

(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))

(use-package browse-at-remote
  :ensure t
  :config
  (add-to-list 'browse-at-remote-remote-type-regexps '(:host "^gitlab\\.cern\\.ch$" :type "gitlab"))
  :bind
  (("C-c g g" . browse-at-remote)))

;; upcase/downcase region without asking
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; eglot
(use-package eglot
  :ensure t
  :init
  (add-hook 'eglot--managed-mode-hook (lambda () (flymake-mode -1)))
  :hook
  (python-mode . eglot-ensure)
  (yaml-mode . eglot-ensure)
  (sh-mode . eglot-ensure))

;; ======================== start vertico/consult ====================

(use-package vertico
  :ensure t
  :init
  (vertico-mode))

(use-package consult
  :ensure t
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings (mode-specific-map)
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings (ctl-x-map)
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings (goto-map)
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings (search-map)
         ("M-s d" . consult-find)
         ("M-s D" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; By default `consult-project-function' uses `project-root' from project.el.
  ;; Optionally configure a different project root function.
  ;;;; 1. project.el (the default)
  ;; (setq consult-project-function #'consult--default-project--function)
  ;;;; 2. vc.el (vc-root-dir)
  ;; (setq consult-project-function (lambda (_) (vc-root-dir)))
  ;;;; 3. locate-dominating-file
  ;; (setq consult-project-function (lambda (_) (locate-dominating-file "." ".git")))
  ;;;; 4. projectile.el (projectile-project-root)
  ;; (autoload 'projectile-project-root "projectile")
  ;; (setq consult-project-function (lambda (_) (projectile-project-root)))
  ;;;; 5. No project support
  ;; (setq consult-project-function nil)
)

;; Optionally use the `orderless' completion style.
(use-package orderless
  :ensure t
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

;; ======================== end vertico/consult ====================

(use-package flycheck
  :ensure t
  :config
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))
  (setq flycheck-check-syntax-automatically '(mode-enabled save))
  :hook
  ((python-mode . (lambda ()
                    (flycheck-mode)
                    (setq flycheck-checker 'python-pylint)
                    (flycheck-add-next-checker 'python-pylint 'python-pyright)))
   (sh-mode . (lambda ()
                (flycheck-mode)
                (flycheck-select-checker 'sh-shellcheck)))
   (yaml-mode . (lambda ()
                  (flycheck-mode)
                  (flycheck-select-checker 'yaml-yamllint)))
   (puppet-mode . (lambda ()
                    (flycheck-mode)
                    (setq flycheck-checker 'puppet-parser)
                    (flycheck-add-next-checker 'puppet-parser 'puppet-lint)))
   ;; simply enable for all prog-mode
   (prog-mode . (lambda ()
                    (flycheck-mode)))))


(use-package expand-region
  :ensure t
  :bind (("C-=" . 'er/expand-region)))


(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory (file-truename "~/myhome/org/roam"))
  (org-roam-completion-everywhere t)
  (org-roam-capture-templates
   '(("d" "default" plain
      "%?"
      :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                         "#+title: ${title}\n")
      :unnarrowed t)
   ("f" "fleeting note" plain
      "* %?"
      :target (file+head "%<%Y%m%d>-fleet.org"
                         "#+title: %<%Y-%m-%d>-fleeting-note\n")
      :unnarrowed t)
   ))

  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ;; Dailies
         ("C-c n j" . org-roam-dailies-capture-today)

         ("C-M-i"    . completion-at-point))
  :config
  (org-roam-db-autosync-mode))

(use-package envrc
  :ensure t
  :config (envrc-global-mode))

(use-package jinx
  :ensure t
  :hook (org-mode . jinx-mode)
  :bind (("M-$" . jinx-correct)
         ("C-M-$" . jinx-languages)))

;; testing tree-sitter
;; use builtin treesit.el and only for python and yaml
(use-package treesit-auto
  :ensure t
  :config
  (global-treesit-auto-mode)
  ;; somehow it does not work with latest emacs master HEAD.
  ;; but still need it to notify (warning message)  if grammar is not installed.
  ;; install grammar manually via treesit-install-language-grammar
  (setq treesit-auto-install t)
  (setq python-ts-mode-hook python-mode-hook
        yaml-ts-mode-hook yaml-mode-hook)
  ;; sh-mode is changed to bash-ts-mode by this package.
  (setq bash-ts-mode-hook sh-mode-hook))


(use-package treesit-fold
  :ensure t
  :bind  (("C-c f f" . treesit-fold-toggle)
          ("C-c f o" . treesit-fold-open)
          ("C-c f c" . treesit-fold-close)
          ("C-c f O" . treesit-fold-open-recursively)
          ("C-c f M-o" . treesit-fold-open-all)
          ("C-c f M-c" . treesit-fold-close-all)))

;; major mode
(use-package nix-mode
  :ensure t)

(use-package dockerfile-mode
  :ensure t)

(use-package groovy-mode
  :ensure t)
(use-package php-mode
  :ensure t)

(use-package go-mode
  :ensure t)

(use-package rust-mode
  :ensure t
  :config
  (setq rust-format-on-save t))
(use-package lua-mode
  :ensure t)

(use-package puppet-mode
  :ensure t)

(use-package yaml-mode
  :ensure t)

(use-package jsonnet-mode
  :ensure t
  :config
  (setq jsonnet-use-smie t)
  :hook
  (jsonnet-mode . (lambda ()
                    (setq indent-tabs-mode t
                          tab-width 2))))

(use-package json-mode
  :ensure t
  :config
  ;; set tab size to 4
  (setq json-encoding-default-indentation "    ")
  )

(use-package terraform-mode
  :ensure t)

;; load all file in loads dir
(mapc 'load (directory-files-recursively "~/.emacs.d/loads" ".el$"))

;; ================== deprecate =====================

;; avy: alternative ace-jump-mode
;;(use-package avy
;;  :defer t
;;  :bind* ("C-." . avy-goto-char-timer)
;;  :config
;;  (avy-setup-default))

;;(use-package sudo-edit
;;  :defer t
;;  :bind
;;  (("C-c C-r" . sudo-edit)))

;;(use-package lsp-mode
;;
;;  :init (setq lsp-headerline-breadcrumb-enable nil)
;;  :hook
;;  (python-mode . (lambda ()
;;                   (lsp)
;;                   (setq-default lsp-diagnostics-provider :none)))
;;  (lsp-mode . lsp-enable-which-key-integration)
;;  :commands lsp)
;;
;;;; temporary disable
;;;;(use-package lsp-ui
;;;;
;;;;  :bind
;;;;  (:map lsp-ui-mode-map
;;;;        ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
;;;;        ([remap xref-find-references]  . lsp-ui-peek-find-references))
;;;;  :commands lsp-ui-mode)
;;
;;(use-package lsp-ivy
;;  :commands lsp-ivy-workspace-symbol)

;;(use-package lsp-jedi
;;
;;  :config
;;  (with-eval-after-load "lsp-mode"
;;    (add-to-list 'lsp-disabled-clients 'pyls)
;;    (add-to-list 'lsp-enabled-clients 'jedi)))

;; move to flymake later, or maybe not
;; copy from https://github.com/purcell/emacs.d/blob/6eec82f623d6a866cba1b182c63d6d11446d88c4/lisp/init-flymake.el#L15-L18
;;(use-package flymake-flycheck
;;
;;  :init
;;  (defun sanityinc/enable-flymake-flycheck ()
;;    (setq-local flymake-diagnostic-functions
;;                (append flymake-diagnostic-functions
;;                        (flymake-flycheck-all-chained-diagnostic-functions))))
;;  :hook
;;  (flymake-mode . sanityinc/enable-flymake-flycheck)
;;  :bind
;;  (("C-c ! n" . flymake-goto-next-error)
;;  ("C-c ! p" . flymake-goto-prev-error)
;;  ("C-c ! c" . flymake-start)))

;; fix exec path for mac os, but I do not use mac anymore
;;(require 'exec-path-from-shell)
;;(when (memq window-system '(mac ns x))
;;  (exec-path-from-shell-initialize))

;; show ssh list in counsel
;;(use-package counsel-tramp
;;
;;  :config
;;  (define-key global-map (kbd "C-c s") 'counsel-tramp))

;; ================== testing section ======================

;;(use-package pdf-tools
;;  )

;;(use-package impatient-mode
;;  )
;;
;;(defun markdown-html (buffer)
;;  (princ (with-current-buffer buffer
;;    (format "<!DOCTYPE html><html><title>Impatient Markdown</title><xmp theme=\"united\" style=\"display:none;\"> %s  </xmp><script src=\"http://strapdownjs.com/v/0.2/strapdown.js\"></script></html>" (buffer-substring-no-properties (point-min) (point-max))))
;;  (current-buffer)))


;; (add-to-list 'org-export-backends 'md)

;; not sure why i need this
;;(require 'auth-source-pass)
;;(auth-source-pass-enable)
