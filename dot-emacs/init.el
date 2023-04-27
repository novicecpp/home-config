;; for debug
;; https://stackoverflow.com/questions/1322591/tracking-down-max-specpdl-size-errors-in-emacs/1322978
;;(setq max-specpdl-size 5)  ; default is 1000, reduce the backtrace level
;;(setq debug-on-error t)    ; now you should get a backtrace

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

;;(setq gc-cons-threshold most-positive-fixnum)
(setq gc-cons-percentage 0.9)
(setq read-process-output-max (* 1024 1024))

(setq inhibit-startup-screen t)

(defvar bootstrap-version)
(progn (setq straight-repository-branch "develop")
       (let ((bootstrap-file
              (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
             (bootstrap-version 6))
         (unless (file-exists-p bootstrap-file)
           (with-current-buffer
               (url-retrieve-synchronously
                "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
                'silent 'inhibit-cookies)
             (goto-char (point-max))
             (eval-print-last-sexp)))
         (load bootstrap-file nil 'nomessage)))

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
(setq straight-use-package-by-default t)
(straight-use-package 'use-package)

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

;; theme
(use-package solarized-theme)
(use-package zenburn-theme
  :config
  ;; load solarized theme when run in test machine
  (if (file-exists-p "~/.emacs_test")
      (load-theme 'solarized-light t)
    (load-theme 'zenburn t)))

;; https://www.emacswiki.org/emacs/SavePlace
(save-place-mode 1)

;; auto refresh when file change
;; http://stackoverflow.com/questions/1480572/how-to-have-emacs-auto-refresh-all-buffers-when-files-have-changed-on-disk
(global-auto-revert-mode t)
(setq auto-revert-interval 2)

;; highlight parenthesis
(show-paren-mode 1)

;; tramp mode
;; https://stackoverflow.com/questions/3465567/how-to-use-ssh-and-sudo-together-with-tramp-in-emacs
(use-package tramp
  :defer t
  :straight (tramp :type built-in)
  :config
  (setq tramp-default-method "ssh")
  (setq tramp-use-ssh-controlmaster-options nil) ; Don't override SSH config.
  (add-to-list 'tramp-default-proxies-alist '(nil "\\`root\\'" "/ssh:%h:"))
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))

;; delete trailing whitespace when save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; disable lockfiles feature
(setq create-lockfiles nil)

;; for doom-modeline, also resize icon to normal text
;; need to install fonts manually by execute `M-x all-the-icons-install-fonts`
(use-package all-the-icons
  :config
  (setq all-the-icons-scale-factor 0.9))

(use-package doom-modeline
  :config
  (setq doom-modeline-icon t)
  (setq doom-modeline-height 1)
  (set-face-attribute 'mode-line nil :height 0.9)
  (set-face-attribute 'mode-line-inactive nil :height 0.9)
  :hook (after-init . doom-modeline-mode))

;; jump window like tmux+ace-jump-mode
(use-package ace-window
  :defer t
  :bind
  ("C-x q" . ace-window)
  :config
  (setq aw-keys '(?a ?s ?d ?f ?j ?k ?l ?\;)))

;; fix exec path for mac os, but don't use mac anymore
;;(require 'exec-path-from-shell)
;;(when (memq window-system '(mac ns x))
;;  (exec-path-from-shell-initialize))

;; show ssh list in counsel
;;(use-package counsel-tramp
;;
;;  :config
;;  (define-key global-map (kbd "C-c s") 'counsel-tramp))

;; key helper
(use-package which-key
  :defer t
  :config (which-key-mode))

;; avy: alternative ace-jump-mode
(use-package avy
  :defer t
  :bind* ("C-." . avy-goto-char-timer)
  :config
  (avy-setup-default))

(use-package undo-tree
  :defer t
  :config
  (global-undo-tree-mode)
  (setq undo-tree-auto-save-history nil))

(use-package dockerfile-mode
  :defer t)

;;ivy
;;(use-package counsel
;;  )

;;(use-package ivy
;;
;;  :config
;;  (ivy-mode 1)
;;  (setq ivy-use-virtual-buffers t)
;;  (setq ivy-count-format "(%d/%d) ")
;;  :bind
;;  (("C-s" . swiper)
;;   ("M-x" . counsel-M-x)
;;   ;;("C-c g" . counsel-git)
;;   ("C-c j" . counsel-git-grep)
;;   ("C-c k" . counsel-ag)
;;   ("C-c C-c C-l" . counsel-locate)
;;   ("C-c C-r" . ivy-resume)))

;;(use-package ivy-rich
;;  :ensure
;;  :init
;;  (ivy-rich-mode 1)
;;  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)
;;  :config
;;  (setq ivy-rich-parse-remote-buffer nil
;;        ivy-rich-parse-remote-file-path nil))

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

(use-package company
  :defer t
  :diminish
  :bind
  ("M-/" . company-complete)
  :hook
  (after-init . global-company-mode)
  :config
  (setq company-idle-delay 0.2))

(use-package company-quickhelp
  :defer t
  :config
  (company-quickhelp-mode 1))


(use-package company-ansible
  :defer t
  :config  (push 'company-ansible company-backends))

(use-package yasnippet
  :defer t
  :config
  (yas-global-mode 1))

(use-package yasnippet-snippets
  :defer t)


(use-package projectile
  :defer t
  :config
  (projectile-mode)
  ;;(setq projectile-completion-system 'ivy)
  (setq projectile-project-search-path '("~/myhome/coding/home-config/"
                                         "~/myhome/coding/home-config-ansible/"
                                         ("~/myhome/coding" . 5)))
  (setq projectile-auto-discover nil)
  :bind
  ("C-c p" . projectile-command-map))

;;(use-package counsel-projectile
;;counsel
;;  :config
;;  (counsel-projectile-mode))

(use-package display-line-numbers
  :defer t
  :hook
  (prog-mode . display-line-numbers-mode)
  (yaml-mode . display-line-numbers-mode))

(use-package magit
  :defer t
  :bind
  ("C-x g" . magit-status))

(use-package zoom-window
  :defer t
  :bind
  ("C-x z" . zoom-window-zoom))


(setq python-shell-interpreter "python3"
      python-shell-interpreter-args "-i")

(use-package yaml-mode
  :defer t)
;;  :hook (yaml-mode . (lambda ()
;;                       (flycheck-select-checker 'yaml-yamllint)
;;                       (flycheck-mode))))

(use-package jsonnet-mode
  :defer t
  :config
  (setq jsonnet-use-smie t)
  :hook
  (jsonnet-mode . (lambda ()
                    (setq indent-tabs-mode t
                          tab-width 2))))

(use-package json-mode
  :defer t
  :config
  ;; set tab size to 4
  (setq json-encoding-default-indentation "    ")
  )

(use-package terraform-mode
  :defer t)

;;.auto-mode & interpreter-mode
(push '("python" . python-mode) interpreter-mode-alist)
(push '("bash" . sh-mode) interpreter-mode-alist)
(push '("\\.yaml\\'" . yaml-mode) auto-mode-alist)
(push '("\\.emacs_exwm\\'" . emacs-lisp-mode) auto-mode-alist)
(push '("\\.env\\'" . sh-mode) auto-mode-alist)
(push '("Dockerfile\\'" . dockerfile-mode) auto-mode-alist)

;; set default emacs web browser
;; browse-url-generic-program value is browser executable file
 (setq-default browse-url-browser-function 'browse-url-generic
               browse-url-generic-program "firefox")
;; follow symlink
(setq find-file-visit-truename t)

;;
(setq custom-file "~/.emacs.d/custom-set-variable.el")

;; temporary disable because straight.el cannot pull source
;; interact with gpg file
;;(use-package epa-file
;;  :config
;;  (epa-file-enable)
;;  (setq epg-gpg-program  "/usr/bin/gpg2"
;;        epg-pinentry-mode 'loopback))

;;(use-package dumb-jump
;;
;;  :bind
;;  (("M-g o" . dumb-jump-go-other-window)
;;   ("M-g j" . dumb-jump-go)
;;   ("M-g b" . dumb-jump-back)
;;   ("M-g i" . dumb-jump-go-prompt)
;;   ("M-g x" . dumb-jump-go-prefer-external)
;;   ("M-g z" . dumb-jump-go-prefer-external-other-window))
;;  :config
;;  (setq dumb-jump-selector 'ivy))

(use-package go-mode
  :defer t)

(use-package rust-mode
  :defer t
  :config
  (setq rust-format-on-save t))

(use-package cargo
  :defer t
  :hook
  (rust-mode . cargo-minor-mode))

(use-package emamux
  :defer t
  :config
  ;; copy keymap from emamux.el
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-s") #'emamux:send-command)
    (define-key map (kbd "C-y") #'emamux:yank-from-list-buffers)
    (define-key map (kbd "C-k") #'emamux:close-runner-pane)
    (define-key map (kbd "c")   #'emamux:new-window)
    (define-key map (kbd "C")   #'emamux:clone-current-frame)
    (define-key map (kbd "2")   #'emamux:split-window)
    (define-key map (kbd "3")   #'emamux:split-window-horizontally)
    (global-set-key (kbd "C-C t") map)))

;;(use-package blacken
;;
;;  :config
;;  (setq-default blacken-line-length 120))

(use-package lua-mode
  :defer t)
(use-package puppet-mode
  :defer t)


;; load all file in loads dir
(mapc 'load (directory-files-recursively "~/.emacs.d/loads" ".el$"))

(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

(use-package browse-at-remote
  :defer t
  :config
  (add-to-list 'browse-at-remote-remote-type-regexps '(:host "^gitlab\\.cern\\.ch$" :type "gitlab"))
  :bind
  (("C-c g g" . browse-at-remote)))

(use-package pipenv
  :defer t
  :hook (python-mode . pipenv-mode)
  :init
  (setq
   pipenv-projectile-after-switch-function
   #'pipenv-projectile-after-switch-extended)
  )

;; upcase/downcase region without asking
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;;;; ================== testing section ======================
;;(use-package pdf-tools
;;  )

;; eglot testing
(use-package eglot
  :straight (eglot :type built-in)
  :defer t
  :init
  (add-hook 'eglot--managed-mode-hook (lambda () (flymake-mode -1)))
  :hook
  (python-mode . eglot-ensure)
  (yaml-mode . eglot-ensure)
  (sh-mode . eglot-ensure))

(use-package vertico
  :defer t
  :init
  (vertico-mode))

(use-package consult
  :defer t
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
  :defer t
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))




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


(use-package flycheck
  :defer t
  :config
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))
  (setq flycheck-check-syntax-automatically '(mode-enabled save))
  :hook
  ((python-mode . (lambda ()
                    (flycheck-mode)
                    (flycheck-select-checker 'python-pyright)
                    (flycheck-add-next-checker 'python-pyright 'python-pycheckers)))
   (sh-mode . (lambda ()
                (flycheck-mode)
                (flycheck-select-checker 'sh-shellcheck)))
   (yaml-mode . (lambda ()
                  (flycheck-mode)
                  (flycheck-select-checker 'yaml-yamllint)))))



;;
(use-package flycheck-pycheckers
  :defer t
  :config
  (add-hook 'flycheck-mode-hook #'flycheck-pycheckers-setup)
  (setq flycheck-pycheckers-checkers '(pylint flake8)))

;;(use-package lsp-pyright
;;
;;  :hook (python-mode . (lambda ()
;;                          (require 'lsp-pyright)
;;                          (lsp)  ;; or lsp-deferred
;;                          (flycheck-add-next-checker 'lsp 'python-pycheckers))))


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


;; testing tree-sitter
;; use builtin treesit.el and only for python and yaml
(use-package treesit-auto
  :defer t
  :config
  (global-treesit-auto-mode)
  (setq treesit-auto-install t)
  (setq python-ts-mode-hook python-mode-hook
        yaml-ts-mode-hook yaml-mode-hook)
  ;; sh-mode is changed to bash-ts-mode by this package.
  (setq bash-ts-mode-hook sh-mode-hook))



;; shameless copy from Gary Oberbrunner’s https://github.com/garyo/emacs-config/blob/36639b9d771c68611f4be2786d74319229fc24bd/emacs-config.org
;; manually clone ts-fold forked from Andrew Swerlick's https://github.com/AndrewSwerlick/ts-fold/tree/andrew-sw/treesit-el-support
;; and put in ~/.emacs.d/clone/ts-fold
;; still need to figure it out why the there is error everytime toggle ts-fold
;; ts-fold--after-command: Symbol’s function definition is void: ts-fold-indicators-refresh
;; and ts-fold does not load properly with hydra when use (use-package ts-fold :load-path /path)


(use-package hydra
  :defer t)

(defhydra hydra-ts-fold (:exit t :hint nil)
  "
Tree-sitter code folding
Point^^                     Recursive^^             All^^
^^^^^^---------------------------------------------------------------
[_f_] toggle fold at point
[_o_] open at point         [_O_] open recursively  [_M-o_] open all
[_c_] close at point         ^ ^                    [_M-c_] close all"
  ("f" ts-fold-toggle)
  ("o" ts-fold-open)
  ("c" ts-fold-close)
  ("O" ts-fold-open-recursively)
  ("M-o" ts-fold-open-all)
  ("M-c" ts-fold-close-all))


(use-package ts-fold
  :straight (ts-fold :type git :host github
                     :repo "AndrewSwerlick/ts-fold"
                     :branch "andrew-sw/treesit-el-support")
  :defer t)


(global-set-key (kbd "C-c f") 'hydra-ts-fold/body)
