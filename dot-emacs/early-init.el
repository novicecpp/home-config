;; disable package.el, use elpaca instead
(setq package-enable-at-startup nil)

;; derived from https://github.com/purcell/emacs.d/blob/4ea0b79754c3054e62c5fe6e94319b715a2698ba/init.el#L27
(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'emacs-startup-hook
    (lambda () (setq gc-cons-threshold (* 20 1024 1024))))
(setq read-process-output-max (* 4 1024 1024))
(setq process-adaptive-read-buffering nil) ;; I do not know what it is...

;; for debug
;; https://stackoverflow.com/questions/1322591/tracking-down-max-specpdl-size-errors-in-emacs/1322978
;;(setq max-specpdl-size 5)  ; default is 1000, reduce the backtrace level
;;(setq debug-on-error t)    ; now you should get a backtrace
