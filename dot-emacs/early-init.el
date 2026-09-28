;; -*- mode: emacs-lisp; lexical-binding: nil; -*-
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
(setq max-specpdl-size 5)  ; default is 1000, reduce the backtrace level
(setq debug-on-error t)    ; now you should get a backtrace


;; ref: https://www.jamescherti.com/compiling-emacs/
;;
;; Display the architecture using:
;;   gcc -march=native -Q --help=target | grep march
;;
;; The above command asks the compiler to resolve native for your current CPU
;; and display the resulting target. For example, if the output shows
;; -march=skylake, you know that skylake is the identifier you should pass to
;; -mtune and -march.
;; (setq my-cpu-architecture "znver4")
(setq my-cpu-architecture
      (replace-regexp-in-string "\n\\'" ""
       (shell-command-to-string "gcc -march=native -Q --help=target | awk '$1 == \"-march=\" {print $2}'")))

;; `native-comp-compiler-options' specifies flags passed directly to the C
;; compiler (for example, GCC) when compiling the Lisp-to-C output
;; produced by the native compilation process. These flags affect code
;; generation, optimization, and debugging information.
(setq native-comp-compiler-options `(;; The most meaningful optimizations:
                                     "-O2"
                                     ,(format "-mtune=%s" my-cpu-architecture)
                                     ,(format "-march=%s" my-cpu-architecture)
                                     ;; Reduce .eln size and compilation
                                     ;; overhead.
                                     "-g0"
                                     ;; Good defensive choice for Emacs
                                     ;; stability.
                                     "-fno-omit-frame-pointer"
                                     "-fno-finite-math-only"))

(setq native-comp-driver-options '(;; -Wl,-z,pack-relative-relocs compresses
                                   ;; relocation tables to reduce file size and
                                   ;; slightly improve load times.
                                   "-Wl,-z,pack-relative-relocs"
                                   ;; -Wl,-O2 applies standard linker-level
                                   ;; optimizations (like string merging) to the
                                   ;; generated shared object.
                                   "-Wl,-O2"
                                   ;; -Wl,--as-needed prevents the linker from
                                   ;; recording dependencies on libraries that
                                   ;; are not actually used by the code.
                                   "-Wl,--as-needed"))

(setq load-prefer-newer t)
