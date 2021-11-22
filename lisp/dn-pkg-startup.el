;;;; -*- lexical-binding: t; -*-

(setup show-startup-time
  (:with-hook emacs-startup-hook
    (:hook mf/show-startup-time)))

(setup (:pkg benchmark-init)
  (:disable) ; Disabled when not benchmarking.
  (define-advice define-obsolete-function-alias (:filter-args (ll))
    (let ((obsolete-name (pop ll))
          (current-name (pop ll))
          (when (if ll (pop ll) "1"))
          (docstring (if ll (pop ll) nil)))
      (list obsolete-name current-name when docstring)))
  (:require benchmark-init-modes)
  (:global-bind
   "<M-f2>" #'benchmark-init/show-durations-tabulated
   "<M-f3>" #'benchmark-init/show-durations-tree)
  (:with-hook after-init-hook
    (:hook benchmark-init/deactivate)))

(setup (:pkg auto-compile)
  (auto-compile-on-load-mode 1)
  (auto-compile-on-save-mode 1)
  (:hide-mode)
  (:hide-mode auto-compile-on-load))

(setup (:pkg gcmh)
  (setq gcmh-auto-idle-delay-factor 10)
  (setq gcmh-high-cons-threshold (* 128 1024 1024))
  (setq gcmh-idle-delay 'auto)
  (gcmh-mode 1)
  (:hide-mode))

(setup (:pkg no-littering)
  (:require no-littering))

(provide 'dn-pkg-startup)
