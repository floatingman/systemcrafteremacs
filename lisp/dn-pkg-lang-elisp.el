;;;; -*- lexical-binding: t; -*-

(setup elisp-mode
  (define-local-keys emacs-lisp-mode-map
    "'" '(ielm :wk "ielm"))
  (:with-mode (emacs-lisp-mode lisp-interaction-mode)
    (:bind
     "C-c C-c" eval-defun))
  (:with-mode emacs-lisp-mode
    (:advise eval-region :around (fn beg end &rest args)
      (let ((pulse-flag t))
        (pulse-momentary-highlight-region beg end))
      (apply fn beg end args))))

(setup (:pkg elisp-slime-nav)
  (:load-after ielm
    (:hook-into emacs-lisp-mode ielm-mode)
    (:hide-mode)))

(setup (:pkg ielm)
  (:load-after comint
    (:with-map ielm-map
      (:bind
       [up] comint-previous-input
       [down] comint-next-input))))

(setup (:pkg macrostep)
  (:with-state normal
    (:with-map macrostep-keymap
      (:bind
       [tab] macrostep-next-macro
       [backtab] macrostep-prev-macro
       "c" macrostep-collapse
       "e" macrostep-expand
       "q" macrostep-collapse-all)))
  (:with-hook macrostep-mode-hook
    (:hook evil-normalize-keymaps))
  (:hide-mode))

(provide 'dn-pkg-lang-elisp)
