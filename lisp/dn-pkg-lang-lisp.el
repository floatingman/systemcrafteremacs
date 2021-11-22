;;;; -*- lexical-binding: t; -*-

(setup (:pkg evil-cleverparens)
  (setq evil-cleverparens-swap-move-by-word-and-symbol t)
  (setq evil-cleverparens-use-additional-bindings nil)
  (setq evil-cleverparens-use-additional-movement-keys nil)
  (setq evil-cleverparens-use-regular-insert t)
  (:load-after (evil smartparens evil-smartparens)
    (:hook-into emacs-lisp-mode
                eval-expression-minibuffer-setup
                ielm-mode
                lisp-interaction-mode
                lisp-mode
                sly-mrepl-mode)
    (:hide-mode)))

(setup (:pkg evil-smartparens)
  (:load-after (evil smartparens)
    (:hook-into smartparens-enabled-hook)
    (:hide-mode)))

(setup (:pkg rainbow-delimiters)
  (setq rainbow-delimiters-max-face-count 2)
  (:hook-into emacs-lisp-mode-hook
              eval-expression-minibuffer-setup-hook
              ielm-mode-hook
              lisp-interaction-mode-hook
              lisp-mode-hook
              sly-mrepl-mode-hook)
  (:hide-mode))

(setup (:pkg smartparens)
  (:require smartparens)
  (setq sp-cancel-autoskip-on-backward-movement nil)
  (setq sp-highlight-pair-overlay nil)
  (setq sp-highlight-wrap-overlay nil)
  (setq sp-highlight-wrap-tag-overlay nil)
  (setq sp-max-pair-length 2)
  (setq sp-max-prefix-length 32)
  (setq sp-message-width nil)
  (setq sp-navigate-consider-sgml-tags nil)
  (setq sp-navigate-skip-match nil)
  (setq sp-show-pair-from-inside t)
  (sp-pair "'" nil :actions :rem)
  (sp-pair "`" nil :actions :rem)
  (sp-pair "(" nil :unless '(:rem sp-point-before-word-p))
  (:with-hook (emacs-lisp-mode-hook
               eval-expression-minibuffer-setup-hook
               ielm-mode-hook
               lisp-interaction-mode-hook
               lisp-mode-hook
               sly-mrepl-mode-hook)
    (:hook smartparens-strict-mode))
  (:hide-mode))

(define-local-keys (emacs-lisp-mode-map sly-mrepl-mode-map lisp-mode-map)
  :infix "l"
  "" '(:ignore t :wk "lisp")
  "a" '(sp-absorb-sexp :wk "absorb")
  "b" '(sp-forward-barf-sexp :wk "barf forward")
  "B" '(sp-backward-barf-sexp :wk "barf backward")
  "c" '(sp-convolute-sexp :wk "convolute")
  "e" '(sp-splice-sexp-killing-backward :wk "splice killing backward")
  "E" '(sp-splice-sexp-killing-forward :wk "splice killing forward")
  "j" '(sp-join-sexp :wk "join")
  "r" '(sp-raise-sexp :wk "raise")
  "s" '(sp-forward-slurp-sexp :wk "slurp forward")
  "S" '(sp-backward-slurp-sexp :wk "slurp backward")
  "t" '(sp-transpose-sexp :wk "transpose")
  "w" '(sp-wrap-round :wk "wrap")
  "W" '(sp-unwrap-sexp :wk "unwrap"))

(define-local-keys (lisp-mode-map emacs-lisp-mode-map)
  "m" '(macrostep-expand :wk "macro expand"))

(provide 'dn-pkg-lang-lisp)
