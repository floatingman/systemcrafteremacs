;;;; -*- lexical-binding: t; -*-

(setup (:pkg aggressive-indent)
  (:hook-into prog-mode sly-mrepl-mode)
  (:hide-mode))

(setup (:pkg avy)
  (setq avy-all-windows nil)
  (setq avy-background t)
  (setq avy-keys (nconc (number-sequence ?a ?z)
                        (number-sequence ?A ?Z)
                        (number-sequence ?1 ?9)))
  (setq avy-style 'pre))

(setup (:pkg evil)
  (setq evil-move-beyond-eol t)
  (setq evil-respect-visual-line-mode t)
  (setq evil-undo-system 'undo-fu)
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq-default evil-shift-width tab-width)
  (evil-mode 1))

(setup (:pkg evil-collection)
  (:load-after evil
    (evil-collection-init)
    (:hide-mode evil-collection-unimpaired-mode)))

(setup (:pkg evil-commentary)
  (:load-after evil
    (:with-mode prog-mode
      (:with-state (normal visual)
        (:bind
         "gc" evil-commentary)))))

(setup (:pkg evil-multiedit)
  (:load-after evil))

(setup (:pkg evil-surround)
  (:load-after evil
    (global-evil-surround-mode 1)))

(setup (:pkg expand-region)
  (:require expand-region)
  (:with-map prog-mode-map
    (:with-state visual
      (:bind
       "v" er/expand-region
       "V" er/contract-region))))

(setup (:pkg hungry-delete)
  (:load-after smartparens
    (setq hungry-delete-join-reluctantly t)
    (global-hungry-delete-mode 1)
    (:hook-into smartparens-enabled)
    (:hide-mode)))

(setup (:pkg undo-fu undo-fu-session)
  (:with-map (prog-mode-map text-mode-map)
    (:with-state normal
      (:bind
       "u" undo-fu-only-undo
       "C-r" undo-fu-only-redo)))
  (setq undo-fu-session-incompatible-files
        '("/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'"))
  (global-undo-fu-session-mode 1))

(setup (:pkg whitespace-cleanup-mode)
  (global-whitespace-cleanup-mode 1)
  (:with-hook before-save-hook
    (:hook delete-trailing-whitespace))
  (:hide-mode))

(provide 'dn-pkg-editing)
