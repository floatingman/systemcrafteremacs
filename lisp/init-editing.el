(setq-default
  tab-width 4
  indent-tabs-mode nil                   ;use spaces instead of tabs
  c-basic-offset 4                       ;"tab" with in c-related modes
  c-hungry-delete-key t)                 ;delete more than one space

(use-package undo-tree
  :config
  ;; Always have it on
  (global-undo-tree-mode)

  ;; Each node in the undo tree should have a timestamp.
  (setq undo-tree-visualizer-timestamps t)

  ;; Show a diff window displaying changes between undo nodes.
  (setq undo-tree-visualizer-diff t))

(use-package paren
  :config
  ;;visualize ( and )
  (show-paren-mode t))

(use-package prog-mode
  :straight nil
  :config
  (defun my-prog-mode-hook ()
    (setq show-trailing-whitespace 1)
    (prettify-symbols-mode 1))
  :hook (prog-mode . my-prog-mode-hook))

(use-package darkroom
  :bind ("S-<f11>" . darkroom-tentative-mode)
  :custom
  (darkroom-text-scale-increase 3)
  (darkroom-margins-if-failed-guess 0.1))

(use-package default-text-scale)

(provide 'init-editing)
