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

(use-package anzu
  :defer t
  :bind ("M-%" . anzu-query-replace-regexp)
  :config
  (progn
    (use-package thingatpt)
    (setq anzu-mode-lighter ""
          ;; spaceline already takes care of this
          anzu-cons-mode-line-p nil)
    (set-face-attribute 'anzu-mode-line nil :foreground "yellow")))

(add-hook 'prog-mode-hook #'anzu-mode)
(add-hook 'org-mode-hook #'anzu-mode)

(use-package darkroom
  :bind ("S-<f11>" . darkroom-tentative-mode)
  :custom
  (darkroom-text-scale-increase 3)
  (darkroom-margins-if-failed-guess 0.1))

(use-package default-text-scale)

(use-package multiple-cursors
  :ensure t
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)))

(provide 'init-editing)
