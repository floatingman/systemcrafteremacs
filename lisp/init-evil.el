(use-package undo-tree
  :config
  ;; Always have it on
  (global-undo-tree-mode)

  ;; Each node in the undo tree should have a timestamp.
  (setq undo-tree-visualizer-timestamps t)

  ;; Show a diff window displaying changes between undo nodes.
  (setq undo-tree-visualizer-diff t))

    (use-package  evil
      :config
      ;; Pre-load configuration
      (setq evil-want-integration t)
      (setq evil-want-keybinding nil)
      (setq evil-want-C-u-scroll t)
      (setq evil-want-C-i-jump nil)
      (setq evil-respect-visual-line-mode t)
      (setq evil-undo-system 'undo-tree)

      ;; Activate the Evil
      (evil-mode 1)

      ;; Set Emacs state modes
      (dolist (mode '(custom-mode
                      eshell-mode
                      git-rebase-mode
                      erc-mode
                      circe-server-mode
                      circe-chat-mode
                      circe-query-mode
                      sauron-mode
                      term-mode))
        (add-to-list 'evil-emacs-state-modes mode))

      (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
      (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

      ;; Use visual line motions even outside of visual-line-mode buffers
      (evil-global-set-key 'motion "j" 'evil-next-visual-line)
      (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

      (evil-set-initial-state 'messages-buffer-mode 'normal)
      (evil-set-initial-state 'dashboard-mode 'normal))

    (use-package evil-collection
      :init
      ;; Is this a bug in evil-collection?
      (setq evil-collection-company-use-tng nil)
      :after (evil)
      :config
      (evil-collection-outline-bind-tab-p nil
      (remove evil-collection-mode-list) 'lispy
      (remove evil-collection-mode-list) 'org-present)
      (evil-collection-init))

(use-package general
  :init
  (general-evil-setup t)
  :config
  (general-create-definer dn/leader-key-def
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")

  (general-create-definer dn/ctrl-c-keys
    :prefix "C-c"))

(provide 'init-evil)
