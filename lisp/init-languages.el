(use-package lsp-mode
  :commands (lsp)
  :init (setq lsp-eldoc-render-all nil
              lsp-keymap-prefix "C-c l"
              lsp-highlight-symbol-at-point nil
              lsp-prefer-flymake nil    ;; for metals, https://scalameta.org/metals/docs/editors/emacs.html
              lsp-inhibit-message t)
  )

(use-package lsp-ui
  :after lsp-mode
  :config
  (setq lsp-ui-sideline-update-mode 'point)
  :bind (
         :map lsp-ui-mode-map
              ("C-c C-SPC" . lsp-execute-code-action)
              )
   )

(provide 'init-languages)
