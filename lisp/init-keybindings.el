(provide 'init-keybindings)

(use-package  hydra)

(use-package which-key
  :diminish
  :custom
  (which-key-show-docstrings 'docstring-only)
  (which-key-max-discription-length nil)
  (which-key-side-window-max-height 0.75)
  :config
  (which-key-mode))
