(provide 'init-keybindings)

(use-package  hydra :commands defhydra)
(use-package use-package-hydra)
(if my-laptop-p
    (use-package hydra-posframe :if my-laptop-p :quelpa (hydra-posframe :fetcher github :repo "Ladicle/hydra-posframe") :after hydra))

(use-package which-key
  :diminish
  :custom
  (which-key-show-docstrings 'docstring-only)
  (which-key-max-discription-length nil)
  (which-key-side-window-max-height 0.75)
  :config
  (which-key-mode))
