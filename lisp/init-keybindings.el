(provide 'init-keybindings)

(use-package  hydra)

(use-package which-key
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-idle-delay 0.3))

(use-package evil-org
  :after org
  :hook (org-mode . (lambda () evil-org-mode))
  :config
  (require 'evil-org-agenda)
  (evil-org-set-key-theme '(navigation todo insert textobjects additional))
  (evil-org-agenda-set-keys))

  (dn/leader-key-def
    "o"   '(:ignore t :which-key "org mode")
    "oi"  '(:ignore t :which-key "insert")
    "oil" '(org-insert-link :which-key "insert link")
    "on"  '(org-toggle-narrow-to-subtree :which-key "toggle narrow")
    "os"  '(dw/counsel-rg-org-files :which-key "search notes")
    "oa"  '(org-agenda :which-key "status")
    "ot"  '(org-todo-list :which-key "todos")
    "oc"  '(org-capture t :which-key "capture")
    "ox"  '(org-export-dispatch t :which-key "export"))

(dn/leader-key-def
  "fn" '((lambda () (interactive) (counsel-find-file "~/Notes/")) :which-key "notes")
  "fd"  '(:ignore t :which-key "dotfiles")
  "fde" '((lambda () (interactive) (find-file (expand-file-name "~/.dotfiles/emacs.d/README.org"))) :which-key "edit config")
  "fdE" '((lambda () (interactive) (dw/org-file-show-headings "~/.dotfiles/emacs.d/README.org")) :which-key "edit config"))
