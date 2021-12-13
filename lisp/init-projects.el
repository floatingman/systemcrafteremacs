(defun dw/switch-project-action ()
  "Switch to a workspace with the project name and start `magit-status'."
  ;; TODO: Switch to EXWM workspace 1?
  (persp-switch (projectile-project-name))
  (magit-status))

(use-package projectile
:config
  (when (file-directory-p "~/Repos")
    (setq projectile-project-search-path '("~/Repos")))
  (setq projectile-switch-project-action #'dw/switch-project-action)

  (projectile-mode)

  (:global "C-M-p" projectile-find-file
           "C-c p" projectile-command-map)

  (dn/leader-key-def
    "pf"  'projectile-find-file
    "ps"  'projectile-switch-project
    "pF"  'consult-ripgrep
    "pp"  'projectile-find-file
    "pc"  'projectile-compile-project
    "pd"  'projectile-dired))

(provide 'init-projects)
