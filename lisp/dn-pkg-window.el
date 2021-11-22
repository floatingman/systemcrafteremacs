;;;; -*- lexical-binding: t; -*-

(setup (:pkg ace-window)
  (setq aw-background t)
  (setq aw-scope 'frame)
  (ace-window-display-mode 1)
  (:hide-mode))

(setup (:pkg popper)
  (:global-bind
   "<M-tab>" popper-toggle-latest
   "<M-SPC>" popper-cycle)
  (setq popper-display-control nil)
  (setq popper-group-function nil)
  (setq popper-mode-line nil)
  (setq popper-reference-buffers
        '(compilation-mode
          help-mode
          helpful-mode
          org-roam-mode
          sly-mrepl-mode
          term-mode))
  (popper-mode 1))

(setup (:pkg shackle)
  (let ((git '(magit-status-mode))
        (help '(help-mode helpful-mode))
        (org '(org-roam-mode))
        (repl "\\*\\(sly-mrepl\\|ielm\\)"))
    (setq shackle-rules
          `((,git :select t :align right :size 0.5)
            (,help :select t :align right :size 0.4)
            (,org :noselect t :align right :size 0.33)
            (,repl :regexp t :noselect t :align below :size 0.4)))
    (shackle-mode 1)))

(setup (:pkg windmove)
  (windmove-default-keybindings))

(setup (:pkg winner)
  (winner-mode 1))

(provide 'dn-pkg-window)
