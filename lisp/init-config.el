(setq backup-directory-alist '(("." . "~/.cache/emacs/backups")))

(setq user-emacs-directory
      (expand-file-name "emacs/" (or (getenv "XDG_CACHE_HOME") "~/.cache/")))

;; This sets up the load path so that we can override it
(setq warning-suppress-log-types '((package reinitialization)))  (package-initialize)
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp")
(push (expand-file-name "vendor/org-mode/lisp" (file-name-directory user-init-file)) load-path)
(push (expand-file-name "vendor/org-mode/contrib/lisp" (file-name-directory user-init-file)) load-path)
(setq custom-file "~/.config/emacs/custom-settings.el")
(setq use-package-always-ensure t)
(load custom-file t)
(push (expand-file-name "lisp/" (file-name-directory user-init-file)) load-path)

(require 'init-vc)
(require 'init-org)
