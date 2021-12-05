;; This sets up the load path so that we can override it
(setq warning-suppress-log-types '((package reinitialization)))  (package-initialize)
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp")
(push (expand-file-name "vendor/org-mode/lisp" (file-name-directory user-init-file)) load-path)
(push (expand-file-name "vendor/org-mode/contrib/lisp" (file-name-directory user-init-file)) load-path)
(setq use-package-always-ensure t)
(push (expand-file-name "lisp/" (file-name-directory user-init-file)) load-path)

(require 'init-packages)
(require 'init-config)
(require 'init-vc)
(require 'init-org)
