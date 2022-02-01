(use-package org
  :load-path ("~/Repos/org-mode/lisp" "~/Repos/org-contrib/lisp")
  :config
  (require 'oc-basic)                   ; started needing this
  (unless (functionp 'org-link-make-string)
    (fset 'org-link-make-string 'org-make-link-string))
  )

(setq org-modules '(org-habit
                    org-mouse
                    org-protocol
                    org-annotate-file
                    org-eval
                    org-expiry
                    org-interactive-query
                    org-collector
                    org-panel
                    org-screen
                    org-toc))
(eval-after-load 'org
  '(org-load-modules-maybe t))
;; Prepare stuff for org-export-backends
(setq org-export-backends '(org latex icalendar html ascii))

(use-package org-superstar
  :hook (org-mode . org-superstar-mode))

(provide 'init-org)
