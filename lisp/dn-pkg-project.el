;;;; -*- lexical-binding: t; -*-

(setup (:pkg git-timemachine)
  (:with-state normal
    (:bind
     "[" git-timemachine-show-previous-revision
     "]" git-timemachine-show-next-revision
     "b" git-timemachine-blame)))

(setup (:pkg magit)
  (setq git-commit-summary-max-length 120)
  (setq magit-commit-show-diff nil)
  (setq magit-delete-by-moving-to-trash nil)
  (setq magit-display-buffer-function
        #'magit-display-buffer-same-window-except-diff-v1)
  (setq magit-log-auto-more t)
  (setq magit-log-margin-show-committer-date t)
  (setq magit-revert-buffers 'silent)
  (setq magit-save-repository-buffers 'dontask)
  (setq magit-wip-after-apply-mode t)
  (setq magit-wip-after-save-mode t)
  (setq magit-wip-before-change-mode t)
  (setq transient-values
        '((magit-log:magit-log-mode "--graph" "--color" "--decorate"))))

(setup magit-wip
  (:load-after magit
    (magit-wip-mode 1)
    (:hide-mode)))

(setup (:pkg magit-todos)
  (:load-after magit
    (magit-todos-mode 1)))

(setup (:pkg persp-projectile)
  (:load-after (perspective projectile)))

(setup (:pkg perspective)
  (setq persp-modestring-short t)
  (setq persp-show-modestring t)
  (setq persp-sort 'created)
  (setq persp-state-default-file (mf/etc-file "perspectives"))
  (setq persp-switch-wrap nil)
  (persp-mode 1)
  (:global-bind
   "M-1" (fn! (persp-switch-by-number 1))
   "M-2" (fn! (persp-switch-by-number 2))
   "M-3" (fn! (persp-switch-by-number 3))
   "M-4" (fn! (persp-switch-by-number 4))
   "M-5" (fn! (persp-switch-by-number 5))
   "M-6" (fn! (persp-switch-by-number 6))
   "M-7" (fn! (persp-switch-by-number 7))
   "M-8" (fn! (persp-switch-by-number 8))
   "M-9" (fn! (persp-switch-by-number 9))))

(setup (:pkg projectile)
  (setq projectile-cache-file (mf/etc-file "projectile.cache"))
  (setq projectile-kill-buffers-filter 'kill-only-files)
  (setq projectile-known-projects-file (mf/etc-file "projectile-bookmarks"))
  (projectile-mode 1)
  (setq projectile-find-dir-includes-top-level t)
  (setf projectile-globally-ignored-directories
        (delete-dups (append projectile-globally-ignored-directories
                             mf/ignored-directories)))
  (setq projectile-globally-ignored-file-suffixes mf/ignored-suffixes)
  (:with-idle-delay 15 (quiet! (projectile-cleanup-known-projects)))
  (:hide-mode))

(provide 'dn-pkg-project)
