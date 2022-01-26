(use-package magit
  :demand
  :diminish magit-wip-after-apply-mode
  :init
  (setq magit-no-confirm '(stage-all-changes))
  (setq magit-push-always-verify nil)
  (setq git-commit-finish-query-functions nil)
  (setq magit-save-some-buffers nil) ;don't ask to save buffers
  (setq magit-set-upstream-on-push t) ;ask to set upstream
  (setq magit-diff-refine-hunk 'all) ;show word-based diff for all hunks
  (setq magit-default-tracking-name-function
        'magit-default-tracking-name-branch-only) ;don't track with origin-*

  :config
  (setq magit-wip-after-save-mode 1)
  (setq magit-wip-after-apply-mode 1)
  ;; Emacs Minor mode to automatically commit and push
  (use-package git-auto-commit-mode
    :commands (gac-commit gac)
    :config
    (defun gac ()
      (interactive)
      (gac-commit))))

(use-package diff-hl
  :demand
  :config
  (global-diff-hl-mode 1)
  (eval-after-load 'magit
    (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)))

(provide 'init-vc)
