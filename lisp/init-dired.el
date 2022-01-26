(use-package dired
  :ensure nil
  :demand
  :init
  (defun my-find-name-dired (pattern)
    "Find files in `default-directory' using `rg' if available.
  PREFIX forces the use of `find'."
    (interactive "sFind-name (filename wildcard): ")
    (if (and (not current-prefix-arg) (executable-find "rg"))
        (let ((find-program (concat "rg -g " (shell-quote-argument pattern) " --files"))
              (find-ls-option (cons "" "-dilsb")))
          (find-dired default-directory ""))
      (find-dired
       default-directory
       (concat find-name-arg " " (shell-quote-argument pattern)))))

  (setq dired-auto-revert-buffer t)
  (setq dired-no-confirm
        '(byte-compile chgrp chmod chown copy delete load move symlink))
  (setq dired-deletion-confirmer (lambda (x) t))
  :bind (:map dired-mode-map ("`" . dired-toggle-read-only))
  :config

  ;; Rename files editing their names in dired buffers
  (use-package wdired
    :init
    ;; allow changing of file permissions
    (setq wdired-allow-to-change-permissions t))

  ;; dired+ adds some features to standard dired (like reusing buffers)
  (use-package dired+
    :ensure nil
    :quelpa (dired+ :fetcher url :url "https://www.emacswiki.org/emacs/download/dired+.el")
    :defer 1
    :init
    (setq diredp-hide-details-initially-flag nil)
    (setq diredp-hide-details-propagate-flag nil)

    :config
    (diredp-toggle-find-file-reuse-dir 1)))

(provide 'init-dired)
