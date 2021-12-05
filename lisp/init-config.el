(setq backup-directory-alist '(("." . "~/.cache/emacs/backups")))

(setq user-emacs-directory
      (expand-file-name "emacs/" (or (getenv "XDG_CACHE_HOME") "~/.cache/")))

(setq delete-old-versions -1)
(setq version-control t)
(setq vc-make-backup-files t)
(setq auto-save-file-name-transforms '((".*" "~/.cache/emacs/auto-save-list/" t)))

(setq savehist-file "~/.cache/emacs/savehist")
(savehist-mode 1)
(setq history-length t)
(setq history-delete-duplicates t)
(setq savehist-save-minibuffer-history 1)
(setq savehist-additional-variables
      '(kill-ring
        search-ring
        regexp-search-ring))

;; Change the user-emacs-directory to keep unwanted things out of ~/.emacs.d
(setq url-history-file (expand-file-name "url/history" user-emacs-directory))

;; Use no-littering to automatically set common paths to the new user-emacs-directory
(setup (:straight no-littering)
  (require 'no-littering))

;; Keep customization settings in a temporary file (thanks Ambrevar!)
(setq custom-file
      (if (boundp 'server-socket-dir)
          (expand-file-name "custom.el" server-socket-dir)
        (expand-file-name (format "emacs-custom-%s.el" (user-uid)) temporary-file-directory)))
(load custom-file t)

(provide 'init-config)
