(defconst *spell-check-support-enabled* nil) ;; Enable with t if you prefer
(defconst *is-a-mac* (eq system-type 'darwin))

(defvar my-laptop-p (equal (system-name) "sunstreaker"))
(defvar my-server-p (and (equal (system-name) "localhost") (equal user-login-name "dnewman")))
(defvar my-phone-p (not (null (getenv "ANDROID_ROOT")))
  "If non-nil, GNU Emacs is running on Termux.")
(when my-phone-p (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))
(global-auto-revert-mode)  ; simplifies syncing

(provide 'init-system)
