(setq user-emacs-directory
      (expand-file-name "emacs/" (or (getenv "XDG_CACHE_HOME") "~/.cache/")))

(setq delete-old-versions -1)
(setq version-control t)
(setq vc-make-backup-files t)

(require 'whitespace)
(add-hook 'prog-mode-hook #'whitespace-mode)
(add-hook 'conf-mode-hook #'whitespace-mode)

(use-package savehist
  :config
  (setq savehist-additional-variables
        '(kill-ring
          mark-ring
          global-mark-ring
          search-ring
          regexp-search-ring
          extended-command-history))
  (savehist-mode 1))

(use-package saveplace
  :config (setq-default save-place t))

(defun dw/org-file-jump-to-heading (org-file heading-title)
  (interactive)
  (find-file (expand-file-name org-file))
  (goto-char (point-min))
  (search-forward (concat "* " heading-title))
  (org-overview)
  (org-reveal)
  (org-show-subtree)
  (forward-line))

(defun dw/org-file-show-headings (org-file)
  (interactive)
  (find-file (expand-file-name org-file))
  (counsel-org-goto)
  (org-overview)
  (org-reveal)
  (org-show-subtree)
  (forward-line))

(use-package recentf
  :config
  ;;(setq recentf-save-file (expand-file-name "~/.recentf"))
  (recentf-mode 1))

(add-hook 'after-make-frame-functions
          (lambda (frame)
            (select-frame frame)
            (my-setup-color-theme)))

(defvar my-todo "~/.emacs.d/README.org")

(setq
 inhibit-startup-message t
 require-final-newline t                ;auto add newline at the end of file
 column-number-mode t                   ;show the column number
 default-major-mode 'text-mode          ;use text mode per default
 mouse-yank-at-point t                  ;middle click with the mouse yanks at point
 history-length 250                     ;default is 30
 locale-coding-system 'utf-8            ;utf-8 is default
 tab-always-indent 'complete            ;try to complete before identing
 confirm-nonexistent-file-or-buffer nil ;don't ask to create a buffer
 vc-follow-symlinks t                   ;follow symlinks automatically
 recentf-max-saved-items 5000           ;save up to 5000 recent files
 eval-expression-print-length nil       ;do not truncate printed expressions
 eval-expression-print-level nil        ;print nested expressions
 send-mail-function 'sendmail-send-it
 kill-ring-max 5000                     ;truncate kill ring after 5000 entries
 mark-ring-max 5000                     ;truncate mark ring after 5000 entries
 mouse-autoselect-window -.1            ;window focus follows the mouse pointer
 mouse-wheel-scroll-amount '(1 ((shift) . 5) ((control))) ;make mouse scrolling smooth
 indicate-buffer-boundaries 'left       ;fringe markers (on the left side)
 enable-recursive-minibuffers t         ;whatever...
 show-paren-delay 0                     ;show the paren immediately
 load-prefer-newer t                    ;prefer newer .el instead of the .elc
 split-height-threshold 140             ;more readily split horziontally
 split-width-threshold 140              ;split horizontally only if less than 160 columns
 safe-local-variable-values '((engine . django))
 switch-to-buffer-preserve-window-point t ;this allows operating on the same buffer in diff. positions
 initial-buffer-choice my-todo)

;; disable full `yes' or `no' answers, `y' and `n' suffices
(defalias 'yes-or-no-p 'y-or-n-p)

(use-package autorevert
  :diminish auto-revert-mode
  :config
  ;; auto revert buffers when changed on disk
  (global-auto-revert-mode 1))

(provide 'init-config)
