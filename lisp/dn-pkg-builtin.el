;;;; -*- lexical-binding: t; -*-

(setup auto-fill
  (:with-feature simple
    (:with-mode (text-mode org-mode)
      (:hook turn-on-auto-fill))
    (:with-mode prog-mode
      (:hook (fn (setq-local comment-auto-fill-only-comments t)
                 (auto-fill-mode 1))))
    (:hide-mode auto-fill-function)))

(setup (:require autorevert)
  (setq auto-revert-check-vc-info t)
  (setq auto-revert-remote-files t)
  (setq global-auto-revert-non-file-buffers t)
  (setq auto-revert-verbose nil)
  (global-auto-revert-mode 1)
  (:hide-mode auto-revert))

(setup comint
  (setq ansi-color-for-comint-mode t)
  (setq comint-buffer-maximum-size 4096)
  (setq comint-prompt-read-only t))

(setup compile
  (setq compilation-always-kill t)
  (setq compilation-ask-about-save nil)
  (setq compilation-scroll-output 'first-error))

(setup (:require delsel)
  (delete-selection-mode 1))

(setup dired
  (:also-load dired-x)
  (:pkg dired-collapse
        dired-git-info
        dired-single
        dired-subtree
        diredfl)
  (:with-map dired-mode-map
    (:bind
     [tab] dired-subtree-cycle
     "i" dired-subtree-toggle
     "q" quit-window))
  (:hook dired-collapse-mode)
  (setq dired-recursive-copies 'top)
  (setq dired-recursive-deletes 'top)
  (diredfl-global-mode 1))

(setup (:require eldoc)
  (setq eldoc-echo-area-use-multiline-p nil)
  (setq eldoc-idle-delay 0.1)
  (:with-mode prog-mode
    (:hook turn-on-eldoc-mode))
  (:hide-mode))

(setup (:require elec-pair)
  (electric-pair-mode 1))

(setup executable
  (setq executable-prefix-env t)
  (:with-hook after-save-hook
    (:hook executable-make-buffer-file-executable-if-script-p)))

(setup flyspell
  (:hook-into text-mode-hook)
  (:when-loaded
    (:hide-mode)))

(setup (:require goto-addr)
  (:with-mode prog-mode
    (:hook goto-address-prog-mode))
  (:with-mode text-mode
    (:hook goto-address-mode)))

(setup (:require help-mode)
  (setq help-window-select t))

(setup (:require recentf)
  (setq recentf-auto-cleanup 'mode)
  (setq recentf-exclude `(,#'mf/cache-dir-p
                          "^/tmp/"
                          "COMMIT_EDITMSG$"
                          ".gz$"))
  (setq recentf-filename-handlers '(abbreviate-file-name))
  (setq recentf-max-menu-items 100)
  (setq recentf-max-saved-items nil)
  (recentf-mode 1)
  (run-at-time nil 120 (fn (quiet! (recentf-save-list))))
  (:with-hook kill-emacs-hook
    (:hook recentf-cleanup recentf-save-list)))

(setup (:require savehist)
  (setq history-delete-duplicates t)
  (setq history-length t)
  (setq savehist-additional-variables
        '(extended-command-history
          global-mark-ring
          kill-ring
          mark-ring
          regexp-search-ring
          search-ring))
  (setq savehist-autosave-interval 60)
  (setq savehist-file (mf/etc-file "history"))
  (setq savehist-save-minibuffer-history t)
  (savehist-mode 1))

(setup (:require saveplace)
  (setq save-place-file (mf/etc-file "places"))
  (setq save-place-forget-unreadable-files nil)
  (save-place-mode 1))

(setup (:require server)
  (unless (server-running-p)
    (server-start)))

(setup (:require subword)
  (global-subword-mode 1)
  (:hide-mode))

(setup (:require uniquify)
  (setq uniquify-after-kill-buffer-p t)
  (setq uniquify-buffer-name-style 'forward))

(setup (:require url)
  (setq url-cookie-file (mf/etc-file "url-cookies")))

(setup (:require visual-line-mode)
  (:hide-mode))

(provide 'dn-pkg-builtin)
