(setq debug-on-error t)
(setq debug-on-quit t)

(let ((minver "25.1"))
  (when (version< emacs-version minver)
    (error "Your Emacs is too old -- this config requires v%s or higher" minver)))
(when (version< emacs-version "26.1")
  (message "Your Emacs is old, and some functionality in this config will be disabled. Please upgrade if possible."))

;; This sets up the load path so that we can override it
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path "~/Repos/org-mode/lisp")
(add-to-list 'load-path "~/Repos/org-contrib/lisp")

(let ((normal-gc-cons-threshold (* 20 1024 1024))
      (init-gc-cons-threshold (* 128 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'emacs-startup-hook
            (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))

(setq user-full-name "Daniel Newman"
      user-mail-address "dan@danlovesprogramming.com")

(require 'init-system)

(require 'init-utils)

(require 'init-packages)

;; Use a hook so the message doesn't get clobbered by other messages.
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

;; Change the user-emacs-directory to keep unwanted things out of ~/.emacs.d
(setq user-emacs-directory (expand-file-name "~/.cache/emacs/")
      url-history-file (expand-file-name "url/history" user-emacs-directory))

;; Use no-littering to automatically set common paths to the new user-emacs-directory
(use-package no-littering)

;; Save auto-save files to the no-littering var folder
(setq auto-save-file-name-transforms
      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)

(require 'init-config)
(require 'init-keybindings)
(require 'init-themes)
(require 'init-editing)
(require 'init-languages)
(require 'init-completion)
(require 'init-projects)
(require 'init-shell)
(require 'init-vc)
(require 'init-dired)
(require 'init-org)
(require 'init-misc-packages)

(setq debug-on-error nil)
(setq debug-on-quit nil)
