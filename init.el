(setq gc-cons-threshold (* 100 1000 1000))
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 2 1000 1000))))

;;; init.el --- Emacs initialization file tangled from a README.org file
;;
;;  Author: Daniel Newman <dan@danlovesprogramming.com>
;;  URL: https://github.com/floatingman/systemcrafteremacs
;;  ============================================================================

;;; User setting
;;  ----------------------------------------------------------------------------

(setq user-full-name "Daniel Newman"
      user-mail-address "dan@danlovesprogramming.com")

(load-file "~/.emacs.d/lisp/init-packages.el")

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

;; Keep customization settings in a temporary file (thanks Ambrevar!)
(setq custom-file
      (if (boundp 'server-socket-dir)
          (expand-file-name "custom.el" server-socket-dir)
        (expand-file-name (format "emacs-custom-%s.el" (user-uid)) temporary-file-directory)))
(load custom-file t)

;; This sets up the load path so that we can override it
(push (expand-file-name "lisp/" (file-name-directory user-init-file)) load-path)

(set-default-coding-systems 'utf-8)

(require 'init-system)
(require 'init-config)
(require 'init-themes)
(require 'init-editing)
(require 'init-languages)
(require 'init-completion)
(require 'init-projects)
(require 'init-shell)
(require 'init-vc)
(require 'init-org)
(require 'init-keybindings)
