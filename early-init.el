;;;; -*- lexical-binding: t; -*-

;;; ============================================================================
;;; Emacs startup optimizations
;;; ============================================================================

;; Garbage collection slows down startup time, so we maximize the threshold for
;; it to run, and we will later reset it.
(setq gc-cons-threshold most-positive-fixnum)

;; file-name-handler-alist is consulted on various I/O functions such as
;; REQUIRE, slowing down startup time, so we set it to NIL, and establish a hook
;; to restore when Emacs is finished starting.
(unless (or (daemonp) noninteractive)
  (let ((file-name-handler-alist/old file-name-handler-alist))
    (setq file-name-handler-alist nil)
    (add-hook 'emacs-startup-hook
              (lambda ()
                (let ((value (delete-dups
                              (append file-name-handler-alist
                                      file-name-handler-alist/old))))
                  (setq file-name-handler-alist value))))))

(unless (daemonp)
  (advice-add #'tty-run-terminal-initialization :override #'ignore)
  (add-hook 'window-setup-hook
            (lambda ()
              (advice-remove #'tty-run-terminal-initialization #'ignore)
              (tty-run-terminal-initialization (selected-frame) nil t))))

;; Set frame parameters early for faster startup.
(setq frame-inhibit-implied-resize t)
(setq frame-resize-pixelwise t)
(setq truncate-partial-width-windows nil)
(add-to-list 'default-frame-alist '(font . "Iosevka Slab 11"))
(blink-cursor-mode 0)
(column-number-mode 1)
(display-time-mode 0)
(fringe-mode '(4 . 0))
(menu-bar-mode 0)
(scroll-bar-mode 0)
(tool-bar-mode 0)
(tooltip-mode 0)
(window-divider-mode 1)

;;; ============================================================================
;;; Specify some directory paths
;;; ============================================================================

;; For the rest of the Emacs configuration, set this directory to something
;; inside the standard cache directory, so we do not pollute our emacs.d
;; directory with files that we would then have to ignore with Git.
(setq user-emacs-directory
      (expand-file-name "emacs/" (or (getenv "XDG_CACHE_HOME") "~/.cache/")))

;; Add our custom lisp modules to the Emacs load path so they can be discovered.
(push (expand-file-name "lisp/" (file-name-directory user-init-file)) load-path)

;; For the list of native compilation ELN cache directories, delete all but the
;; last element, which is always assumed to be the system path, and then cons a
;; new path in our cache directory to the front. This effectively removes the
;; entry for the original ~/.emacs.d/eln-cache/ and any others that are
;; unwanted.
(setq native-comp-eln-load-path
      (cons (expand-file-name "eln-cache/" user-emacs-directory)
            (last native-comp-eln-load-path)))

;;; ============================================================================
;;; Set up the package manager
;;; ============================================================================

;; Pre-configure the package manager settings before it is loaded.
(setq package-enable-at-startup nil)
(setq package-quickstart nil)
(setq straight-check-for-modifications '(check-on-save find-when-checking))

;; Bootstrap the straight.el package manager if it is not already installed,
;; then unconditionally load it. We use this rather than Emacs' built-in package
;; manager.
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el"
                         user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Additional post-setup of straight.el.
(require 'straight-x)
(defalias 'straight-ಠ_ಠ-mode nil)
