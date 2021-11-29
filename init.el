;;; init.el --- Emacs initialization file tangled from a README.org file
;;
;;  Author: Daniel Newman <dan@danlovesprogramming.com>
;;  URL: https://github.com/floatingman/systemcrafteremacs
;;  ============================================================================

;;; User setting
;;  ----------------------------------------------------------------------------

(setq user-full-name "Daniel Newman"
      user-mail-address "dan@danlovesprogramming.com")

(setq user-emacs-directory
      (expand-file-name "emacs/" (or (getenv "XDG_CACHE_HOME") "~/.cache/")))

(push (expand-file-name "lisp/" (file-name-directory user-init-file)) load-path)

(require 'init-org)
