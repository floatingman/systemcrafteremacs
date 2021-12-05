(unless (featurep 'straight)
  ;; Bootstrap straight.el
  (defvar bootstrap-version)
  (let ((bootstrap-file
         (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
        (bootstrap-version 5))
    (unless (file-exists-p bootstrap-file)
      (with-current-buffer
          (url-retrieve-synchronously
           "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
           'silent 'inhibit-cookies)
        (goto-char (point-max))
        (eval-print-last-sexp)))
    (load bootstrap-file nil 'nomessage)))

;; Use straight.el for use-package expressions
(straight-use-package 'use-package)

(straight-use-package '(setup :type git :host nil :repo "https://git.sr.ht/~pkal/setup"))
(require 'setup)

(setup-define :straight
  (lambda (recipe)
    `(unless (straight-use-package ',recipe)
       ,(setup-quit)))
  :documentation
  "Install RECIPE with `straight-use-package'.
This macro can be used as HEAD, and will replace itself with the
first RECIPE's package."
  :repeatable t
  :shorthand (lambda (sexp)
               (let ((recipe (cadr sexp)))
                 (if (consp recipe)
                     (car recipe)
                   recipe))))

(setup-define :straight-when
  (lambda (recipe condition)
    `(if ,condition
         (straight-use-package ',recipe)
       ,(setup-quit)))
  :documentation
  "Install RECIPE with `straight-use-package' when CONDITION is met.
If CONDITION is false, stop evaluating the body.  This macro can
be used as HEAD, and will replace itself with the RECIPE's
package.  This macro is not repeatable."
  :repeatable nil
  :indent 1
  :shorthand (lambda (sexp)
               (let ((recipe (cadr sexp)))
                 (if (consp recipe) (car recipe) recipe))))

(provide 'init-packages)
