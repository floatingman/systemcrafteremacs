;;;; -*- lexical-binding: t; -*-

(defvar mf/cl-implementations
  '((sbcl ("sbcl.sh"))
    (sbcl-renderdoc ("sbcl-renderdoc.sh"))))

(defvar mf/clhs-dir
  (file-name-as-directory (expand-file-name "~/.data/common-lisp/clhs")))

(defun mf/sly-ask ()
  (interactive)
  (let ((current-prefix-arg '-))
    (sly nil nil t)))

(setup (:pkg sly)
  (:when-loaded
    (sly-setup '(sly-fancy)))
  (setq common-lisp-hyperspec-root mf/clhs-dir)
  (setq sly-command-switch-to-existing-lisp 'always)
  (setq sly-complete-symbol-function 'sly-flex-completions)
  (setq sly-enable-evaluate-in-emacs t)
  (setq sly-kill-without-query-p t)
  (setq sly-lisp-implementations mf/cl-implementations)
  (setq sly-mrepl-history-file-name (mf/etc-file "sly-repl-history"))
  (setq sly-mrepl-pop-sylvester nil)
  (setq sly-mrepl-prevent-duplicate-history 'move)
  (setq sly-net-coding-system 'utf-8-unix)
  (:with-map sly-mrepl-mode-map
    (:with-state insert
      (:bind
       [S-return] newline-and-indent
       [up] sly-mrepl-previous-input-or-button
       [down] sly-mrepl-next-input-or-button)))
  (:with-map sly-inspector-mode-map
    (:with-state normal
      (:bind
       [return] push-button
       [M-return] sly-mrepl-copy-part-to-repl
       "gb" sly-inspector-pop
       "h" sly-inspector-history
       "i" sly-inspector-describe-inspectee
       "p" sly-button-pretty-print)))
  (:hide-mode))

(setup (:pkg sly-macrostep)
  (:load-after sly))

(setup (:pkg sly-repl-ansi-color)
  (:load-after sly
    (push 'sly-repl-ansi-color sly-contribs)))

(setup lisp-mode
  (:load-after sly
    (define-local-keys lisp-mode-map
      "'" '(sly :wk "sly")
      ";" `(mf/sly-ask :wk "sly (ask)"))
    (define-local-keys lisp-mode-map
      :infix "c"
      "" '(:ignore t :wk "compile")
      "c" '(sly-compile-file :wk "compile file")
      "C" '(sly-compile-and-load-file :wk "compile/load file")
      "f" '(sly-compile-defun :wk "compile top-level form")
      "l" '(sly-load-file :wk "load file")
      "n" '(sly-remove-notes :wk "remove notes")
      "r" '(sly-compile-region :wk "compile region"))
    (define-local-keys lisp-mode-map
      :infix "e"
      "" '(:ignore t :wk "evaluate")
      "b" '(sly-eval-buffer :wk "buffer")
      "e" '(sly-eval-last-expression :wk "last expression")
      "f" '(sly-eval-defun :wk "function")
      "F" '(sly-undefine-function :wk "undefine function")
      "r" '(sly-eval-region :wk "region"))
    (define-local-keys lisp-mode-map
      :infix "g"
      "" '(:ignore t :wk "go")
      "b" '(sly-pop-find-definition-stack :wk "back")
      "d" '(sly-edit-definition :wk "definition")
      "D" '(sly-edit-definition-other-window :wk "definition (other window)")
      "n" '(sly-next-note :wk "next note")
      "N" '(sly-previous-note :wk "previous note")
      "s" '(sly-stickers-next-sticker :wk "next sticker")
      "S" '(sly-stickers-prev-sticker :wk "previous sticker"))
    (define-local-keys lisp-mode-map
      :infix "h"
      "" '(:ignore t :wk "help")
      "<" '(sly-who-calls :wk "who calls")
      ">" '(sly-calls-who :wk "calls who")
      "~" '(hyperspec-lookup-format :wk "lookup format directive")
      "#" '(hyperspec-lookup-reader-macro :wk "lookup reader macro")
      "a" '(sly-apropos :wk "apropos")
      "b" '(sly-who-binds :wk "who binds")
      "d" '(sly-disassemble-symbol :wk "disassemble symbol")
      "h" '(sly-describe-symbol :wk "describe symbol")
      "H" '(sly-hyperspec-lookup :wk "hyperspec lookup")
      "m" '(sly-who-macroexpands :wk "who macro-expands")
      "p" '(sly-apropos-package :wk "apropos package")
      "r" '(sly-who-references :wk "who references")
      "s" '(sly-who-specializes :wk "who specializes")
      "S" '(sly-who-sets :wk "who sets"))
    (define-local-keys lisp-mode-map
      :infix "r"
      "" '(:ignore t :wk "repl")
      "c" '(sly-mrepl-clear-repl :wk "clear")
      "q" '(sly-quit-lisp :wk "quit")
      "r" '(sly-restart-inferior-lisp :wk "restart")
      "s" '(sly-mrepl-sync :wk "sync"))
    (define-local-keys lisp-mode-map
      :infix "s"
      "" '(:ignore t :wk "stickers")
      "b" '(sly-stickers-toggle-break-on-stickers :wk "toggle break")
      "c" '(sly-stickers-clear-defun-stickers :wk "clear function")
      "C" '(sly-stickers-clear-buffer-stickers :wk "clear buffer")
      "f" '(sly-stickers-fetch :wk "fetch")
      "r" '(sly-stickers-replay :wk "replay")
      "s" '(sly-stickers-dwim :wk "add/remove"))
    (define-local-keys lisp-mode-map
      :infix "t"
      "" '(:ignore t :wk "trace")
      "t" '(sly-toggle-trace-fdefinition :wk "toggle")
      "T" '(sly-toggle-fancy-trace :wk "toggle (fancy)")
      "u" '(sly-untrace-all :wk "untrace all"))))

(provide 'dn-pkg-lang-common-lisp)
