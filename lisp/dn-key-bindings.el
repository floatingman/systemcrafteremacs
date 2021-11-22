;;;; -*- lexical-binding: t; -*-

;;; ============================================================================
;;; Global bindings
;;; ============================================================================

(general-unbind
  [insert]
  [insertchar]
  [M-mouse-1] [M-mouse-2] [M-mouse-3] [M-mouse-4] [M-mouse-5]
  [C-mouse-1] [C-mouse-2] [C-mouse-3] [C-mouse-4] [C-mouse-5]
  [S-mouse-1] [S-mouse-2] [S-mouse-3] [S-mouse-4] [S-mouse-5]
  "C-x C-z")

(define-keys
  [S-insert] #'mf/yank-primary-selection
  [remap move-beginning-of-line] #'mf/smarter-move-beginning-of-line
  [remap evil-beginning-of-line] #'mf/smarter-move-beginning-of-line
  [remap newline] #'newline-and-indent)

(define-keys 'special-mode-map
  "q" #'quit-window)

;;; ============================================================================
;;; Leader key bindings
;;; ============================================================================

(define-leader-keys
  "SPC" '(execute-extended-command :wk t)
  "'" '(vertico-repeat :wk t)
  ";" '(eval-expression :wk t)
  "u" '(universal-argument :wk t))

(define-leader-keys
  :infix "a"
  "" '(:ignore t :wk "app")
  "t" `(,(fn! (term (getenv "SHELL"))) :wk "terminal"))

(define-leader-keys
  :infix "b"
  "" '(:ignore t :wk "buffer")
  "b" '(persp-switch-to-buffer* :wk "switch (current layout)")
  "B" '(persp-switch-to-buffer :wk "switch (all layouts)")
  "d" `(,(fn! (kill-buffer nil)) :wk "delete")
  "r" '(revert-buffer :wk "revert"))

(define-leader-keys
  :infix "c"
  "" '(:ignore t :wk "chat")
  "[" '(tracking-previous-buffer :wk "previous notification")
  "]" '(tracking-next-buffer :wk "next notification"))

(define-leader-keys
  :infix "f"
  "" '(:ignore t :wk "file")
  "c" `(,(fn! (call-interactively 'write-file)) :wk "copy")
  "d" '(dired-jump :wk "directory")
  "D" '(mf/delete-file :wk "delete")
  "f" '(find-file :wk "find")
  "r" '(consult-recent-file :wk "recent")
  "R" '(mf/rename-file :wk "rename")
  "s" '(save-buffer :wk "save")
  "S" '(evil-write-all :wk "save all"))

(define-leader-keys
  :infix "g"
  "" '(:ignore t :wk "git")
  "b" '(magit-blame-addition :wk "blame")
  "g" '(gist-region-or-buffer :wk "gist")
  "G" '(gist-region-or-buffer-private :wk "gist private")
  "n" '(magit-init :wk "initialize repository")
  "s" '(magit-status :wk "status")
  "t" '(git-timemachine-toggle :wk "time machine")
  "w" '(browse-at-remote :wk "browse remote"))

(define-leader-keys
  :infix "h"
  "" '(:ignore t :wk "help")
  "." '(helpful-at-point :wk "point")
  "a" '(consult-apropos :wk "apropos")
  "c" '(describe-char :wk "character")
  "f" '(helpful-callable :wk "function")
  "F" '(describe-face :wk "face")
  "i" '(info-lookup-symbol :wk "info")
  "k" '(helpful-key :wk "key")
  "l" '(find-library :wk "library")
  "m" '(describe-minor-mode :wk "minor mode")
  "M" '(describe-mode :wk "major mode")
  "v" '(helpful-variable :wk "variable"))

(define-leader-keys
  :infix "j"
  "" '(:ignore t :wk "jump")
  "c" '(avy-goto-char :wk "jump to char")
  "l" '(avy-goto-line :wk "jump to line")
  "w" '(avy-goto-word-1 :wk "jump to word"))

(define-leader-keys
  :infix "l"
  "" '(:ignore t :wk "layout")
  "d" '(persp-kill :wk "delete")
  "l" '(persp-switch :wk "switch")
  "L" '(persp-state-load :wk "load")
  "m" '(persp-set-buffer :wk "move buffer")
  "s" '(persp-state-save :wk "save"))

(define-leader-keys
  :infix "n"
  "" '(:ignore t :wk "notes")
  "a" '(archive :wk "archive")
  "A" '(org-agenda :wk "agenda")
  "c" '(org-capture :wk "capture")
  "f" '(org-refile :wk "refile")
  "T" '(org-babel-tangle :wk "tangle"))

(define-leader-keys
  :infix "p"
  "" '(:ignore t :wk "project")
  "a" '(projectile-add-known-project :wk "add project")
  "c" '(projectile-invalidate-cache :wk "clear cache")
  "d" '(projectile-remove-known-project :wk "delete project")
  "f" '(projectile-find-file :wk "find file")
  "k" '(projectile-kill-buffers :wk "kill buffers")
  "p" '(projectile-persp-switch-project :wk "switch project")
  "r" '(projectile-recentf :wk "recent files")
  "s" '(projectile-save-project-buffers :wk "save project files"))

(define-leader-keys
  :infix "q"
  "" '(:ignore t :wk "quit")
  "q" '(evil-quit-all :wk "quit")
  "Q" '(evil-save-and-quit :wk "save/quit"))

(define-leader-keys
  :infix "s"
  "" '(:ignore t :wk "search")
  "b" '(consult-line :wk "search buffer")
  "B" `(,(fn! (consult-line (thing-at-point 'symbol)))
        :wk "search buffer for symbol at point")
  "d" `(,(fn! (consult-ripgrep default-directory)) :wk "search directory")
  "m" '(evil-multiedit-match-all :wk "multi-edit")
  "p" '(consult-ripgrep :wk "search project"))

(define-leader-keys
  :infix "w"
  "" '(:ignore t :wk "window")
  "-" '(evil-window-split :wk "split horizontal")
  "|" '(evil-window-vsplit :wk "split vertical")
  "=" '(balance-windows :wk "balance")
  "d" '(evil-window-delete :wk "delete")
  "D" '(delete-other-windows :wk "delete other")
  "f" '(make-frame :wk "new frame")
  "F" '(delete-frame :wk "delete-frame")
  "s" '(ace-swap-window :wk "swap")
  "u" '(winner-undo :wk "undo")
  "U" '(winner-redo :wk "redo")
  "w" '(ace-window :wk "go to"))

(provide 'dn-key-bindings)
