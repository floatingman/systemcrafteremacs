(defun dw/minibuffer-backward-kill (arg)
  "When minibuffer is completing a file name delete up to parent
    folder, otherwise delete a word"
  (interactive "p")
  (if minibuffer-completing-file-name
      ;; Borrowed from https://github.com/raxod502/selectrum/issues/498#issuecomment-803283608
      (if (string-match-p "/." (minibuffer-contents))
          (zap-up-to-char (- arg) ?/)
        (delete-minibuffer-contents))
    (delete-word (- arg))))

(use-package vertico
  :config
  (vertico-mode)
  (vertico-cycle t)
  (custom-set-faces '(vertico-current ((t (:background "#3a3f5a")))))
  :bind ((:map vertico-map
              ("C-j" . vertico-next)
              ("C-k" . vertico-previous)
              ("C-f" . vertico-exit))
              (:map minibuffer-local-map
                    ("M-h" . dw/minibuffer-backward-kill))))







(provide 'init-completion)
