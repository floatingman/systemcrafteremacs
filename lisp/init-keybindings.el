(provide 'init-keybindings)

(use-package  hydra :commands defhydra)
(use-package use-package-hydra)
(if my-laptop-p
    (use-package hydra-posframe :if my-laptop-p :quelpa (hydra-posframe :fetcher github :repo "Ladicle/hydra-posframe") :after hydra))

(with-eval-after-load 'hydra
  (defhydra my-window-movement ()
    ("<left>" windmove-left)
    ("<right>" windmove-right)
    ("<down>" windmove-down)
    ("<up>" windmove-up)
    ("y" other-window "other")
    ("h" switch-window "switch-window")
    ("b" consult-buffer "buffer")
    ("f" find-file "file")
    ("F" find-file-other-window "other file")
    ("v" (progn (split-window-right) (windmove-right)))
    ("o" delete-other-windows :color blue)
    ("a" ace-window)
    ("s" ace-swap-window)
    ("d" delete-window "delete")
    ("D" ace-delete-window "ace delete")
    ("i" ace-maximize-window "maximize")
     ("q" nil)))

(with-eval-after-load 'hydra
  (defhydra my-shortcuts (:exit t)
    ("j" my-helm-journal "Journal")
    ("C" my-resolve-orgzly-syncthing "Conflicts")
    ("n" my-capture-timestamped-note "Note")
    ("c" my-org-categorize-emacs-news/body "Categorize")
    ("d" my-emacs-news-check-duplicates "Dupe")
    ("s" save-buffer "Save")
    ("f" my-file-shortcuts/body "File shortcut")
    ("+" text-scale-increase "Increase")
    ("-" text-scale-decrease "Decrease")
    ("g" my-geeqie/body "Geeqie")
    ("r" my-record-ffmpeg-toggle-recording "Record screen")
    ("l" (my-toggle-or-create "*scratch*" (lambda () (switch-to-buffer (startup--get-buffer-create-scratch)))) "Lisp")
    ("e" eshell-toggle "Eshell")
    ("w" my-engine-mode-hydra/body "Search web")
    ("E" my-emacs-news/body "Emacs News"))
  (global-set-key (kbd "<f5>") #'my-shortcuts/body)
  (defhydra my-emacs-news (:exit t)
    "Emacs News"
    ("f" (find-file "~/sync/emacs-news/index.org") "News")
    ("C" (find-file "~/code/emacs-calendar/README.org") "Calendar")
    ("C" (find-file "/ssh:web:/var/www/emacslife.com/calendar/README.org" "Calendar on server"))
    ("d" my-emacs-news-check-duplicates "Dupe")
    ("c" my-org-categorize-emacs-news/body "Categorize")
    ("h" (my-org-update-link-description "HN") "Link HN")
    ("i" (my-org-update-link-description "Irreal") "Link Irreal")
    ("m" my-share-emacs-news "Mail")
    ("t" (browse-url "https://tweetdeck.twitter.com") "Twitter")))

(defun my-org-update-link-description (description)
  "Update the current link's DESCRIPTION."
  (interactive "MDescription: ")
  (let (link)
    (save-excursion
      (cond
       ((org-in-regexp org-link-bracket-re 1)
        (setq link (org-link-unescape (match-string-no-properties 1)))
        (delete-region (match-beginning 0) (match-end 0))
        (insert (org-link-make-string link description))
        (sit-for 0))
       ((or (org-in-regexp org-link-angle-re)
            (org-in-regexp org-link-plain-re))
        (setq link (org-unbracket-string "<" ">" (match-string 0)))
        (delete-region (match-beginning 0) (match-end 0))
        (insert (org-link-make-string link description))
        (sit-for 0))))))

(defun my-org-insert-link ()
  (interactive)
  (when (org-in-regexp org-bracket-link-regexp 1)
    (goto-char (match-end 0))
    (insert "\n"))
  (call-interactively 'org-insert-link))

(defun my-switch-to-previous-buffer ()
  "Switch to previously open buffer.
      Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(defun my-org-check-agenda ()
  "Peek at agenda."
  (interactive)
  (cond
   ((derived-mode-p 'org-agenda-mode)
    (if (window-parent) (delete-window) (bury-buffer)))
   ((get-buffer "*Org Agenda*")
    (switch-to-buffer-other-window "*Org Agenda*"))
   (t (org-agenda nil "a"))))

(defun my-goto-random-char ()
  (interactive)
  (goto-char (random (point-max))))

(defvar hydra-stack nil)

(defun my-hydra-push (expr)
  (push `(lambda () ,expr) hydra-stack))

(defun my-hydra-pop ()
  (interactive)
  (let ((x (pop hydra-stack)))
    (when x (funcall x))))

(defun my-hydra-go-and-push (expr)
  (push hydra-curr-body-fn hydra-stack)
  (prin1 hydra-stack)
  (funcall expr))

;; example (progn (hydra-b/body) (hydra-push '(hydra-a/body)))
;; or   ("q" hydra-pop "exit")

(defun my-hydra-format-head (h)
  (let ((key-binding (elt h 0))
        (hint (elt h 2))
        (cmd (and (elt h 1) (prin1-to-string (elt h 1)))))
    (if cmd
        (format "%s (%s) - %s" hint key-binding cmd)
      (format "%s (%s)" hint key-binding))))

(defun my-hydra-heads-to-candidates (base)
  (mapcar (lambda (h)
            (cons (my-hydra-format-head h) (hydra--head-name h base)))
          (symbol-value (intern (concat (symbol-name base) "/heads")))))

(defun my-hydra-execute-extended (&optional prefixarg hydra-base)
  (interactive (list current-prefix-arg nil))
  (hydra-keyboard-quit)
  (let* ((candidates (my-hydra-heads-to-candidates
                      (or hydra-base
                          (intern
                           (replace-regexp-in-string "/body$" ""
                                                     (symbol-name hydra-curr-body-fn))))))
         (command-name (completing-read "Cmd: " candidates))
         (bind (assoc-default command-name candidates 'string=)))
    (cond
     ((null bind) nil)
     ((hydra--callablep bind) (call-interactively bind)))))

(with-eval-after-load 'hydra
  (define-key hydra-base-map (kbd "<tab>") #'my-hydra-execute-extended))

(defun my-key-chord-define (keymap keys command)
  "Define in KEYMAP, a key-chord of two keys in KEYS starting a COMMAND.
      \nKEYS can be a string or a vector of two elements. Currently only elements
      that corresponds to ascii codes in the range 32 to 126 can be used.
      \nCOMMAND can be an interactive function, a string, or nil.
      If COMMAND is nil, the key-chord is removed.

      MODIFICATION: Do not define the transposed key chord.
      "
  (if (/= 2 (length keys))
      (error "Key-chord keys must have two elements"))
  ;; Exotic chars in a string are >255 but define-key wants 128..255 for those
  (let ((key1 (logand 255 (aref keys 0)))
        (key2 (logand 255 (aref keys 1))))
    (define-key keymap (vector 'key-chord key1 key2) command)))
(fset 'key-chord-define 'my-key-chord-define)

(use-package key-chord
  :if my-laptop-p
  :hydra (my-key-chord-commands
          ()
          "Main"
          ("k" kill-sexp)
          ("h" my-org-jump :color blue)
          ("x" my-org-finish-previous-task-and-clock-in-new-one "Finish and clock in" :color blue)
          ("b" helm-buffers-list :color blue)
          ("f" find-file :color blue)
          ("a" my-org-check-agenda :color blue)
          ("c" (call-interactively 'org-capture) "capture" :color blue)
          ("t" (org-capture nil "T") "Capture task")
          ("." repeat)
          ("C-t" transpose-chars)
          ("o" my-org-off-my-computer :color blue)
          ("w" my-engine-mode-hydra/body "web" :exit t)
          ("m" imenu :color blue)
          ("i" my-capture-timestamped-note-with-screenshot :exit t)
          ("n" my-capture-timestamped-note "Timestamped note" :exit t)
          ("q" quantified-track :color blue)
          ("r" my-describe-random-interactive-function)
          ("l" org-insert-last-stored-link)
          ("L" my-org-insert-link))
  :init
  (setq key-chord-one-key-delay 0.16)
  (setq key-chord-two-keys-delay 0.002)
  (key-chord-define-global "uu" 'undo)
  (key-chord-define-global "jr" 'my-goto-random-char-hydra/my-goto-random-char)
  (key-chord-define-global "kk" 'kill-whole-line)
  (key-chord-define-global "et" 'my-stream-message)
  (key-chord-define-global "em" 'embark-act)
  (key-chord-define-global ".t" 'my-stream/body)
  (key-chord-define-global "jj" 'avy-goto-word-1)
  (key-chord-define-global "yy" 'my-window-movement/body)
  (key-chord-define-global "jw" 'switch-window)
  (key-chord-define-global "jl" 'avy-goto-line)
  (key-chord-define-global "j." 'join-lines/body)
  (key-chord-define-global "FF" 'find-file)
  (key-chord-define-global "qq" 'my-quantified-hydra/body)
  (key-chord-define-global "hh" 'my-key-chord-commands/body)
  (key-chord-define-global "xx" 'er/expand-region)
  (key-chord-define-global "  " 'my-insert-space-or-expand)
  (key-chord-define-global "vv" 'god-mode-all)
  (key-chord-define-global "JJ" 'my-switch-to-previous-buffer)
  (key-chord-mode 1)) ;; disable for now

(bind-key "C-t" 'my-key-chord-commands/body)

(use-package which-key
  :diminish
  :custom
  (which-key-show-docstrings 'docstring-only)
  (which-key-max-discription-length nil)
  (which-key-side-window-max-height 0.75)
  :config
  (which-key-mode))
