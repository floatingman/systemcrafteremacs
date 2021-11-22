;;;; -*- lexical-binding: t; -*-

(defvar mf/dir-notes
  (file-name-as-directory (expand-file-name "~/Documents/Notes")))

(setup (:pkg evil-org)
  (:load-after (evil org)
    (:hook-into org-mode org-agenda-mode)
    (:require evil-org-agenda)
    (evil-org-set-key-theme
     '(navigation insert textobjects additional calendar))
    (evil-org-agenda-set-keys)
    (:hide-mode)))

(setup (:pkg org)
  (setq org-capture-bookmark nil)
  (setq org-capture-templates
        '(("c" "Code Task" entry
           (file+headline org-default-notes-file "Coding Tasks")
           "* TODO %?\n  Entered on: %U - %a\n")
          ("t" "Task" entry (file+headline org-default-notes-file "Tasks")
           "* TODO %?\n  Entered on: %U")
          ("n" "Note" entry (file+olp+datetree org-default-notes-file)
           "* %?\n\n")))
  (setq org-catch-invisible-edits 'show-and-error)
  (setq org-cycle-separator-lines 2)
  (setq org-directory
        (file-name-as-directory (expand-file-name "Org" mf/dir-notes)))
  (setq org-default-notes-file (expand-file-name "notes.org" org-directory))
  (setq org-edit-src-content-indentation 2)
  (setq org-ellipsis " ▾")
  (setq org-export-coding-system 'utf-8-unix)
  (setq org-export-headline-levels 8)
  (setq org-export-with-section-numbers nil)
  (setq org-export-with-smart-quotes t)
  (setq org-export-with-sub-superscripts t)
  (setq org-export-with-toc t)
  (setq org-fontify-quote-and-verse-blocks t)
  (setq org-fontify-whole-heading-line t)
  (setq org-hide-block-startup nil)
  (setq org-hide-emphasis-markers t)
  (setq org-html-coding-system 'utf-8-unix)
  (setq org-html-todo-kwd-class-prefix "keyword ")
  (setq org-id-locations-file (expand-file-name ".orgids" mf/dir-notes))
  (setq org-outline-path-complete-in-steps nil)
  (setq org-pretty-entities t)
  (setq org-refile-targets '((nil :maxlevel . 3)
                             (org-agenda-files :maxlevel . 3)))
  (setq org-refile-use-outline-path t)
  (setq org-return-follows-link t)
  (setq org-src-fontify-natively t)
  (setq org-src-preserve-indentation nil)
  (setq org-src-tab-acts-natively t)
  (setq org-startup-folded 'content)
  (setq org-startup-indented t)
  (setq org-startup-with-inline-images t)
  (setq org-todo-keywords
        '((sequence "TODO(t)" "INPROGRESS(i)" "|" "DONE(d)")
          (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)")
          (sequence "REPORTED(r@/!)" "BUG(b@/!)" "|" "FIXED(f@/!)")))
  (setq org-todo-keyword-faces
        '(("TODO" :foreground "dodger blue" :weight bold)
          ("INPROGRESS" :foreground "spring green" :weight bold)
          ("WAITING" :foreground "yellow" :weight bold)
          ("HOLD" :foreground "yellow" :weight bold)
          ("DONE" :foreground "forest green" :weight bold)
          ("CANCELLED" :foreground "forest green" :weight bold)
          ("REPORTED" :foreground "red" :weight bold)
          ("BUG" :foreground "red" :weight bold)
          ("FIXED" :foreground "forest green" :weight bold)))
  (:load-after hl-fill-column
    (:hook (fn (auto-fill-mode 0)
               (hl-fill-column-mode 0)
               (visual-line-mode 1)))))

(setup (:pkg org-appear)
  (:load-after org
    (setq org-appear-autolinks t)
    (setq org-appear-autoemphasis t)
    (setq org-appear-autoentities t)
    (setq org-appear-autokeywords t)
    (:hook-into org-mode)))

(setup org-faces
  (:load-after org-indent
    (dolist (face-cons '((org-document-title . 1.75)
                         (org-level-1 . 1.5)
                         (org-level-2 . 1.25)
                         (org-level-3 . 1.12)
                         (org-level-4 . 1.05)
                         (org-level-5 . 1.0)
                         (org-level-6 . 1.0)
                         (org-level-7 . 1.0)
                         (org-level-8 . 1.0)))
      (cl-destructuring-bind (face . height) face-cons
        (set-face-attribute face
                            nil
                            :weight 'bold
                            :font "Iosevka Aile"
                            :height height)))))

(setup org-indent
  (:load-after (org evil)
    (setq evil-auto-indent nil)
    (org-indent-mode 1)
    (:hide-mode)))

(setup (:pkg (org-roam :files (:defaults "extensions/*")))
  (setq org-roam-v2-ack t)
  (:load-after org
    (setq org-roam-completion-everywhere t)
    (setq org-roam-directory
          (file-name-as-directory (expand-file-name "Roam" mf/dir-notes)))
    (org-roam-db-autosync-mode 1)))

(setup (:pkg org-superstar)
  (:load-after org
    (setq org-superstar-headline-bullets-list '("◉" "○" "●" "○" "●" "○" "●"))
    (setq org-superstar-remove-leading-stars t)
    (:hook-into org-mode)))

(setup org-tempo
  (:load-after org
    (add-to-list 'org-structure-template-alist '("cl" . "src common-lisp"))
    (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
    (add-to-list 'org-structure-template-alist '("li" . "src lisp"))
    (add-to-list 'org-structure-template-alist '("sh" . "src sh"))))

(setup (:pkg toc-org)
  (:load-after org
    (:hook-into org-mode)))

(define-local-keys org-mode-map
  "i" '(org-id-get-create :wk "add id")
  "t" '(org-set-tags-command :wk "add tags"))

(define-local-keys org-mode-map
  :infix "r"
  "" '(:ignore t :wk "roam")
  "a" '(org-roam-alias-add :wk "add alias")
  "A" '(org-roam-alias-remove :wk "remove alias")
  "b" '(org-roam-buffer-toggle :wk "toggle buffer")
  "B" '(org-roam-buffer-display-dedicated :wk "show dedicated buffer")
  "f" '(org-roam-node-find :wk "find node")
  "i" '(org-roam-node-insert :wk "add node")
  "r" '(org-roam-ref-add :wk "add reference")
  "R" '(org-roam-ref-add :wk "remove reference"))

(provide 'dn-pkg-lang-org)

;;;; -*- lexical-binding: t; -*-

(setup (:pkg evil-cleverparens)
  (setq evil-cleverparens-swap-move-by-word-and-symbol t)
  (setq evil-cleverparens-use-additional-bindings nil)
  (setq evil-cleverparens-use-additional-movement-keys nil)
  (setq evil-cleverparens-use-regular-insert t)
  (:load-after (evil smartparens evil-smartparens)
    (:hook-into emacs-lisp-mode
                eval-expression-minibuffer-setup
                ielm-mode
                lisp-interaction-mode
                lisp-mode
                sly-mrepl-mode)
    (:hide-mode)))

(setup (:pkg evil-smartparens)
  (:load-after (evil smartparens)
    (:hook-into smartparens-enabled-hook)
    (:hide-mode)))

(setup (:pkg rainbow-delimiters)
  (setq rainbow-delimiters-max-face-count 2)
  (:hook-into emacs-lisp-mode-hook
              eval-expression-minibuffer-setup-hook
              ielm-mode-hook
              lisp-interaction-mode-hook
              lisp-mode-hook
              sly-mrepl-mode-hook)
  (:hide-mode))

(setup (:pkg smartparens)
  (:require smartparens)
  (setq sp-cancel-autoskip-on-backward-movement nil)
  (setq sp-highlight-pair-overlay nil)
  (setq sp-highlight-wrap-overlay nil)
  (setq sp-highlight-wrap-tag-overlay nil)
  (setq sp-max-pair-length 2)
  (setq sp-max-prefix-length 32)
  (setq sp-message-width nil)
  (setq sp-navigate-consider-sgml-tags nil)
  (setq sp-navigate-skip-match nil)
  (setq sp-show-pair-from-inside t)
  (sp-pair "'" nil :actions :rem)
  (sp-pair "`" nil :actions :rem)
  (sp-pair "(" nil :unless '(:rem sp-point-before-word-p))
  (:with-hook (emacs-lisp-mode-hook
               eval-expression-minibuffer-setup-hook
               ielm-mode-hook
               lisp-interaction-mode-hook
               lisp-mode-hook
               sly-mrepl-mode-hook)
    (:hook smartparens-strict-mode))
  (:hide-mode))

(define-local-keys (emacs-lisp-mode-map sly-mrepl-mode-map lisp-mode-map)
  :infix "l"
  "" '(:ignore t :wk "lisp")
  "a" '(sp-absorb-sexp :wk "absorb")
  "b" '(sp-forward-barf-sexp :wk "barf forward")
  "B" '(sp-backward-barf-sexp :wk "barf backward")
  "c" '(sp-convolute-sexp :wk "convolute")
  "e" '(sp-splice-sexp-killing-backward :wk "splice killing backward")
  "E" '(sp-splice-sexp-killing-forward :wk "splice killing forward")
  "j" '(sp-join-sexp :wk "join")
  "r" '(sp-raise-sexp :wk "raise")
  "s" '(sp-forward-slurp-sexp :wk "slurp forward")
  "S" '(sp-backward-slurp-sexp :wk "slurp backward")
  "t" '(sp-transpose-sexp :wk "transpose")
  "w" '(sp-wrap-round :wk "wrap")
  "W" '(sp-unwrap-sexp :wk "unwrap"))

(define-local-keys (lisp-mode-map emacs-lisp-mode-map)
  "m" '(macrostep-expand :wk "macro expand"))

(provide 'dn-pkg-lang-lisp)
