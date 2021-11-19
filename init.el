(load-file "~/.emacs.d/lisp/dw-settings.el")

;; Load settings for the first time
(dw/load-system-settings)

(defconst *spell-check-support-enabled* t) ;; Enable with t if you prefer
(defconst *is-a-mac* (eq system-type 'darwin))
(defconst *is-windows* (eq system-type 'windows-nt))
(defconst *is-linux* (eq system-type 'gnu/linux))
(defconst *is-gui* (not (eq window-system nil)))

;; The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

(defun efs/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                     (time-subtract after-init-time before-init-time)))
           gcs-done))

(add-hook 'emacs-startup-hook #'efs/display-startup-time)

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
  (load bootstrap-file nil 'nomessage))

;; Always use straight to install on systems other than Linux
(setq straight-use-package-by-default t)

;; Use straight.el for use-package expressions
(straight-use-package 'use-package)

;; Load the helper package for commands like `straight-x-clean-unused-repos'
(require 'straight-x)

;; Change the user-emacs-directory to keep unwanted things out of ~/.emacs.d
(setq user-emacs-directory (expand-file-name "~/.cache/emacs/")
        url-history-file (expand-file-name "url/history" user-emacs-directory))

(use-package no-littering)

;; Keep customization settings in a temporary file (thanks Ambrevar!)
(setq custom-file
      (if (boundp 'server-socket-dir)
          (expand-file-name "custom.el" server-socket-dir)
        (expand-file-name (format "emacs-custom-%s.el" (user-uid)) temporary-file-directory)))
(load custom-file t)

;; Add my library path to load-path
(push "~/.emacs.d/lisp" load-path)

(set-default-coding-systems 'utf-8)

;; only start emacs server when it's not started, I hate warnings.
(setq server-socket-file "/tmp/emacs1000/server")
(unless (file-exists-p server-socket-file)
  (server-start))

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(global-set-key (kbd "C-M-u") 'universal-argument)

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))

(use-package general
  :config
  (general-evil-setup t)

  (general-create-definer dn/leader-key-def
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")

  (general-create-definer dn/ctrl-c-keys
    :prefix "C-c"))

(defun dw/evil-hook ()
  (dolist (mode '(custom-mode
                  eshell-mode
                  git-rebase-mode
                  erc-mode
                  circe-server-mode
                  circe-chat-mode
                  circe-query-mode
                  sauron-mode
                  term-mode))
    (add-to-list 'evil-emacs-state-modes mode)))
(use-package undo-tree
  :init
  (global-undo-tree-mode 1))

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  (setq evil-respect-visual-line-mode t)
  (setq evil-undo-system 'undo-tree)
  :config
  (add-hook 'evil-mode-hook 'dw/evil-hook)
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line))

(use-package evil-collection
  :after evil
  :init
  (setq evil-collection-company-use-tng nil)  ;; Is this a bug in evil-collection?
  :custom
  (evil-collection-outline-bind-tab-p nil)
  :config
  (setq evil-collection-mode-list
        (remove 'lispy evil-collection-mode-list))
  (evil-collection-init))

(setq inhibit-startup-message t)

(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room

(menu-bar-mode -1)            ; Disable the menu bar

;; Set up the visible bell
(setq visible-bell t)

(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time
(setq use-dialog-box nil) ;; Disable dialog boxes since they weren't working in Mac OSX

;; Set frame transparency
(set-frame-parameter (selected-frame) 'alpha '(90 . 90))
(add-to-list 'default-frame-alist '(alpha . (90 . 90)))
;; (set-frame-parameter (selected-frame) 'fullscreen 'maximized)
;; (add-to-list 'default-frame-alist '(fullscreen . maximized))

(column-number-mode)

;; Enable line numbers for some modes
(dolist (mode '(text-mode-hook
                prog-mode-hook
                conf-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 1))))

;; Override some modes which derive from the above
(dolist (mode '(org-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(setq large-file-warning-threshold nil)

(setq vc-follow-symlinks t)

(setq ad-redefinition-action 'accept)

(use-package base16-theme
  :config
  (load-theme 'base16-tomorrow t))

;; Set the font face based on platform
(pcase system-type
  ((or 'gnu/linux 'windows-nt 'cygwin)
   (set-face-attribute 'default nil
                       :font "JetBrains Mono"
                       :weight 'light
                       :height (dw/system-settings-get 'emacs/default-face-size)))
  ('darwin (set-face-attribute 'default nil :font "Fira Mono" :height 170)))

;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil
                    :font "JetBrains Mono"
                    :weight 'light
                    :height (dw/system-settings-get 'emacs/fixed-face-size))

;; Set the variable pitch face
(set-face-attribute 'variable-pitch nil
                    ;; :font "Cantarell"
                    :font "Iosevka"
                    :height (dw/system-settings-get 'emacs/variable-face-size)
                    :weight 'light)

(defun dw/replace-unicode-font-mapping (block-name old-font new-font)
  (let* ((block-idx (cl-position-if
                         (lambda (i) (string-equal (car i) block-name))
                         unicode-fonts-block-font-mapping))
         (block-fonts (cadr (nth block-idx unicode-fonts-block-font-mapping)))
         (updated-block (cl-substitute new-font old-font block-fonts :test 'string-equal)))
    (setf (cdr (nth block-idx unicode-fonts-block-font-mapping))
          `(,updated-block))))

(use-package unicode-fonts
  :disabled
  :custom
  (unicode-fonts-skip-font-groups '(low-quality-glyphs))
  :config
  ;; Fix the font mappings to use the right emoji font
  (mapcar
    (lambda (block-name)
      (dw/replace-unicode-font-mapping block-name "Apple Color Emoji" "Noto Color Emoji"))
    '("Dingbats"
      "Emoticons"
      "Miscellaneous Symbols and Pictographs"
      "Transport and Map Symbols"))
  (unicode-fonts-setup))

(use-package emojify
  :hook (erc-mode . emojify-mode)
  :commands emojify-mode)

(setq display-time-format "%l:%M %p %b %y"
      display-time-default-load-average nil)

(use-package diminish)

(use-package smart-mode-line
  :disabled
  :config
  (setq sml/no-confirm-load-theme t)
  (sml/setup)
  (sml/apply-theme 'respectful)  ; Respect the theme colors
  (setq sml/mode-width 'right
      sml/name-width 60)

  (setq-default mode-line-format
  `("%e"
      ,(when dw/exwm-enabled
          '(:eval (format "[%d] " exwm-workspace-current-index)))
      mode-line-front-space
      evil-mode-line-tag
      mode-line-mule-info
      mode-line-client
      mode-line-modified
      mode-line-remote
      mode-line-frame-identification
      mode-line-buffer-identification
      sml/pos-id-separator
      (vc-mode vc-mode)
      " "
      ;mode-line-position
      sml/pre-modes-separator
      mode-line-modes
      " "
      mode-line-misc-info))

  (setq rm-excluded-modes
    (mapconcat
      'identity
      ; These names must start with a space!
      '(" GitGutter" " MRev" " company"
      " Helm" " Undo-Tree" " Projectile.*" " Z" " Ind"
      " Org-Agenda.*" " ElDoc" " SP/s" " cider.*")
      "\\|")))

;; You must run (all-the-icons-install-fonts) one time after
;; installing this package!

(use-package minions
  :hook (doom-modeline-mode . minions-mode))

(use-package doom-modeline
  :after eshell     ;; Make sure it gets hooked after eshell
  :hook (after-init . doom-modeline-init)
  :custom-face
  (mode-line ((t (:height 0.85))))
  (mode-line-inactive ((t (:height 0.85))))
  :custom
  (doom-modeline-height 15)
  (doom-modeline-bar-width 6)
  (doom-modeline-lsp t)
  (doom-modeline-github nil)
  (doom-modeline-mu4e nil)
  (doom-modeline-irc nil)
  (doom-modeline-minor-modes t)
  (doom-modeline-persp-name nil)
  (doom-modeline-buffer-file-name-style 'truncate-except-project)
  (doom-modeline-major-mode-icon nil))

(use-package perspective
  :demand t
  :bind (("C-M-k" . persp-switch)
         ("C-M-n" . persp-next)
         ("C-x k" . persp-kill-buffer*))
  :custom
  (persp-initial-frame-name "Main")
  :config
  ;; Running `persp-mode' multiple times resets the perspective list...
  (unless (equal persp-mode t)
    (persp-mode)))

(use-package alert
  :commands alert
  :config
  (setq alert-default-style 'notifications))

(use-package super-save
  :defer 1
  :diminish super-save-mode
  :config
  (super-save-mode +1)
  (setq super-save-auto-save-when-idle t))

;; Revert Dired and other buffers
(setq global-auto-revert-non-file-buffers t)

;; Revert buffers when the underlying file has changed
(global-auto-revert-mode 1)

(dn/leader-key-def
  "t"  '(:ignore t :which-key "toggles")
  "tw" 'whitespace-mode
  "tt" '(counsel-load-theme :which-key "choose theme"))

(use-package paren
  :config
  (set-face-attribute 'show-paren-match-expression nil :background "#363e4a")
  (show-paren-mode 1))

(setq display-time-world-list
  '(("Etc/UTC" "UTC")
    ("America/Los_Angeles" "Seattle")
    ("America/New_York" "New York")
    ("America/Buenos_Aries" "Buenos Aries")
    ("Europe/Athens" "Athens")
    ("Pacific/Auckland" "Auckland")
    ("Asia/Shanghai" "Shanghai")))
(setq display-time-world-time-format "%a, %d %b %I:%M %p %Z")

;; Set default connection mode to SSH
(setq tramp-default-method "ssh")

(defun dw/show-server-edit-buffer (buffer)
  ;; TODO: Set a transient keymap to close with 'C-c C-c'
  (split-window-vertically -15)
  (other-window 1)
  (set-buffer buffer))

(setq server-window #'dw/show-server-edit-buffer)

(setq-default tab-width 2)
(setq-default evil-shift-width tab-width)

(setq-default indent-tabs-mode nil)

(use-package evil-nerd-commenter
  :bind ("M-/" . evilnc-comment-or-uncomment-lines))

(use-package ws-butler
  :hook ((text-mode . ws-butler-mode)
         (prog-mode . ws-butler-mode)))

(use-package origami
  :hook (yaml-mode . origami-mode))

(defun dw/org-file-jump-to-heading (org-file heading-title)
  (interactive)
  (find-file (expand-file-name org-file))
  (goto-char (point-min))
  (search-forward (concat "* " heading-title))
  (org-overview)
  (org-reveal)
  (org-show-subtree)
  (forward-line))

(defun dw/org-file-show-headings (org-file)
  (interactive)
  (find-file (expand-file-name org-file))
  (counsel-org-goto)
  (org-overview)
  (org-reveal)
  (org-show-subtree)
  (forward-line))

(dn/leader-key-def
  "fn" '((lambda () (interactive) (counsel-find-file "~/Notes/")) :which-key "notes")
  "fd"  '(:ignore t :which-key "dotfiles")
  "fde" '((lambda () (interactive) (find-file (expand-file-name "~/.dotfiles/emacs.d/Emacs.org"))) :which-key "edit config")
  "fdE" '((lambda () (interactive) (dw/org-file-show-headings "~/.dotfiles/emacs.d/Emacs.org")) :which-key "edit config")
  "fds" '((lambda () (interactive) (dw/org-file-jump-to-heading "~/.dotfiles/emacs.d/Systems.org" "Base Configuration")) :which-key "base system")
  "fdS" '((lambda () (interactive) (dw/org-file-jump-to-heading "~/.dotfiles/emacs.d/Systems.org" system-name)) :which-key "this system")
  "fdw" '((lambda () (interactive) (find-file (expand-file-name "~/.dotfiles/emacs.d/Workflow.org"))) :which-key "workflow"))

(use-package hydra
  :defer t)

(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
	 :map ivy-minibuffer-map
	 ("TAB" . ivy-alt-done)
	 ("C-f" . ivy-alt-done)
	 ("C-l" . ivy-alt-done)
	 ("C-j" . ivy-next-line)
	 ("C-k" . ivy-previous-line)
	 :map ivy-switch-buffer-map
	 ("C-k" . ivy-previous-line)
	 ("C-l" . ivy-done)
	 ("C-d" . ivy-switch-buffer-kill)
	 :map ivy-reverse-i-search-map
	 ("C-k" . ivy-previous-line)
	 ("C-d" . ivy-reverse-i-search-kill))
  :init
  (ivy-mode 1)
  :config
  (setq ivy-use-virtual-buffers t)
  (setq ivy-wrap t)
  (setq ivy-count-format "(%d/%d) ")
  (setq enbale-recursive-minibuffers t)

  ;;  Use different regex strategies per completion command
  (push '(completion-at-point . ivy--regex-fuzzy) ivy-re-builders-alist) ;; This doesn't seem to work
  (push '(swiper . ivy--regex-ignore-order) ivy-re-builders-alist)
  (push '(counsel-M-x . ivy--regex-ignore-order) ivy-re-builders-alist)

  ;; Set minibuffer height for different commands
  (setf (alist-get 'counsel-projectile-ag ivy-height-alist) 15)
  (setf (alist-get 'counsel-projectile-rg ivy-height-alist) 15)
  (setf (alist-get 'swiper ivy-height-alist) 15)
  (setf (alist-get 'counsel-switch-buffer ivy-height-alist) 7))

(use-package ivy-hydra
  :defer t
  :after hydra)

(use-package ivy-rich
  :init
  (ivy-rich-mode 1)
  :after counsel
  :config
  (setq ivy-format-function #'ivy-format-function-line)
(setq ivy-rich-display-transformers-list
	  (plist-put ivy-rich-display-transformers-list
		     'ivy-switch-buffer
		     '(:columns
		       ((ivy-rich-candidate (:width 40))
			(ivy-rich-switch-buffer-indicators (:width 4 :face error :align right)); return the buffer indicators
			(ivy-rich-switch-buffer-major-mode (:width 12 :face warning))          ; return the major mode info
			(ivy-rich-switch-buffer-project (:width 15 :face success))             ; return project name using `projectile'
			(ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))  ; return file path relative to project root or `default-directory' if project is nil
		       :predicate
		       (lambda (cand)
			 (if-let ((buffer (get-buffer cand)))
			     ;; Don't mess with EXWM buffers
			     (with-current-buffer buffer
			       (not (derived-mode-p 'exwm-mode)))))))))
(use-package counsel
  :demand t
  :bind (("M-x" . counsel-M-x)
	 ("C-x b" . counsel-ibuffer)
	 ("C-x C-f" . counsel-find-file)
	 ("C-M-l" . counsel-imenu)
	 :map minibuffer-local-map
	 ("C-r" . 'counsel-minibuffer-history))
  :custom
  (counsel-linux-app-format-function #'counsel-linux-app-format-function-name-only)
  :config
  (setq ivy-initial-inputs-alist nil)) ;; Don't start seraches with ^

(use-package flx ;; Improves sorting for fuzzy-matched results
  :after ivy
  :defer t
  :init
  (setq ivy-flx-limit 10000))

(use-package wgrep)

(use-package prescient
  :after counsel
  :config
  (prescient-persist-mode 1))

(use-package ivy-prescient
  :after prescient
  :config
  (ivy-prescient-mode 1))

(dn/leader-key-def
 "r" '(ivy-resume :which-key "ivy resume")
 "f" '(:ignore t :which-key "files")
 "ff" '(counsel-find-file :which-key "open file")
 "C-f" 'counsel-find-file
 "fr" '(counsel-recentf :which-key "recent files")
 "fR" '(revert-buffer :which-key "revert file")
 "fj" '(counsel-file-jump :which-key "jump to file"))

(use-package savehist
  :config
  (setq history-length 25)
  (savehist-mode 1))

  ;; Individual history elements can be configured separately
  ;;(put 'minibuffer-history 'history-length 25)
  ;;(put 'evil-ex-history 'history-length 50)
  ;;(put 'kill-ring 'history-length 25))

(defun dw/minibuffer-backward-kill (arg)
  "When minibuffer is completing a file name delete up to parent
folder, otherwise delete a word"
  (interactive "p")
  (if minibuffer-completing-file-name
      ;; Borrowed from https://github.com/raxod502/selectrum/issues/498#issuecomment-803283608
      (if (string-match-p "/." (minibuffer-contents))
          (zap-up-to-char (- arg) ?/)
        (delete-minibuffer-contents))
      (backward-kill-word arg)))

(use-package vertico
  :straight '(vertico :host github
                      :repo "minad/vertico"
                      :branch "main")
  :bind (:map vertico-map
         ("C-j" . vertico-next)
         ("C-k" . vertico-previous)
         ("C-f" . vertico-exit)
         :map minibuffer-local-map
         ("M-h" . dw/minibuffer-backward-kill))
  :custom
  (vertico-cycle t)
  :custom-face
  (vertico-current ((t (:background "#3a3f5a"))))
  :init
  (vertico-mode))

(use-package corfu
  :straight '(corfu :host github
                    :repo "minad/corfu")
  :bind (:map corfu-map
         ("C-j" . corfu-next)
         ("C-k" . corfu-previous)
         ("C-f" . corfu-insert))
  :custom
  (corfu-cycle t)
  :config
  (corfu-global-mode))

(use-package orderless
  :straight t
  :init
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles . (partial-completion))))))

(defun dw/get-project-root ()
  (when (fboundp 'projectile-project-root)
    (projectile-project-root)))

(use-package consult
  :straight t
  :demand t
  :bind (("C-s" . consult-line)
         ("C-M-l" . consult-imenu)
         ("C-M-j" . persp-switch-to-buffer*)
         :map minibuffer-local-map
         ("C-r" . consult-history))
  :custom
  (consult-project-root-function #'dw/get-project-root)
  (completion-in-region-function #'consult-completion-in-region))

(use-package marginalia
  :after vertico
  :straight t
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :init
  (marginalia-mode))

(use-package embark
  :straight t
  :bind (("C-S-a" . embark-act)
         :map minibuffer-local-map
         ("C-d" . embark-act))
  :config

  ;; Show Embark actions via which-key
  (setq embark-action-indicator
        (lambda (map)
          (which-key--show-keymap "Embark" map nil nil 'no-paging)
          #'which-key--hide-popup-ignore-command)
        embark-become-indicator embark-action-indicator))

;; (use-package embark-consult
;;   :straight '(embark-consult :host github
;;                              :repo "oantolin/embark"
;;                              :files ("embark-consult.el"))
;;   :after (embark consult)
;;   :demand t
;;   :hook
;;   (embark-collect-mode . embark-consult-preview-minor-mode))

(use-package avy
  :commands (avy-goto-char avy-goto-word-0 avy-goto-line))

(dn/leader-key-def
  "j"  '(:ignore t :which-key "jump")
  "jj" '(avy-goto-char :which-key "jump to char")
  "jw" '(avy-goto-word-0 :which-key "jump to word")
  "jl" '(avy-goto-line :which-key "jump to line"))

(use-package default-text-scale
  :defer 1
  :config
  (default-text-scale-mode))

(use-package ace-window
  :bind (("M-o" . ace-window))
  :custom
  (aw-scope 'frame)
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (aw-minibuffer-flag t)
  :config
  (ace-window-display-mode 1))

(use-package winner
  :after evil
  :config
  (winner-mode)
  (define-key evil-window-map "u" 'winner-undo)
  (define-key evil-window-map "U" 'winner-redo))

(defun dn/org-mode-visual-fill ()
  (setq visual-fill-column-width 110
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :defer t
  :hook (org-mode . dn/org-mode-visual-fill))

(setq display-buffer-base-action
        '(display-buffer-reuse-mode-window
          display-buffer-reuse-window
          display-buffer-same-window))

(use-package expand-region
  :bind (("M-[" . er/expand-region)
         ("C-(" . er/mark-outside-pairs)))

(use-package all-the-icons-dired)

(use-package dired
  :ensure nil
  :straight nil
  :defer 1
  :commands (dired dired-jump)
  :config
  (setq dired-listing-switches "-agho --group-directories-first"
        dired-omit-verbose nil
        dired-hide-details-hide-symlink-targets nil
        delete-by-moving-to-trash t)

  (autoload 'dired-omit-mode "dired-x")

  (add-hook 'dired-load-hook
            (lambda ()
              (interactive)
              (dired-collapse)))

  (add-hook 'dired-mode-hook
            (lambda ()
              (interactive)
              (dired-omit-mode 1)
              (dired-hide-details-mode 1)
              (all-the-icons-dired-mode 1)
              (hl-line-mode 1)))
  (use-package dired-rainbow
      :defer 2
      :config
      (dired-rainbow-define-chmod directory "#6cb2eb" "d.*")
      (dired-rainbow-define html "#eb5286" ("css" "less" "sass" "scss" "htm" "html" "jhtm" "mht" "eml" "mustache" "xhtml"))
      (dired-rainbow-define xml "#f2d024" ("xml" "xsd" "xsl" "xslt" "wsdl" "bib" "json" "msg" "pgn" "rss" "yaml" "yml" "rdata"))
      (dired-rainbow-define document "#9561e2" ("docm" "doc" "docx" "odb" "odt" "pdb" "pdf" "ps" "rtf" "djvu" "epub" "odp" "ppt" "pptx"))
      (dired-rainbow-define markdown "#ffed4a" ("org" "etx" "info" "markdown" "md" "mkd" "nfo" "pod" "rst" "tex" "textfile" "txt"))
      (dired-rainbow-define database "#6574cd" ("xlsx" "xls" "csv" "accdb" "db" "mdb" "sqlite" "nc"))
      (dired-rainbow-define media "#de751f" ("mp3" "mp4" "mkv" "MP3" "MP4" "avi" "mpeg" "mpg" "flv" "ogg" "mov" "mid" "midi" "wav" "aiff" "flac"))
      (dired-rainbow-define image "#f66d9b" ("tiff" "tif" "cdr" "gif" "ico" "jpeg" "jpg" "png" "psd" "eps" "svg"))
      (dired-rainbow-define log "#c17d11" ("log"))
      (dired-rainbow-define shell "#f6993f" ("awk" "bash" "bat" "sed" "sh" "zsh" "vim"))
      (dired-rainbow-define interpreted "#38c172" ("py" "ipynb" "rb" "pl" "t" "msql" "mysql" "pgsql" "sql" "r" "clj" "cljs" "scala" "js"))
      (dired-rainbow-define compiled "#4dc0b5" ("asm" "cl" "lisp" "el" "c" "h" "c++" "h++" "hpp" "hxx" "m" "cc" "cs" "cp" "cpp" "go" "f" "for" "ftn" "f90" "f95" "f03" "f08" "s" "rs" "hi" "hs" "pyc" ".java"))
      (dired-rainbow-define executable "#8cc4ff" ("exe" "msi"))
      (dired-rainbow-define compressed "#51d88a" ("7z" "zip" "bz2" "tgz" "txz" "gz" "xz" "z" "Z" "jar" "war" "ear" "rar" "sar" "xpi" "apk" "xz" "tar"))
      (dired-rainbow-define packaged "#faad63" ("deb" "rpm" "apk" "jad" "jar" "cab" "pak" "pk3" "vdf" "vpk" "bsp"))
      (dired-rainbow-define encrypted "#ffed4a" ("gpg" "pgp" "asc" "bfe" "enc" "signature" "sig" "p12" "pem"))
      (dired-rainbow-define fonts "#6cb2eb" ("afm" "fon" "fnt" "pfb" "pfm" "ttf" "otf"))
      (dired-rainbow-define partition "#e3342f" ("dmg" "iso" "bin" "nrg" "qcow" "toast" "vcd" "vmdk" "bak"))
      (dired-rainbow-define vc "#0074d9" ("git" "gitignore" "gitattributes" "gitmodules"))
      (dired-rainbow-define-chmod executable-unix "#38c172" "-.*x.*"))

    (use-package dired-single
      :defer t)

    (use-package dired-ranger
      :defer t)

    (use-package dired-collapse
      :defer t)

    (evil-collection-define-key 'normal 'dired-mode-map
      "h" 'dired-single-up-directory
      "H" 'dired-omit-mode
      "l" 'dired-single-buffer
      "y" 'dired-ranger-copy
      "X" 'dired-ranger-move
      "p" 'dired-ranger-paste))

(when (eq system-type 'darwin)
  (setq ns-use-native-fullscreen nil)
  ;; brew install coreutils
  (if (executable-find "gls")
      (progn
        (setq insert-directory-program "gls")
        (setq dired-listing-switches "-lFaGh1v --group-directories-first"))
    (setq dired-listing-switches "-ahlF"))
  (defun copy-from-osx ()
    "Handle copy/paste intelligently on osx."
    (let ((pbpaste (purecopy "/usr/bin/pbpaste")))
      (if (and (eq system-type 'darwin)
               (file-exists-p pbpaste))
          (let ((tramp-mode nil)
                (default-directory "~"))
            (shell-command-to-string pbpaste)))))

  (defun paste-to-osx (text &optional push)
    (let ((process-connection-type nil))
      (let ((proc (start-process "pbcopy" "*Messages*" "/usr/bin/pbcopy")))
        (process-send-string proc text)
        (process-send-eof proc))))
  (setq interprogram-cut-function 'paste-to-osx
        interprogram-paste-function 'copy-from-osx)

  (defun move-file-to-trash (file)
    "Use `trash' to move FILE to the system trash.
When using Homebrew, install it using \"brew install trash\"."
    (call-process (executable-find "trash")
                  nil 0 nil
                  file))

  ;; Trackpad scrolling
  (global-set-key [wheel-up] 'previous-line)
  (global-set-key [wheel-down] 'next-line))

(use-package openwith
    :config
    (setq openwith-associations
          (list
            (list (openwith-make-extension-regexp
                  '("mpg" "mpeg" "mp3" "mp4"
                    "avi" "wmv" "wav" "mov" "flv"
                    "ogm" "ogg" "mkv"))
                  "mpv"
                  '(file))
            (list (openwith-make-extension-regexp
                  '("xbm" "pbm" "pgm" "ppm" "pnm"
                    "png" "gif" "bmp" "tif" "jpeg")) ;; Removed jpg because Telega was
                    ;; causing feh to be opened...
                    "feh"
                    '(file))
            (list (openwith-make-extension-regexp
                  '("pdf"))
                  "zathura"
                  '(file)))))

(setq-default fill-column 80)

;; Turn on indentation and auto-fill mode for Org files
(defun dn/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (auto-fill-mode 0)
  (visual-line-mode 1)
  (setq evil-auto-indent nil)
  (diminish org-indent-mode))

(use-package org
  :defer t
  :hook (org-mode . dn/org-mode-setup)
  :config
  (setq org-ellipsis " ▾"
      org-hide-emphasis-markers t
      org-src-fontify-natively t
      org-fontify-quote-and-verse-blocks t
      org-src-tab-acts-natively t
      org-edit-src-content-indentation 2
      org-hide-block-startup nil
      org-src-preserve-indentation nil
      org-startup-folded 'content
      org-cycle-separator-lines 2)

  (setq org-modules
    '(org-crypt
        org-habit
        org-bookmark
        org-eshell
        org-irc))

  (setq org-refile-targets '((nil :maxlevel . 1)
                             (org-agenda-files :maxlevel . 1)))

  (setq org-outline-path-complete-in-steps nil)
  (setq org-refile-use-outline-path t)

  (evil-define-key '(normal insert visual) org-mode-map (kbd "C-j") 'org-next-visible-heading)
  (evil-define-key '(normal insert visual) org-mode-map (kbd "C-k") 'org-previous-visible-heading)

  (evil-define-key '(normal insert visual) org-mode-map (kbd "M-j") 'org-metadown)
  (evil-define-key '(normal insert visual) org-mode-map (kbd "M-k") 'org-metaup)

  (org-babel-do-load-languages
    'org-babel-load-languages
    '((emacs-lisp . t)
      (ledger . t)))

  (push '("conf-unix" . conf-unix) org-src-lang-modes)

  ;; NOTE: Subsequent sections are still part of this use-package block!

(require 'dw-org)
(require 'dn-workflow)

(use-package org-superstar
  :after org
  :hook (org-mode . org-superstar-mode)
  :custom
  (org-superstar-remove-leading-stars t)
  (org-superstar-headline-bullets-list '("◉" "○" "●" "○" "●" "○" "●")))

;; Replace list hyphen with dot
;; (font-lock-add-keywords 'org-mode
;;                         '(("^ *\\([-]\\) "
;;                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

;; Increase the size of various headings
(set-face-attribute 'org-document-title nil :font "Iosevka Aile" :weight 'bold :height 1.3)
(dolist (face '((org-level-1 . 1.2)
                (org-level-2 . 1.1)
                (org-level-3 . 1.05)
                (org-level-4 . 1.0)
                (org-level-5 . 1.1)
                (org-level-6 . 1.1)
                (org-level-7 . 1.1)
                (org-level-8 . 1.1)))
  (set-face-attribute (car face) nil :font "Iosevka Aile" :weight 'medium :height (cdr face)))

;; Make sure org-indent face is available
(require 'org-indent)

;; Ensure that anything that should be fixed-pitch in Org files appears that way
(set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
(set-face-attribute 'org-table nil  :inherit 'fixed-pitch)
(set-face-attribute 'org-formula nil  :inherit 'fixed-pitch)
(set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-indent nil :inherit '(org-hide fixed-pitch))
(set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
(set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
(set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)

;; Get rid of the background on column views
(set-face-attribute 'org-column nil :background nil)
(set-face-attribute 'org-column-title nil :background nil)

;; TODO: Others to consider
;; '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
;; '(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
;; '(org-property-value ((t (:inherit fixed-pitch))) t)
;; '(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
;; '(org-table ((t (:inherit fixed-pitch :foreground "#83a598"))))
;; '(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8))))
;; '(org-verbatim ((t (:inherit (shadow fixed-pitch))))))

;; This is needed as of Org 9.2
(require 'org-tempo)

(add-to-list 'org-structure-template-alist '("sh" . "src sh"))
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("sc" . "src scheme"))
(add-to-list 'org-structure-template-alist '("ts" . "src typescript"))
(add-to-list 'org-structure-template-alist '("py" . "src python"))
(add-to-list 'org-structure-template-alist '("go" . "src go"))
(add-to-list 'org-structure-template-alist '("yaml" . "src yaml"))
(add-to-list 'org-structure-template-alist '("json" . "src json"))

(use-package org-pomodoro
  :after org
  :config
  (setq org-pomodoro-start-sound "~/.emacs.d/sounds/focus_bell.wav")
  (setq org-pomodoro-short-break-sound "~/.emacs.d/sounds/three_beeps.wav")
  (setq org-pomodoro-long-break-sound "~/.emacs.d/sounds/three_beeps.wav")
  (setq org-pomodoro-finished-sound "~/.emacs.d/sounds/meditation_bell.wav")

  (dn/leader-key-def
    "op"  '(org-pomodoro :which-key "pomodoro")))

(require 'org-protocol)

(defun dw/search-org-files ()
  (interactive)
  (counsel-rg "" "~/Notes" nil "Search Notes: "))

(use-package evil-org
  :after org
  :hook ((org-mode . evil-org-mode)
         (org-agenda-mode . evil-org-mode)
         (evil-org-mode . (lambda () (evil-org-set-key-theme '(navigation todo insert textobjects additional)))))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(dn/leader-key-def
  "o"   '(:ignore t :which-key "org mode")

  "oi"  '(:ignore t :which-key "insert")
  "oil" '(org-insert-link :which-key "insert link")

  "on"  '(org-toggle-narrow-to-subtree :which-key "toggle narrow")

  "os"  '(dw/counsel-rg-org-files :which-key "search notes")

  "oa"  '(org-agenda :which-key "status")
  "ot"  '(org-todo-list :which-key "todos")
  "oc"  '(org-capture t :which-key "capture")
  "ox"  '(org-export-dispatch t :which-key "export"))

;; This ends the use-package org-mode block
)

(use-package org-make-toc
  :hook (org-mode . org-make-toc-mode))

(defun dw/org-present-prepare-slide ()
  (org-overview)
  (org-show-entry)
  (org-show-children))

(defun dw/org-present-hook ()
  (setq-local face-remapping-alist '((default (:height 1.5) variable-pitch)
                                     (header-line (:height 4.5) variable-pitch)
                                     (org-code (:height 1.55) org-code)
                                     (org-verbatim (:height 1.55) org-verbatim)
                                     (org-block (:height 1.25) org-block)
                                     (org-block-begin-line (:height 0.7) org-block)))
  (setq header-line-format " ")
  (org-display-inline-images)
  (dw/org-present-prepare-slide))

(defun dw/org-present-quit-hook ()
  (setq-local face-remapping-alist '((default variable-pitch default)))
  (setq header-line-format nil)
  (org-present-small)
  (org-remove-inline-images))

(defun dw/org-present-prev ()
  (interactive)
  (org-present-prev)
  (dw/org-present-prepare-slide))

(defun dw/org-present-next ()
  (interactive)
  (org-present-next)
  (dw/org-present-prepare-slide))

(use-package org-present
  :bind (:map org-present-mode-keymap
         ("C-c C-j" . dw/org-present-next)
         ("C-c C-k" . dw/org-present-prev))
  :hook ((org-present-mode . dw/org-present-hook)
         (org-present-mode-quit . dw/org-present-quit-hook)))

(defun dw/org-start-presentation ()
  (interactive)
  (org-tree-slide-mode 1)
  (setq text-scale-mode-amount 3)
  (text-scale-mode 1))

(defun dw/org-end-presentation ()
  (interactive)
  (text-scale-mode 0)
  (org-tree-slide-mode 0))

(use-package org-tree-slide
  :defer t
  :after org
  :commands org-tree-slide-mode
  :config
  (evil-define-key 'normal org-tree-slide-mode-map
    (kbd "q") 'dw/org-end-presentation
    (kbd "C-j") 'org-tree-slide-move-next-tree
    (kbd "C-k") 'org-tree-slide-move-previous-tree)
  (setq org-tree-slide-slide-in-effect nil
        org-tree-slide-activate-message "Presentation started."
        org-tree-slide-deactivate-message "Presentation ended."
        org-tree-slide-header t))

(use-package org-roam
  :straight t
  :hook
  (after-init . org-roam-mode)
  :config
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory "~/Notes/Roam/")
  (org-roam-completion-everywhere t)
  (org-roam-completion-system 'default)
  (org-roam-capture-templates
   '(("d" "default" plain
      #'org-roam-capture--get-point
      "%?"
      :file-name "%<%Y%m%d%H%M%S>-${slug}"
      :head "#+title: ${title}\n"
      :unnarrowed t)
     ("ll" "link note" plain
      #'org-roam-capture--get-point
      "* %^{Link}"
      :file-name "Inbox"
      :olp ("Links")
      :unnarrowed t
      :immediate-finish)
     ("lt" "link task" entry
      #'org-roam-capture--get-point
      "* TODO %^{Link}"
      :file-name "Inbox"
      :olp ("Tasks")
      :unnarrowed t
      :immediate-finish)))
  (org-roam-dailies-directory "Journal/")
  (org-roam-dailies-capture-templates
   '(("d" "default" entry
      #'org-roam-capture--get-point
      "* %?"
      :file-name "Journal/%<%Y-%m-%d>"
      :head "#+title: %<%Y-%m-%d %a>\n\n[[roam:%<%Y-%B>]]\n\n")
     ("t" "Task" entry
      #'org-roam-capture--get-point
      "* TODO %?\n  %U\n  %a\n  %i"
      :file-name "Journal/%<%Y-%m-%d>"
      :olp ("Tasks")
      :empty-lines 1
      :head "#+title: %<%Y-%m-%d %a>\n\n[[roam:%<%Y-%B>]]\n\n")
     ("j" "journal" entry
      #'org-roam-capture--get-point
      "* %<%I:%M %p> - Journal  :journal:\n\n%?\n\n"
      :file-name "Journal/%<%Y-%m-%d>"
      :olp ("Log")
      :head "#+title: %<%Y-%m-%d %a>\n\n[[roam:%<%Y-%B>]]\n\n")
     ("l" "log entry" entry
      #'org-roam-capture--get-point
      "* %<%I:%M %p> - %?"
      :file-name "Journal/%<%Y-%m-%d>"
      :olp ("Log")
      :head "#+title: %<%Y-%m-%d %a>\n\n[[roam:%<%Y-%B>]]\n\n")
     ("m" "meeting" entry
      #'org-roam-capture--get-point
      "* %<%I:%M %p> - %^{Meeting Title}  :meetings:\n\n%?\n\n"
      :file-name "Journal/%<%Y-%m-%d>"
      :olp ("Log")
      :head "#+title: %<%Y-%m-%d %a>\n\n[[roam:%<%Y-%B>]]\n\n")))
  :bind (:map org-roam-mode-map
              (("C-c n l"   . org-roam)
               ("C-c n f"   . org-roam-find-file)
               ("C-c n d"   . org-roam-dailies-find-date)
               ("C-c n c"   . org-roam-dailies-capture-today)
               ("C-c n C r" . org-roam-dailies-capture-tomorrow)
               ("C-c n t"   . org-roam-dailies-find-today)
               ("C-c n y"   . org-roam-dailies-find-yesterday)
               ("C-c n r"   . org-roam-dailies-find-tomorrow)
               ("C-c n g"   . org-roam-graph))
              :map org-mode-map
              (("C-c n i" . org-roam-insert))
              (("C-c n I" . org-roam-insert-immediate))))

(use-package deft
  :commands (deft)
  :config (setq deft-directory "~/Notes/Roam"
                deft-recursive t
                deft-extensions '("md" "org")))

(use-package org-appear
  :hook (org-mode . org-appear-mode))

(use-package magit
    :bind ("C-M-;" . magit-status)
    :commands (magit-status magit-get-current-branch)
    :custom
    (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

  (dn/leader-key-def
    "g"   '(:ignore t :which-key "git")
    "gs"  'magit-status
    "gd"  'magit-diff-unstaged
    "gc"  'magit-branch-or-checkout
    "gl"   '(:ignore t :which-key "log")
    "glc" 'magit-log-current
    "glf" 'magit-log-buffer-file
    "gb"  'magit-branch
    "gP"  'magit-push-current
    "gp"  'magit-pull-branch
    "gf"  'magit-fetch
    "gF"  'magit-fetch-all
    "gr"  'magit-rebase)

(use-package magit-todos
  :defer t)

(use-package git-link
  :commands git-link
  :config
  (setq git-link-open-in-browser t)
  (dn/leader-key-def
    "gL" 'git-link))

(use-package git-gutter
  :straight git-gutter-fringe
  :diminish
  :hook ((text-mode . git-gutter-mode)
         (prog-mode . git-gutter-mode))
  :config
  (setq git-gutter:update-interval 2)
  (require 'git-gutter-fringe)
  (set-face-foreground 'git-gutter-fr:added "LightGreen")
  (fringe-helper-define 'git-gutter-fr:added nil
      "XXXXXXXXXX"
      "XXXXXXXXXX"
      "XXXXXXXXXX"
      ".........."
      ".........."
      "XXXXXXXXXX"
      "XXXXXXXXXX"
      "XXXXXXXXXX"
      ".........."
      ".........."
      "XXXXXXXXXX"
      "XXXXXXXXXX"
      "XXXXXXXXXX")

    (set-face-foreground 'git-gutter-fr:modified "LightGoldenrod")
    (fringe-helper-define 'git-gutter-fr:modified nil
      "XXXXXXXXXX"
      "XXXXXXXXXX"
      "XXXXXXXXXX"
      ".........."
      ".........."
      "XXXXXXXXXX"
      "XXXXXXXXXX"
      "XXXXXXXXXX"
      ".........."
      ".........."
      "XXXXXXXXXX"
      "XXXXXXXXXX"
      "XXXXXXXXXX")

    (set-face-foreground 'git-gutter-fr:deleted "LightCoral")
    (fringe-helper-define 'git-gutter-fr:deleted nil
      "XXXXXXXXXX"
      "XXXXXXXXXX"
      "XXXXXXXXXX"
      ".........."
      ".........."
      "XXXXXXXXXX"
      "XXXXXXXXXX"
      "XXXXXXXXXX"
      ".........."
      ".........."
      "XXXXXXXXXX"
      "XXXXXXXXXX"
      "XXXXXXXXXX")

  ;; These characters are used in terminal mode
  (setq git-gutter:modified-sign "≡")
  (setq git-gutter:added-sign "≡")
  (setq git-gutter:deleted-sign "≡")
  (set-face-foreground 'git-gutter:added "LightGreen")
  (set-face-foreground 'git-gutter:modified "LightGoldenrod")
  (set-face-foreground 'git-gutter:deleted "LightCoral"))

(defun dw/switch-project-action ()
  "Switch to a workspace with the project name and start `magit-status'."
  (persp-switch (projectile-project-name))
  (magit-status))

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :demand t
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/Repos")
    (setq projectile-project-search-path '("~/Repos")))
  (setq projectile-switch-project-action #'dw/switch-project-action))

(use-package counsel-projectile
  :after projectile
  :bind (("C-M-p" . counsel-projectile-find-file))
  :config
  (counsel-projectile-mode))

(dn/leader-key-def
  "pf" 'counsel-projectile-find-file
  "ps" 'counsel-projectile-switch-project
  "pF" 'counsel-projectile-rg
  "pp" 'counsel-projectile
  "pc" 'projectile-compile-project
  "pd" 'projectile-dired)

(use-package lsp-mode
  :straight t
  :commands lsp
  :hook ((typescript-mode js2-mode web-mode) . lsp)
  :bind (:map lsp-mode-map
         ("TAB" . completion-at-point))
  :custom (lsp-headerline-breadcrumb-enable nil))

(dn/leader-key-def
  "l"  '(:ignore t :which-key "lsp")
  "ld" 'xref-find-definitions
  "lr" 'xref-find-references
  "ln" 'lsp-ui-find-next-reference
  "lp" 'lsp-ui-find-prev-reference
  "ls" 'counsel-imenu
  "le" 'lsp-ui-flycheck-list
  "lS" 'lsp-ui-sideline-mode
  "lX" 'lsp-execute-code-action)

(use-package lsp-ui
  :straight t
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq lsp-ui-sideline-enable t)
  (setq lsp-ui-sideline-show-hover nil)
  (setq lsp-ui-doc-position 'bottom)
  (lsp-ui-doc-show))

(use-package dap-mode
  :straight t
  :custom
  (lsp-enable-dap-auto-configure nil)
  :config
  (dap-ui-mode 1)
  (dap-tooltip-mode 1)
  (require 'dap-node)
  (dap-node-setup))

(use-package lispy
  :hook ((emacs-lisp-mode . lispy-mode)
         (scheme-mode . lispy-mode)))

(use-package lispyville
  :hook ((lispy-mode . lispyville-mode))
  :config
  (lispyville-set-key-theme '(operators c-w additional
                                        additional-movement slurp/barf-cp
                                        prettify)))

(use-package scheme-mode
  :straight nil
  :mode "\\.sld\\'")

(use-package nvm
  :defer t)

(use-package typescript-mode
  :mode "\\.ts\\'"
  :config
  (setq typescript-indent-level 2))

(defun dw/set-js-indentation ()
  (setq js-indent-level 2)
  (setq evil-shift-width js-indent-level)
  (setq-default tab-width 2))

(use-package js2-mode
  :mode "\\.jsx?\\'"
  :config
  ;; Use js2-mode for Node scripts
  (add-to-list 'magic-mode-alist '("#!/usr/bin/env node" . js2-mode))

  ;; Don't use built-in syntax checking
  (setq js2-mode-show-strict-warnings nil)

  ;; Set up proper indentation in JavaScript and JSON files
  (add-hook 'js2-mode-hook #'dw/set-js-indentation)
  (add-hook 'json-mode-hook #'dw/set-js-indentation))


(use-package apheleia
  :config
  (apheleia-global-mode +1))

(use-package prettier-js
  ;; :hook ((js2-mode . prettier-js-mode)
  ;;        (typescript-mode . prettier-js-mode))
  :config
  (setq prettier-js-show-errors nil))

(add-hook 'emacs-lisp-mode-hook #'flycheck-mode)

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . helpful-function)
  ([remap describe-symbol] . helpful-symbol)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-command] . helpful-command)
  ([remap describe-key] . helpful-key))

(dn/leader-key-def
  "e"   '(:ignore t :which-key "eval")
  "eb"  '(eval-buffer :which-key "eval buffer"))

(dn/leader-key-def
  :keymaps '(visual)
  "er" '(eval-region :which-key "eval region"))

(use-package markdown-mode
  :straight t
  :mode "\\.md\\'"
  :config
  (setq markdown-command "marked")
  (defun dw/set-markdown-header-font-sizes ()
    (dolist (face '((markdown-header-face-1 . 1.2)
                    (markdown-header-face-2 . 1.1)
                    (markdown-header-face-3 . 1.0)
                    (markdown-header-face-4 . 1.0)
                    (markdown-header-face-5 . 1.0)))
      (set-face-attribute (car face) nil :weight 'normal :height (cdr face))))

  (defun dw/markdown-mode-hook ()
    (dw/set-markdown-header-font-sizes))

  (add-hook 'markdown-mode-hook 'dw/markdown-mode-hook))

(use-package web-mode
  :mode "(\\.\\(html?\\|ejs\\|tsx\\|jsx\\)\\'"
  :config
  (setq-default web-mode-code-indent-offset 2)
  (setq-default web-mode-markup-indent-offset 2)
  (setq-default web-mode-attribute-indent-offset 2))

;; 1. Start the server with `httpd-start'
;; 2. Use `impatient-mode' on any buffer
(use-package impatient-mode
  :straight t)

(use-package skewer-mode
  :straight t)

(use-package yaml-mode
  :mode "\\.ya?ml\\'")

(use-package python-mode
  :straight t
  :custom
  (python-shell-interpreter "python3")
  (dap-python-executable "python3")
  (dap-python-debugger 'debugpy)
  :config
  (require 'dap-python))

(use-package pyvenv
  :straight t
  :config
  (pyvenv-mode 1))

(use-package compile
  :straight nil
  :custom
  (compilation-scroll-output t))

(defun auto-recompile-buffer ()
  (interactive)
  (if (member #'recompile after-save-hook)
      (remove-hook 'after-save-hook #'recompile t)
    (add-hook 'after-save-hook #'recompile nil t)))

(use-package flycheck
  :defer t
  :hook (lsp-mode . flycheck-mode))

(use-package yasnippet
  :hook (prog-mode . yas-minor-mode)
  :config
  (yas-reload-all))

(use-package smartparens
  :hook (prog-mode . smartparens-mode))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package rainbow-mode
  :defer t
  :hook (org-mode
         emacs-lisp-mode
         web-mode
         typescript-mode
         js2-mode))

(dn/leader-key-def
  "a" '(:ignore t :which-key "apps"))

(defun read-file (file-path)
    (with-temp-buffer
      (insert-file-contents file-path)
      (buffer-string)))

  (defun dw/get-current-package-version ()
    (interactive)
    (let ((package-json-file (concat (eshell/pwd) "/package.json")))
      (when (file-exists-p package-json-file)
        (let* ((package-json-contents (read-file package-json-file))
               (package-json (ignore-errors (json-parse-string package-json-contents))))
          (when package-json
            (ignore-errors (gethash "version" package-json)))))))

  (defun dw/map-line-to-status-char (line)
    (cond ((string-match "^?\\? " line) "?")))

  (defun dw/get-git-status-prompt ()
    (let ((status-lines (cdr (process-lines "git" "status" "--porcelain" "-b"))))
      (seq-uniq (seq-filter 'identity (mapcar 'dw/map-line-to-status-char status-lines)))))

  (defun dw/get-prompt-path ()
    (let* ((current-path (eshell/pwd))
           (git-output (shell-command-to-string "git rev-parse --show-toplevel"))
           (has-path (not (string-match "^fatal" git-output))))
      (if (not has-path)
        (abbreviate-file-name current-path)
        (string-remove-prefix (file-name-directory git-output) current-path))))

  ;; This prompt function mostly replicates my custom zsh prompt setup
  ;; that is powered by github.com/denysdovhan/spaceship-prompt.
  (defun dw/eshell-prompt ()
    (let ((current-branch (magit-get-current-branch))
          (package-version (dw/get-current-package-version)))
      (concat
       "\n"
       (propertize (system-name) 'face `(:foreground "#62aeed"))
       (propertize " ॐ " 'face `(:foreground "white"))
       (propertize (dw/get-prompt-path) 'face `(:foreground "#82cfd3"))
       (when current-branch
         (concat
          (propertize " • " 'face `(:foreground "white"))
          (propertize (concat " " current-branch) 'face `(:foreground "#c475f0"))))
       (when package-version
         (concat
          (propertize " @ " 'face `(:foreground "white"))
          (propertize package-version 'face `(:foreground "#e8a206"))))
       (propertize " • " 'face `(:foreground "white"))
       (propertize (format-time-string "%I:%M:%S %p") 'face `(:foreground "#5a5b7f"))
       (if (= (user-uid) 0)
           (propertize "\n#" 'face `(:foreground "red2"))
         (propertize "\nλ" 'face `(:foreground "#aece4a")))
       (propertize " " 'face `(:foreground "white")))))

(add-hook 'eshell-banner-load-hook
              (lambda ()
                 (setq eshell-banner-message
                       (concat "\n" (propertize " " 'display (create-image "~/.dotfiles/.emacs.d/images/flux_banner.png" 'png nil :scale 0.2 :align-to "center")) "\n\n"))))

  (defun dw/eshell-configure ()
    (require 'evil-collection-eshell)
    (evil-collection-eshell-setup)

    (use-package xterm-color)

    (push 'eshell-tramp eshell-modules-list)
    (push 'xterm-color-filter eshell-preoutput-filter-functions)
    (delq 'eshell-handle-ansi-color eshell-output-filter-functions)

    ;; Save command history when commands are entered
    (add-hook 'eshell-pre-command-hook 'eshell-save-some-history)

    (add-hook 'eshell-before-prompt-hook
              (lambda ()
                (setq xterm-color-preserve-properties t)))

    ;; Truncate buffer for performance
    (add-to-list 'eshell-output-filter-functions 'eshell-truncate-buffer)

    ;; We want to use xterm-256color when running interactive commands
    ;; in eshell but not during other times when we might be launching
    ;; a shell command to gather its output.
    (add-hook 'eshell-pre-command-hook
              (lambda () (setenv "TERM" "xterm-256color")))
    (add-hook 'eshell-post-command-hook
              (lambda () (setenv "TERM" "dumb")))

    ;; Use completion-at-point to provide completions in eshell
    (define-key eshell-mode-map (kbd "<tab>") 'completion-at-point)

    ;; Initialize the shell history
    (eshell-hist-initialize)

    (evil-define-key '(normal insert visual) eshell-mode-map (kbd "C-r") 'consult-history)
    (evil-define-key '(normal insert visual) eshell-mode-map (kbd "<home>") 'eshell-bol)
    (evil-normalize-keymaps)

    (setenv "PAGER" "cat")

    (setq eshell-prompt-function      'dw/eshell-prompt
          eshell-prompt-regexp        "^λ "
          eshell-history-size         10000
          eshell-buffer-maximum-lines 10000
          eshell-hist-ignoredups t
          eshell-highlight-prompt t
          eshell-scroll-to-bottom-on-input t
          eshell-prefer-lisp-functions nil))

  (use-package eshell
    :hook (eshell-first-time-mode . dw/eshell-configure)
    :init
    (setq eshell-directory-name "~/.emacs.d/eshell/")
          eshell-aliases-file (expand-file-name "~/.emacs.d/eshell/alias"))

  (use-package eshell-z
    :hook ((eshell-mode . (lambda () (require 'eshell-z)))
           (eshell-z-change-dir .  (lambda () (eshell/pushd (eshell/pwd))))))

  (use-package exec-path-from-shell
    :init
    (setq exec-path-from-shell-check-startup-files nil)
    :config
    (when (memq window-system '(mac ns x))
      (exec-path-from-shell-initialize)))

  (dn/leader-key-def
    "SPC" 'eshell)

(with-eval-after-load 'esh-opt
  (setq eshell-destroy-buffer-when-process-dies t)
  (setq eshell-visual-commands '("htop" "zsh" "nvim" "vim")))

(use-package fish-completion
  :hook (eshell-mode . fish-completion-mode))

(use-package eshell-syntax-highlighting
  :after esh-mode
  :config
  (eshell-syntax-highlighting-global-mode +1))

(use-package esh-autosuggest
  :hook (eshell-mode . esh-autosuggest-mode)
  :config
  (setq esh-autosuggest-delay 0.5)
  (set-face-foreground 'company-preview-common "#4b5668")
  (set-face-background 'company-preview nil))

(use-package eshell-toggle
  :bind ("C-M-'" . eshell-toggle)
  :custom
  (eshell-toggle-size-fraction 3)
  (eshell-toggle-use-projectile-root t)
  (eshell-toggle-run-command nil))

(use-package vterm
  :commands vterm
  :config
  (setq vterm-max-scrollback 10000))

(use-package multi-term
  :commands multi-term-next
  :config
  (setq term-buffer-maximum-size 10000)
  (setq term-scroll-to-bottom-on-output t)
  (add-hook 'term-mode-hook
            (lambda ()
              (add-to-list 'term-bind-key-alist '("M-[" . multi-term-prev))
              (add-to-list 'term-bind-key-alist '("M-]" . multi-term-next)))))
(dn/leader-key-def
  "C-SPC" 'multi-term-next)

(use-package better-shell
    :straight t)

(use-package elfeed
  :straight t
  :config
  (setq elfeed-feeds
        '("https://www.youtube.com/feeds/videos.xml?channel_id=UCd8wC6TEa04SP9p4FjED12A"
          "https://www.youtube.com/feeds/videos.xml?channel_id=UCiDJtJKMICpb9B1qf7qjEOA"
          "https://www.youtube.com/feeds/videos.xml?channel_id=UCYO_jab_esuFRV4b17AJtAw"
          "https://www.youtube.com/feeds/videos.xml?channel_id=UC39z4_U8Kls0llAij3RRZAQ"
          "https://www.youtube.com/feeds/videos.xml?channel_id=UCcg4MQ4IrGArGAIf1BSMHSg"
          "https://www.youtube.com/feeds/videos.xml?channel_id=UCiDJtJKMICpb9B1qf7qjEOA"
          "https://www.youtube.com/feeds/videos.xml?channel_id=UCYtb6DikDDAFn5iD494bYnw"
          "https://www.youtube.com/feeds/videos.xml?channel_id=UCH4pNKkgp6SKjb7-RfcrpLg"
          "https://www.youtube.com/feeds/videos.xml?channel_id=UCyWpRDBBb84ogtALVskwciw"
          "https://www.youtube.com/feeds/videos.xml?channel_id=UCivA7_KLKWo43tFcCkFvydw"
          "https://www.youtube.com/feeds/videos.xml?channel_id=UCL-ZolAIU3J4Gx8KNjuLyLw"
          "https://www.youtube.com/feeds/videos.xml?channel_id=UCkURR2CLd5iDc0B11rSkFeg"
          "https://www.youtube.com/feeds/videos.xml?channel_id=UCraiFqWi0qSIxXxXN4IHFBQ"
          "https://www.youtube.com/feeds/videos.xml?channel_id=UCVkguwL6-IcxiktEwT_ooxQ"
          "https://www.youtube.com/feeds/videos.xml?channel_id=UCS0N5baNlQWJCUrhCEo8WlA"
          "https://www.youtube.com/feeds/videos.xml?channel_id=UCvQECJukTDE2i6aCoMnS-Vg"
          "https://www.youtube.com/feeds/videos.xml?channel_id=UCt_t6FwNsqr3WWoL6dFqG9w"
          "https://www.youtube.com/feeds/videos.xml?channel_id=UCFrjdcImgcQVyFbK04MBEhA"
          "https://www.youtube.com/feeds/videos.xml?channel_id=UCm0rmRuPifODAiW8zSLXs2A"
          "https://www.youtube.com/feeds/videos.xml?channel_id=UC7UAVEyiMRCRb8dWCpxC5qA"
          "https://www.youtube.com/feeds/videos.xml?channel_id=UCIwpdjmSUJmqJ8HwvIGNqig"
          "https://www.youtube.com/feeds/videos.xml?channel_id=UCi6roWLrNBmXCCF8URGQE3A"
          "https://www.youtube.com/feeds/videos.xml?channel_id=UCzNf0liwUzMN6_pixbQlMhQ"
          "https://www.youtube.com/feeds/videos.xml?channel_id=UC9-y-6csu5WGm29I7JiwpnA"
          "https://www.youtube.com/feeds/videos.xml?channel_id=UCCezIgC97PvUuR4_gbFUs5g"
          "https://www.youtube.com/feeds/videos.xml?channel_id=UCXE_7rAvd92tXFS0d68sxdA"
          "https://www.youtube.com/feeds/videos.xml?channel_id=UCPZqSeBf8fCeD667iWEGoSw"
          "https://www.youtube.com/feeds/videos.xml?channel_id=UCEfj5PUP3pMYoOD_BuRTjYg"
          "https://www.youtube.com/feeds/videos.xml?channel_id=UCOIHBHRbvncMo7Bf0Vx1zEQ"
          "https://www.youtube.com/feeds/videos.xml?channel_id=UCsU-ktDYPSvRyRqcFV4uXPw"
          "https://www.youtube.com/feeds/videos.xml?channel_id=UCgBmKSbdGhiUJGnf2SCL0-w"
          "https://www.youtube.com/feeds/videos.xml?channel_id=UCo-3ThNQmPmQSQL_L6Lx1_w"
          "https://www.youtube.com/feeds/videos.xml?channel_id=UC6Om9kAkl32dWlDSNlDS9Iw"
          "https://www.youtube.com/feeds/videos.xml?channel_id=UCwRXb5dUK4cvsHbx-rGzSgw"
          "https://www.youtube.com/feeds/videos.xml?channel_id=UCVls1GmFKf6WlTraIb_IaJg"
          "https://www.youtube.com/feeds/videos.xml?channel_id=UCoj6RxIAQq8kmJme-5dnN0Q"
          "https://www.youtube.com/feeds/videos.xml?channel_id=UCea5cMUa9xNU0kUtbRcTkqA"
          "https://www.youtube.com/feeds/videos.xml?channel_id=UC5Wi_NYysX-LfcqT3Hq9Faw"
          "https://www.youtube.com/feeds/videos.xml?channel_id=UChturLXwYxwTOf_5krs0qvA"
          "https://www.youtube.com/feeds/videos.xml?channel_id=UC2bkHVIDjXS7sgrgjFtzOXQ"
          "https://www.youtube.com/feeds/videos.xml?channel_id=UC2riBMG3qf1Di20ouRc76BA"
          "https://www.youtube.com/feeds/videos.xml?channel_id=UCT2WgHiawhWgbahB0CNENkg"
          "https://www.youtube.com/feeds/videos.xml?channel_id=UC8soFWRYXUoE6IIT16BrEZg"
          "https://www.youtube.com/feeds/videos.xml?channel_id=UC8butISFwT-Wl7EV0hUK0BQ"
          "https://www.youtube.com/feeds/videos.xml?channel_id=UC18vz5hUUqxbGvym9ghtX_w"
          "https://www.youtube.com/feeds/videos.xml?channel_id=UCnfXz2OeVJHYyebACjj4ZGA"
          "https://www.youtube.com/feeds/videos.xml?channel_id=UC3s0BtrBJpwNDaflRSoiieQ"
          "https://www.youtube.com/feeds/videos.xml?channel_id=UC6iWKC08iw9K-R6Wh5pbZNQ"
          "https://www.youtube.com/feeds/videos.xml?channel_id=UC-r13SLLdZtZNmuC2bMnlmw"
          "https://www.youtube.com/feeds/videos.xml?channel_id=UCES8U3Hy6VUxU0kwKBdxSWQ"
          "https://www.youtube.com/feeds/videos.xml?channel_id=UCfe_znKY1ukrqlGActlFmaQ"
          "https://www.youtube.com/feeds/videos.xml?channel_id=UCRE3NFNtdjR96-H4QG4U1Fg"
          "https://www.youtube.com/feeds/videos.xml?channel_id=UCQhqmV26773qZhzqJz4VFcw"
          "https://www.youtube.com/feeds/videos.xml?channel_id=UCVHICXXtKG7rZgtC5xonNdQ"
          "https://www.youtube.com/feeds/videos.xml?channel_id=UCMlfKvFrEpzg1PEpTzJDWoA"
          "https://www.youtube.com/feeds/videos.xml?channel_id=UC6x7GwJxuoABSosgVXDYtTw"
          "https://www.youtube.com/feeds/videos.xml?channel_id=UCQVsTJx31Q_6o1bW9BHaO2w"
          "https://www.youtube.com/feeds/videos.xml?channel_id=UCh4pyZUB0mNzieaKv831flA"
          "https://www.youtube.com/feeds/videos.xml?channel_id=UCO39zTYpvWL5jx2q15Ma_Hw"
          "https://www.youtube.com/feeds/videos.xml?channel_id=UCR-DXc1voovS8nhAvccRZhg"
          "https://www.youtube.com/feeds/videos.xml?channel_id=UCQMkHY8U6B9tefTQdPkpY7A"
          "https://www.youtube.com/feeds/videos.xml?channel_id=UCjA8vRlL1c7BDixQRJ39-LQ"
          "https://www.youtube.com/feeds/videos.xml?channel_id=UCzGbp-rRVNwyFhn9gHoZr5g"
          "https://www.youtube.com/feeds/videos.xml?channel_id=UCAk3t7WHs2zjsZpopox8Taw"
          "https://www.youtube.com/feeds/videos.xml?channel_id=UC7Gmtjqs_LnhYQqTx469z0g"
          "https://www.youtube.com/feeds/videos.xml?channel_id=UC2umy62ojMfxzzHkVcgEUUA"
          "https://www.youtube.com/feeds/videos.xml?channel_id=UCxFWzKZa74SyAqpJyVlG5Ew"
          "https://www.youtube.com/feeds/videos.xml?channel_id=UCFNJ1UTWUeLANzzfz4s3a1g"
          "https://www.youtube.com/feeds/videos.xml?channel_id=UC1nHiti98G6m4OHZxQKMAaw"
          "https://www.youtube.com/feeds/videos.xml?channel_id=UCJhQG42TEDbS3yshdzj2uPg"
          "https://www.youtube.com/feeds/videos.xml?channel_id=UCsXVk37bltHxD1rDPwtNM8Q"
          "https://www.youtube.com/feeds/videos.xml?channel_id=UCG0jTvs7MHZHK-QaLQUgUig"
          "https://www.youtube.com/feeds/videos.xml?channel_id=UCfBqaomkd7VcyOibBX2xtqA"
          "https://www.youtube.com/feeds/videos.xml?channel_id=UCVTyTA7-g9nopHeHbeuvpRA"
          "https://www.youtube.com/feeds/videos.xml?channel_id=UC-ga3onzHSJFAGsIebtVeBg"
          "https://www.youtube.com/feeds/videos.xml?channel_id=UCpJmBQ8iNHXeQ7jQWDyGe3A"
          "https://www.youtube.com/feeds/videos.xml?channel_id=UC2Yx6hbjxJeh2ZrFjRFox7w"
          "https://www.youtube.com/feeds/videos.xml?channel_id=UC2eYFnH61tmytImy1mTYvhA"
          "https://www.youtube.com/feeds/videos.xml?channel_id=UCyouFZEIutTUVJQpE19n17A"
          "https://www.youtube.com/feeds/videos.xml?channel_id=UC7_YxT-KID8kRbqZo7MyscQ"
          "https://www.youtube.com/feeds/videos.xml?channel_id=UCFCTrfb1JUJjs3Im8OZDtBw"
          "https://www.youtube.com/feeds/videos.xml?channel_id=UCckETVOT59aYw80B36aP9vw"
          "https://www.youtube.com/feeds/videos.xml?channel_id=UCgqGOWaaSv4AZ3Qe737eC_g"
          "https://www.youtube.com/feeds/videos.xml?channel_id=UCtuLEGI-JkI6VFCW-5ZYtbw"
          "https://www.youtube.com/feeds/videos.xml?channel_id=UCNdNYSjAuVgq6k60diMnfRg"
          "https://www.youtube.com/feeds/videos.xml?channel_id=UCQNm-orYZ_Gv6rp2iTUoS7g"
          "https://www.youtube.com/feeds/videos.xml?channel_id=UCPV4BsRMseQ23RKy73uplyw"
          "https://www.youtube.com/feeds/videos.xml?channel_id=UCEBb1b_L6zDS3xTUrIALZOw"
          "https://www.youtube.com/feeds/videos.xml?channel_id=UCaTznQhurW5AaiYPbhEA-KA"
          "https://www.youtube.com/feeds/videos.xml?channel_id=UCGm3CO6LPcN-Y7HIuyE0Rew"
          "https://www.youtube.com/feeds/videos.xml?channel_id=UCDR1aI5qGaK7l4y0gqDGGLg"
          "https://www.youtube.com/feeds/videos.xml?channel_id=UC9QOtgGrDyuYcfqsTG4rGDg"
          "https://www.youtube.com/feeds/videos.xml?channel_id=UC0sA4gvlWFHfdwaPlBDqnGg"
          "https://www.youtube.com/feeds/videos.xml?channel_id=UCHFvKf-ATrhs3jbjj793N6w"
          "https://www.youtube.com/feeds/videos.xml?channel_id=UCcSeeATlWJJbXpOZRYOfaDg"
          "https://www.youtube.com/feeds/videos.xml?channel_id=UCzlwH112g2vFyLF0hb2xDvw"
          "https://www.youtube.com/feeds/videos.xml?channel_id=UC8TZwtZ17WKFJSmwTZQpBTA"
          "https://www.youtube.com/feeds/videos.xml?channel_id=UC2h7jWipUgmlt30Wu4oozKQ"
          "https://www.youtube.com/feeds/videos.xml?channel_id=UCe32g28-xzrG-sa11VfOSnw"
          "https://www.youtube.com/feeds/videos.xml?channel_id=UCd6vizTYlSgpR6zJ8j5KiyA"
          "https://www.youtube.com/feeds/videos.xml?channel_id=UC4eYXhJI4-7wSWc8UNRwD4A"
          "https://www.youtube.com/feeds/videos.xml?channel_id=UCoxcjq-8xIDTYp3uz647V5A"
          "https://www.youtube.com/feeds/videos.xml?channel_id=UCyp1gCHZJU_fGWFf2rtMkCg"
          "https://www.youtube.com/feeds/videos.xml?channel_id=UCaV77OIv89qfsnncY5J2zvg"
          "https://www.youtube.com/feeds/videos.xml?channel_id=UCiB8h9jD2Mlxx96ZFnGDSJw"
          "https://www.youtube.com/feeds/videos.xml?channel_id=UCFe6jenM1Bc54qtBsIJGRZQ"
          "https://www.youtube.com/feeds/videos.xml?channel_id=UC6ZFN9Tx6xh-skXCuRHCDpQ"
          "https://www.youtube.com/feeds/videos.xml?channel_id=UC7_gcs09iThXybpVgjHZ_7g"
          "https://www.youtube.com/feeds/videos.xml?channel_id=UC0k27bBwxkKZdentShJj-kQ"
          "https://www.youtube.com/feeds/videos.xml?channel_id=UCn25nZ12HEZq_w_m_1DmbbA"
          "https://www.youtube.com/feeds/videos.xml?channel_id=UCIWNozFjO8yVdJFsGKVmPgg"
          "https://www.youtube.com/feeds/videos.xml?channel_id=UCMOqf8ab-42UUQIdVoKwjlQ"
          "https://www.youtube.com/feeds/videos.xml?channel_id=UCWNK7JuxL2jIpME2gjEnl-g"
          "https://www.youtube.com/feeds/videos.xml?channel_id=UCAL3JXZSzSm8AlZyD3nQdBA"
          "https://www.youtube.com/feeds/videos.xml?channel_id=UC68KSmHePPePCjW4v57VPQg"
          "https://www.youtube.com/feeds/videos.xml?channel_id=UCI0vQvr9aFn27yR6Ej6n5UA"
          "https://www.youtube.com/feeds/videos.xml?channel_id=UC54NcJvLCvM2CNaBjd5j6HA"
          "https://www.youtube.com/feeds/videos.xml?channel_id=UCFJxE0l3cVYU4kHzi4qVEkw"
          "https://www.youtube.com/feeds/videos.xml?channel_id=UCEKSACqYwBf1jbH4LnyMXeA"
          "https://www.youtube.com/feeds/videos.xml?channel_id=UCOapBGVX8o19m1b0rlgsv0Q"
          "https://www.youtube.com/feeds/videos.xml?channel_id=UCvhsiQGy_zcNCiSbeXEjhLg"
          "https://www.youtube.com/feeds/videos.xml?channel_id=UClT2UAbC6j7TqOWurVhkuHQ"
          "https://www.youtube.com/feeds/videos.xml?channel_id=UCvLc83k5o11EIF1lEo0VmuQ"
          "https://www.youtube.com/feeds/videos.xml?channel_id=UCaTmuzKbejaJBvfeHLNYJqQ"
          "https://www.youtube.com/feeds/videos.xml?channel_id=UCemXXQkPWZGzi-_0rKGd0ow"
          "https://www.youtube.com/feeds/videos.xml?channel_id=UCh1VOd-1ZIGzDNNxAvtrX4A"
          "https://www.youtube.com/feeds/videos.xml?channel_id=UCi38HMIvRpGgMJ0Tlm1WYdw"
          "https://www.youtube.com/feeds/videos.xml?channel_id=UCqFzWxSCi39LnW1JKFR3efg"
          "https://www.youtube.com/feeds/videos.xml?channel_id=UCfzlCWGWYyIQ0aLC5w48gBQ"
          "https://www.youtube.com/feeds/videos.xml?channel_id=UCNofX8wmSJh7NTklvMqueOA"
          "https://www.youtube.com/feeds/videos.xml?channel_id=UCaIuFNmawD5i7_KgXYMH11A"
          "https://www.youtube.com/feeds/videos.xml?channel_id=UCvBqzzvUBLCs8Y7Axb-jZew"
          "https://www.youtube.com/feeds/videos.xml?channel_id=UC6107grRI4m0o2-emgoDnAA"
          "https://www.youtube.com/feeds/videos.xml?channel_id=UCCvcd0FYi58LwyTQP9LITpA"
          "https://www.youtube.com/feeds/videos.xml?channel_id=UCfjaAUlTZRHJapJmCT6eyIg"
          "https://www.youtube.com/feeds/videos.xml?channel_id=UC4E98HDsPXrf5kTKIgrSmtQ"
          "https://www.youtube.com/feeds/videos.xml?channel_id=UCSju5G2aFaWMqn-_0YBtq5A"
          "https://www.youtube.com/feeds/videos.xml?channel_id=UC-EnprmCZ3OXyAoG7vjVNCA"
          "https://www.youtube.com/feeds/videos.xml?channel_id=UCj1VqrHhDte54oLgPG4xpuQ"
          "https://www.youtube.com/feeds/videos.xml?channel_id=UCAiiOTio8Yu69c3XnR7nQBQ"
          "https://www.youtube.com/feeds/videos.xml?channel_id=UCWlhyyYBiD67Aju1CXUgaug"
          "https://www.youtube.com/feeds/videos.xml?channel_id=UCi7GJNg51C3jgmYTUwqoUXA"
          "https://www.youtube.com/feeds/videos.xml?channel_id=UCX2Vhc0LIzSS9aMzhGFZ7PA"
          "https://www.youtube.com/feeds/videos.xml?channel_id=UC8uT9cgJorJPWu7ITLGo9Ww"
          "https://www.youtube.com/feeds/videos.xml?channel_id=UC0ArlFuFYMpEewyRBzdLHiw"
          "https://www.youtube.com/feeds/videos.xml?channel_id=UCyK4gDqgXia1a89ip0DglOA"
          "https://www.youtube.com/feeds/videos.xml?channel_id=UCMtFAi84ehTSYSE9XoHefig"
          "https://www.youtube.com/feeds/videos.xml?channel_id=UC7IcJI8PUf5Z3zKxnZvTBog"
          "https://www.youtube.com/feeds/videos.xml?channel_id=UC06E4Y_-ybJgBUMtXx8uNNw"
          "https://www.youtube.com/feeds/videos.xml?channel_id=UCGrpLw1W3P1_BC4J-Hpytww"
          "https://www.youtube.com/feeds/videos.xml?channel_id=UCUcAMH2MeZHQsVKJb8iVNUA"
          "https://www.youtube.com/feeds/videos.xml?channel_id=UCsfu-jdkX2_v2t3_igVQebg"
          "https://www.youtube.com/feeds/videos.xml?channel_id=UC2kEuLCRUrWAvHlupobn4aw"
          "https://www.youtube.com/feeds/videos.xml?channel_id=UCuQV_7hln2oL_nhfEShubZQ"
          "https://www.youtube.com/feeds/videos.xml?channel_id=UCVi6ofFy7QyJJrZ9l0-fwbQ"
          "https://www.youtube.com/feeds/videos.xml?channel_id=UCXEJNKH9I4xsoyUNN3IL96A"
          "https://www.youtube.com/feeds/videos.xml?channel_id=UCuIUGmbCDklkDCDm-cQqv2g"
          "https://www.youtube.com/feeds/videos.xml?channel_id=UCoC47do520os_4DBMEFGg4A"
          ))
  ;; Set youtube-dl executable path
  (setq youtube-dl-path "/usr/bin/youtube-dl")
  ;; Set video storage path
  (setq youtube-dl-output-dir "~/Videos/")
  (defun elfeed-download-video ()
    (interactive)
    (async-shell-command (format "%s -o \"%s%s\" -f bestvideo+bestaudio \"%s\""
                                 youtube-dl-path
                                 youtube-dl-output-dir
                                 "%(title)s.%(ext)s"
                                 (elfeed-entry-link elfeed-show-entry))))

  ;; Add `youtube` tag to all videos
  (add-hook 'elfeed-new-entry-hook
            (elfeed-make-tagger :feed-url "youtube\\.com"
                                :add '(video youtube))))

;; Make gc pauses faster by decreasing the threshold.
(setq gc-cons-threshold (* 2 1000 1000))
