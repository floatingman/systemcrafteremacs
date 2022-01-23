(use-package company
  :config (add-hook 'prog-mode-hook 'company-mode))
(use-package company-posframe :init (company-posframe-mode 1) :diminish)

(use-package selectrum :quelpa (selectrum :fetcher github :repo "raxod502/selectrum") :init (selectrum-mode +1))

(use-package prescient :config (prescient-persist-mode +1))
(use-package selectrum-prescient :init (selectrum-prescient-mode +1) :after selectrum)
(use-package company-prescient :init (company-prescient-mode +1))

(use-package consult :quelpa (consult :fetcher github :repo "minad/consult")
  :after projectile
  :bind (("C-x r x" . consult-register)
         ("C-x r b" . consult-bookmark)
         ("C-c k" . consult-kmacro)
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complet-command
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ("M-g o" . consult-outline)
         ("M-g h" . consult-org-heading)
         ("M-g a" . consult-org-agenda)
         ("M-g m" . consult-mark)
         ("C-x b" . consult-buffer)
         ("<help> a" . consult-apropos)            ;; orig. apropos-command
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-project-imenu)
         ("M-g e" . consult-error)
         ;; M-s bindings (search-map)
         ("M-s f" . consult-find)
         ("M-s L" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s m" . consult-multi-occur)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch)
         ("M-g l" . consult-line)
         ("M-s m" . consult-multi-occur)
         ("C-x c o" . consult-multi-occur)
         ("C-x c SPC" . consult-mark)
         :map isearch-mode-map
         ("M-e" . consult-isearch)                 ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch)               ;; orig. isearch-edit-string
         ("M-s l" . consult-line))
  :init
  (setq register-preview-delay 0
        register-preview-function #'consult-register-format)
  :config
  (setq consult-project-root-function #'projectile-project-root)
  (setq consult-narrow-key "<"))

(use-package marginalia :quelpa (marginalia :fetcher github :repo "minad/marginalia")
  :init
  (marginalia-mode)
  :config
  (setq marginalia-annotators (if my-laptop-p
                                  '(marginalia-annotators-heavy marginalia-annotators-light)
                                '(marginalia-annotators-light)))
  (advice-add #'marginalia-cycle :after
              (lambda () (when (bound-and-true-p selectrum-mode) (selectrum-exhibit))))
  :bind (:map minibuffer-local-completion-map
              ("M-A" . marginalia-cycle)
              ("C-i" . marginalia-cycle-annotators)))

(use-package embark
    :after selectrum
    :config
    (setq embark-prompter 'embark-keymap-prompter)
    (add-to-list 'embark-target-finders 'my-embark-org-element)
    :bind
    (("C-." . embark-act)
     ("C-;" . embark-act)
     :map minibuffer-local-map
     (("C-c e" . embark-act)
      ("C-;" . embark-act))
     :map embark-collect-mode-map
     (("C-c e" . embark-act)
      ("C-;" . embark-act))
     :map embark-general-map
     (("j" . my-journal-post)
      ("m" . my-stream-message)
      ("M-w" . (lambda (s) (interactive "MString: ") (kill-new s))))
     :map embark-symbol-map
     ("r" . erefactor-rename-symbol-in-buffer)
     :map embark-variable-map
     ("l" . edit-list)
     :map embark-url-map
     ("c" . my-caption-show)))

  (use-package
    embark-consult
    :after (embark consult)
    :demand t                ; only necessary if you have the hook below
    ;; if you want to have consult previews as you move around an
    ;; auto-updating embark collect buffer
    :hook (embark-collect-mode . embark-consult-preview-minor-mode))


(defun my-embark-org-element ()
  "Target an Org Mode element at point."
  (save-window-excursion
    (save-excursion
      (save-restriction
        (when (derived-mode-p 'org-agenda-mode)
          (org-goto-marker-or-bmk (org-get-at-bol 'org-marker))
          (org-back-to-heading))
        (when (derived-mode-p 'org-mode)
          (let* ((context ;; Borrowed from org-open-at-point
	                ;; Only consider supported types, even if they are not the
	                ;; closest one.
	                (org-element-lineage (org-element-context)
                                       '(headline src-block link) t))
                 (type (org-element-type context))
                 (value (org-element-property :value context)))
            (cond ((eq type 'headline)
                   (cons 'org-heading (org-element-property :title context)))
                  ((eq type 'src-block)
                   (cons 'org-src-block (org-element-property :name context)))
                  ((eq type 'link)
                   (cons 'url (org-element-property :raw-link context))))))))))

(defun my-embark-org-src-block-copy-noweb-reference (element)
  (kill-new (if (org-element-property element :parameters)
                (format "<<%s(%s)>>" (org-element-property element :name)
                        (org-element-property element :parameters))
              (format "<<%s>>" (org-element-property element :parameters)))))

(provide 'init-completion)
