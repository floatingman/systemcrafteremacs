(use-package company
  :diminish

  :init
  (setq company-idle-delay 0.3)
  (setq company-tooltip-limit 20)
  (setq company-minimum-prefix-length 2)

  :config
  (setq tab-always-indent 'complete)
  (defvar completion-at-point-functions-saved nil)

  (defun company-indent-for-tab-command (&optional arg)
    (interactive "P")
    (let ((completion-at-point-functions-saved completion-at-point-functions)
          (completion-at-point-functions '(company-complete-common-wrapper)))
      (indent-for-tab-command arg)))

  (defun company-complete-common-wrapper ()
    (let ((completion-at-point-functions completion-at-point-functions-saved))
      (company-complete-common)))
  ;; ----------------------------------------------------------------------------

  (global-company-mode 1)
  (add-to-list 'company-backends 'company-dabbrev t)
  (add-to-list 'company-backends 'company-ispell t)
  (add-to-list 'company-backends 'company-files t)
  (add-to-list 'company-begin-commands 'outshine-self-insert-command)
  (setq company-backends (remove 'company-ropemacs company-backends))

  (defun my-company-elisp-setup ()
    (set (make-local-variable 'company-backends)
         '((company-capf :with company-dabbrev-code))))

  ;; Usage based completion sorting
  (use-package company-statistics
    :hook ((emacs-lisp-mode lisp-interaction-mode) . my-company-elisp-setup)
    :config (company-statistics-mode)))

;;;; company-anaconda
;; Anaconda backend for company-mode
(use-package company-anaconda
  :config (add-to-list 'company-backends 'company-anaconda))

;;;; company-dict
;; A backend that emulates ac-source-dictionary
(use-package company-dict
  :config (add-to-list 'company-backends 'company-dict))

;;;; company-quickhelp
;; Popup documentation for completion candidates
(use-package company-quickhelp
  :init
  (setq company-quickhelp-use-propertized-text t)
  (setq company-quickhelp-delay 1)
  :config (company-quickhelp-mode 1))

;;;; company-web
;; Company version of ac-html, complete for web,html,emmet,jade,slim modes
(use-package company-web
  :config
  (defun my-company-web ()
    (set (make-local-variable 'company-backends) '(company-web-html))
    (company-mode t))
  :hook (web-mode . my-company-web))

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

(use-package helm
  :diminish helm-mode
  :if my-laptop-p
  :config
  (progn
    (require 'helm-config)
    (require 'helm-for-files)
    (setq helm-candidate-number-limit 100)
    (setq helm-completing-read-handlers-alist
          '((describe-function)
            (consult-bookmark)
            (org-refile-get-location)
            (consult-outline)
            (consult-line)
            (org-olpath-completing-read)
            (consult-mark)
            (org-refile)
            (consult-multi-occur)
            (describe-variable)
            (execute-extended-command)
            (consult-yank)))
    ;; From https://gist.github.com/antifuchs/9238468
    (setq helm-idle-delay 0.0 ; update fast sources immediately (doesn't).
          helm-input-idle-delay 0.01  ; this actually updates things
                                        ; reeeelatively quickly.
          helm-yas-display-key-on-candidate t
          helm-quick-update t
          helm-M-x-requires-pattern nil
          helm-ff-skip-boring-files t))
  (defadvice helm-files-insert-as-org-links (around sacha activate)
    (insert (mapconcat (lambda (candidate)
                         (org-link-make-string candidate))
                       (helm-marked-candidates)
                       "\n")))
  :bind (("C-c h" . helm-mini)
         ("C-h a" . helm-apropos)
         ("C-x C-b" . helm-buffers-list)
         ("C-x c o" . helm-occur)
         ("C-x c s" . helm-swoop)
         ("C-x c y" . helm-yas-complete)
         ("C-x c Y" . helm-yas-create-snippet-on-region)
         ("C-x c SPC" . helm-all-mark-rings)))

(provide 'init-completion)
