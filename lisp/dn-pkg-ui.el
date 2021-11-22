;;;; -*- lexical-binding: t; -*-

(setup (:pkg all-the-icons)
  (:load-after marginalia
    (:pkg all-the-icons-completion)
    (all-the-icons-completion-mode 1)
    (:with-mode marginalia-mode
      (:hook all-the-icons-completion-marginalia-setup)))
  (:load-after dired
    (:pkg all-the-icons-dired)
    (:with-mode dired-mode
      (:hook all-the-icons-dired-mode))))

(setup (:pkg centaur-tabs)
  (:require centaur-tabs)
  (setq centaur-tabs-adjust-buffer-order t)
  (setq centaur-tabs-cycle-scope 'tabs)
  (setq centaur-tabs-gray-out-icons 'buffer)
  (setq centaur-tabs-height 28)
  (setq centaur-tabs-modified-marker "●")
  (setq centaur-tabs-set-close-button nil)
  (setq centaur-tabs-set-icons t)
  (setq centaur-tabs-set-modified-marker t)
  (setq centaur-tabs-show-new-tab-button nil)
  (setq centaur-tabs-style "rounded")
  (centaur-tabs-group-by-projectile-project)
  (centaur-tabs-headline-match)
  (centaur-tabs-mode 1)
  (:bind
   "M-[" centaur-tabs-backward
   "M-]" centaur-tabs-forward)
  (:with-hook window-setup-hook
    (:hook (fn (centaur-tabs-change-fonts "Iosevka Slab" 120))))
  (defun centaur-tabs-hide-tab (x)
    (let ((name (format "%s" x)))
      (or (window-dedicated-p (selected-window))
          (string-prefix-p "*Messages*" name)
          (string-prefix-p "*scratch" name)
          (string-prefix-p "*Compile-Log*" name)
          (string-prefix-p "*company" name)
          (string-prefix-p "*Flycheck" name)
          (string-prefix-p "*tramp" name)
          (string-prefix-p " *Mini" name)
          (string-prefix-p "*help" name)
          (string-prefix-p "*straight" name)
          (string-prefix-p "*sly" name)
          (string-prefix-p " *temp" name)
          (string-prefix-p "*Help" name)
          (string-prefix-p "magit" name)))))

(setup (:pkg default-text-scale)
  (:bind
   "M--" default-text-scale-decrease
   "M-+" default-text-scale-increase
   "M-=" default-text-scale-reset)
  (default-text-scale-mode 1))

(setup (:pkg diff-hl)
  (global-diff-hl-mode 1)
  (diff-hl-flydiff-mode 1)
  (:with-mode dired-mode
    (:hook diff-hl-dired-mode))
  (:with-after magit
    (:with-hook magit-pre-refresh-hook
      (:hook diff-hl-magit-pre-refresh))
    (:with-hook magit-post-refresh-hook
      (:hook diff-hl-magit-post-refresh))))

(setup (:pkg dimmer)
  (setq dimmer-fraction 0.3)
  (dimmer-mode 1))

(setup (:pkg doom-modeline)
  (setq doom-modeline-bar-width 4)
  (setq doom-modeline-buffer-encoding nil)
  (setq doom-modeline-buffer-file-name-style 'relative-from-project)
  (setq doom-modeline-height 30)
  (setq doom-modeline-major-mode-icon t)
  (setq doom-modeline-minor-modes t)
  (:hook-into window-setup))

(setup (:pkg doom-themes)
  (setq doom-themes-enable-bold t)
  (setq doom-themes-enable-italic t)
  (doom-themes-org-config)
  (load-theme 'doom-one t))

(setup (:pkg emojify)
  (setq emojify-emoji-styles '(unicode))
  (global-emojify-mode 1))

(setup (:pkg evil-goggles)
  (:load-after evil
    (setq evil-goggles-duration 1.0)
    (setq evil-goggles-pulse nil)
    (evil-goggles-mode 1)
    (:hide-mode)))

(setup (:pkg helpful)
  (:when-loaded
    (:global-bind
     "C-h f" helpful-callable
     "C-h v" helpful-variable
     "C-h k" helpful-key)
    (:load-after link-hint
      (:with-state normal
        (:bind
         "o" link-hint-open-link-at-point))
      (setq helpful-switch-buffer-function
            (lambda (x)
              (if (eq major-mode 'helpful-mode)
                  (switch-to-buffer x)
                (pop-to-buffer x)))))))

(setup (:pkg highlight-numbers)
  (:hook-into prog-mode)
  (:hide-mode))

(setup (:pkg hl-fill-column)
  (:require hl-fill-column)
  (:hook-into prog-mode text-mode conf-mode))

(setup (:pkg hl-line)
  (global-hl-line-mode 1))

(setup (:pkg hl-todo)
  (global-hl-todo-mode 1))

(setup (:pkg marginalia)
  (:load-after vertico
    (marginalia-mode 1)))

(setup (:pkg rainbow-mode)
  (:hook-into web-mode-hook css-mode-hook)
  (:hide-mode))

(setup (:pkg solaire-mode)
  (solaire-global-mode 1))

(setup (:pkg unicode-fonts)
  (unicode-fonts-setup))

(setup (:pkg which-key)
  (which-key-mode 1)
  (setq which-key-add-column-padding 2)
  (setq which-key-idle-delay 0.5)
  (setq which-key-idle-secondary-delay 0.1)
  (setq which-key-max-display-columns nil)
  (setq which-key-min-display-lines 6)
  (setq which-key-replacement-alist
        '((("left") . ("�"))
          (("right") . ("�"))
          (("up") . ("�"))
          (("down") . ("�"))
          (("delete") . ("DEL"))
          (("\\`DEL\\'") . ("BKSP"))
          (("RET") . ("⏎"))
          (("next") . ("PgDn"))
          (("prior") . ("PgUp"))))
  (setq which-key-sort-order 'which-key-key-order-alpha)
  (setq which-key-sort-uppercase-first nil)
  (which-key-setup-minibuffer)
  (:with-hook which-key-init-buffer-hook
    (:hook (fn (setq line-spacing 4))))
  (:hide-mode))

(provide 'dn-pkg-ui)
