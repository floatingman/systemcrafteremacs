;;;; -*- lexical-binding: t; -*-

(setup (:pkg company)
  (global-company-mode 1)
  (company-tng-mode 1)
  (setq company-backends (remove 'company-dabbrev company-backends))
  (setq company-idle-delay nil)
  (setq company-minimum-prefix-length 2)
  (setq company-selection-wrap-around t)
  (setq company-tooltip-align-annotations t)
  (:with-state insert
    (:bind
     [tab] company-complete-common-or-cycle))
  (:with-map company-active-map
    (:bind
     [tab] company-select-next
     [backtab] company-select-previous))
  (:hide-mode))

(setup (:pkg consult)
  (setq consult-preview-key (kbd "M-."))
  (setq xref-show-definitions-function #'consult-xref)
  (setq xref-show-xrefs-function #'consult-xref)
  (advice-add #'completing-read-multiple
              :override #'consult-completing-read-multiple)
  (:load-after vertico
    (setq completion-in-region-function
          (lambda (&rest args)
            (apply (if vertico-mode
                       #'consult-completion-in-region
                     #'completion--in-region)
                   args)))))

(setup (:pkg embark)
  (:load-after which-key
    (defun embark-which-key-indicator ()
      (lambda (&optional keymap targets prefix)
        (if (null keymap)
            (which-key--hide-popup-ignore-command)
          (which-key--show-keymap
           (if (eq (plist-get (car targets) :type) 'embark-become)
               "Become"
             (format "Act on %s '%s'%s"
                     (plist-get (car targets) :type)
                     (embark--truncate-target (plist-get (car targets) :target))
                     (if (cdr targets) "…" "")))
           (if prefix
               (pcase (lookup-key keymap prefix 'accept-default)
                 ((and (pred keymapp) km) km)
                 (_ (key-binding prefix 'accept-default)))
             keymap)
           nil nil t (lambda (binding)
                       (not (string-suffix-p "-argument" (cdr binding))))))))
    (setq prefix-help-command #'embark-prefix-help-command)
    (setq embark-indicators '(embark-which-key-indicator
                              embark-highlight-indicator
                              embark-isearch-highlight-indicator))
    (:advise embark-completing-read-prompter :around (fn &rest args)
      (when-let ((win (get-buffer-window which-key--buffer 'visible)))
        (quit-window 'kill-buffer win)
        (let ((embark-indicators (delq #'embark-which-key-indicator
                                       embark-indicators)))
          (apply fn args))))
    (:global "C-," embark-act)))

(setup (:pkg embark-consult)
  (:load-after (embark consult)
    (:with-mode embark-collect-mode
      (:hook consult-preview-at-point-mode))))

(setup (:pkg orderless)
  (setq completion-category-defaults nil)
  (setq completion-category-overrides '((file (styles partial-completion))))
  (setq completion-styles '(orderless)))

(setup (:pkg (vertico :files (:defaults "extensions/*")))
  (:also-load vertico-repeat)
  (setq vertico-count 15)
  (setq vertico-resize t)
  (vertico-mode 1))

(provide 'dn-pkg-completion)
