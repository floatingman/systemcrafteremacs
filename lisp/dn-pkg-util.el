;;;; -*- lexical-binding: t; -*-

(setup (:pkg alert)
  (setq alert-default-style 'libnotify))

(setup (:pkg browse-at-remote))

(setup (:pkg browse-url-dwim)
  (setq browse-url-browser-function 'browse-url-xdg-open)
  (setq browse-url-dwim-always-confirm-extraction nil))

(setup (:pkg general)
  (:load-after evil
    (general-create-definer define-keys
      :states '(normal motion visual insert emacs))
    (general-create-definer define-leader-keys
      :states '(normal visual insert eemacs)
      :prefix "SPC"
      :non-normal-prefix "M-SPC")
    (general-create-definer define-local-keys
      :major-modes t
      :states '(normal visual insert emacs)
      :prefix ","
      :non-normal-prefix "M-,")))

(setup (:pkg gist)
  (setq gist-view-gist t)
  (:hide-mode))

(setup (:pkg link-hint)
  (:require link-hint))

(setup (:pkg rg)
  (:when-loaded
    (setq rg-group-result t)
    (setq rg-ignore-case 'smart)))

(provide 'dn-pkg-util)
