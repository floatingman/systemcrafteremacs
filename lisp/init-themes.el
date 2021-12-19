(tool-bar-mode -1)

(setq my/frame-font-name "New Heterodox Mono")
;;(setq my/frame-font-name "Iosevka")

(defun my/fontify-frame (frame)
  (interactive)
  (if window-system
      (progn
        (if (> (x-display-pixel-width) 3000)
            (set-frame-font (format "%s 10" my/frame-font-name) nil t) ;; HiDPI but setting Xresources properly
          (if (> (x-display-pixel-width) 2600)
              (set-frame-font (format "%s 15" my/frame-font-name) nil t) ;; HIDPI
            (set-frame-font (format "%s 12" my/frame-font-name) nil t))))))

;; Fontify current frame
(my/fontify-frame nil)

;; Fontify any future frames
(push 'my/fontify-frame after-make-frame-functions)

(load "~/.emacs.d/setup-ligatures.el")

(use-package diminish)

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode))

;; icons for major modes
(use-package all-the-icons
  :demand)

(provide 'init-themes)
