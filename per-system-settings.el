(require 'map) ;; Needed for map-merge

(setq dw/system-settings
  (map-merge
    'list
    '((emacs/default-face-size . 220)
      (emacs/variable-face-size . 245)
      (emacs/fixed-face-size . 200))))
