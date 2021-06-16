(require 'map) ;; Needed for map-merge

(setq dn/system-settings
  (map-merge
    'list
    '((emacs/default-face-size . 220)
      (emacs/variable-face-size . 245)
      (emacs/fixed-face-size . 200))
    
    (when (equal system-name "sunstreaker")
      '((emacs/default-face-size . 190)
        (emacs/variable-face-size . 200)
        (emacs/fixed-face-size . 190)))
    ))
