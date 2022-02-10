(provide 'init-misc-packages)

(use-package engine-mode
  :config
  (defengine mail
    "https://mail.google.com/mail/u/0/#search/%s"
    :keybinding "m")
  (defengine google
    "https://google.com/search?q=%s"
    :keybinding "g")
  (defengine duckduckgo
    "https://duckduckgo.com/?q=%s"
    :keybinding "d")
  (defengine emacswiki
    "https://google.com/search?q=site:emacswiki.org+%s"
    :keybinding "e")
  (defengine google-maps
    "http://maps.google.com/maps?q=%s"
    :docstring "Mappin' it up.")
  (defengine github
    "https://github.com/search?ref=simplesearch&q=%s")
  (defengine youtube
    "http://www.youtube.com/results?aq=f&oq=&search_query=%s")
  (engine-mode)
  :hydra
  (my-engine-mode-hydra
   (:color blue)
   "Engine mode"
   ("m" engine/search-mail "mail")
   ("g" engine/search-google "google")
   ("d" engine/search-duckduckgo "duckduckgo")
   ("gh" engine/search-github "github")
   ("e" engine/search-emacswiki "emacswiki")))

(use-package eyebrowse)
(use-package eyebrowse-restore
  (:host github
   :repo "FrostyX/eyebrowse-restore"
   :branch "main")
  :config
  (eyebrowse-restore-mode))
