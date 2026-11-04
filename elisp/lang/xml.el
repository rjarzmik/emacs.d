;; Xml editing
(use-package nxml-mode
  :init
  (unless (package-installed-p 'nxml-mode-extensions)
    (package-vc-install "https://github.com/peterhoeg/nxml-mode-extensions"))
  :config
  (nxml-mode-extensions-enable-current-path-maybe)
  (nxml-mode-extensions-enable-hideshow)
  )

(use-package nxml-mode-extensions
  :defer t)
