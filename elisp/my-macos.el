;;; my-macos.el --- Configuration Emacs -*- lexical-binding: t; elisp-autofmt-load-packages-local: ("use-package" "use-package-core"); -*-
;;; I prefer cmd key for meta
(when (eq system-type 'darwin)
  (setq
   mac-option-key-is-meta nil
   mac-command-key-is-meta t
   mac-command-modifier 'meta
   mac-option-modifier 'none))

(provide 'my-macos)
