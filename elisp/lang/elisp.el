;;; elisp.el --- Configuration Emacs -*- lexical-binding: t; elisp-autofmt-load-packages-local: ("use-package" "use-package-core"); -*-
;;; Robert Jarzmik
;;; 2026-11-11
;;; Global Emacs LISP specific files

(use-package elisp-autofmt
  :hook (emacs-lisp-mode . elisp-autofmt-mode)
  :commands (elisp-autofmt-mode elisp-autofmt-buffer)
  :custom
  (elisp-autofmt-cache-directory
   (concat user-emacs-directory "data/elisp-autofmt-cache"))
  (elisp-autofmt-on-save-p 'always)
  (elisp-autofmt-python-bin "python3"))
