;;; elisp.el --- Configuration Emacs -*- lexical-binding: t; -*-
;;; Robert Jarzmik
;;; 2026-11-11
;;; Global Emacs LISP specific files

(use-package
 elisp-autofmt
 :hook (emacs-lisp-mode . elisp-autofmt-mode)
 :custom
 (elisp-autofmt-cache-directory
  (concat user-emacs-directory "data/elisp-autofmt-cache"))
 (elisp-autofmt-on-save-p 'always) (elisp-autofmt-python-bin "python3"))
