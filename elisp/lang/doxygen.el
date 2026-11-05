;;; doxygen.el --- Configuration Emacs -*- lexical-binding: t; -*-

(use-package doxymacs
  :ensure t
  :hook (font-lock-mode . my-doxymacs-font-lock-hook)
  :config
  (defun my-doxymacs-font-lock-hook ()
    (if (or (eq major-mode 'c-mode) (eq major-mode 'c++-mode))
	(doxymacs-font-lock)))
  )
