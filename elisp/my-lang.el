;;; Robert Jarzmik
;;; 2008-03-28
;;; Global language specific files

;; C, Python, Java
(load "lang/c_modes")
(load "lang/python")
(load "lang/java")
(load "lang/my-lsp")
(load "lang/rust")

;; Compilation
(defvar my-compile-command-hooks nil
  "Hooks run to find a smart compile command. First hook returning a non nil strings wins. If no hook finds a smart compile command, `compile' is used.
The hook is called without any argument.")

(defun my-compile (arg)
  (interactive "P")
  (let ((command
	 (run-hook-with-args-until-success 'my-compile-command-hooks)))
    (let ((current-prefix-arg arg))
      (if (and (not arg) command)
	  (compile command)
	(call-interactively 'compile)))))

;; Doxymacs
(require 'doxymacs)
(defun my-doxymacs-font-lock-hook ()
  (if (or (eq major-mode 'c-mode) (eq major-mode 'c++-mode))
      (doxymacs-font-lock)))
(add-hook 'font-lock-mode-hook 'my-doxymacs-font-lock-hook)

;; Git
(require 'git-blame)

;; Autocompletion
;(add-to-list 'load-path "~/.emacs.d/elisp/lang/m2ym-auto-complete-dc1bf2b")
;(require 'auto-complete-config)
;(add-to-list 'ac-dictionary-directories "~/.emacs.d/elisp/lang/m2ym-auto-complete-dc1bf2b/dict")
;(ac-config-default)

;; CScope
(require 'rscope nil t)

;; IDE lsp
(require 'my-lsp)

(provide 'my-lang)
