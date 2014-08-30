;;; Robert Jarzmik
;;; 2008-03-28
;;; Global language specific files

;; C, Python, Java
(load "lang/c_modes")
(load "lang/python")
(load "lang/java")

;; Compilation
(defvar my-compile-command-hooks nil
  "Hooks run to find a smart compile command. First hook returning a non nil strings wins. If no hook finds a smart compile command, `compile' is used.
The hook is called without any argument.")

(defun my-compile ()
  (interactive)
  (let ((command
	 (run-hook-with-args-until-success 'my-compile-command-hooks)))
    (if command (compile command)
      ;(set (make-local-variable 'compile-command) command))
      (compile "make"))))

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
(require 'rscope)

(provide 'my-lang)

;;; Local Variables:
;;; eval: (defun byte-compile-this-file () (write-region (point-min) (point-max) buffer-file-name nil 't) (byte-compile-file buffer-file-name) nil)
;;; write-file-hooks: (byte-compile-this-file)
;;; End:
