;;; Robert Jarzmik
;;; 2008-03-28
;;; Global language specific files

;; C, Python, Java
(load "lang/c_modes")
(load "lang/python")
(load "lang/java")

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

(provide 'my-lang)

;;; Local Variables:
;;; eval: (defun byte-compile-this-file () (write-region (point-min) (point-max) buffer-file-name nil 't) (byte-compile-file buffer-file-name) nil)
;;; write-file-hooks: (byte-compile-this-file)
;;; End:
