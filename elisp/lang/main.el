;;; Robert Jarzmik
;;; 2008-03-28
;;; Global language specific files

;; C, Python, Java
(load "lang/c_modes")
(load "lang/python")
(load "lang/java")

;; Doxymacs
(load "lang/xml-parse")
(load "lang/doxymacs")
(require 'doxymacs)
(defun my-doxymacs-font-lock-hook ()
  (if (or (eq major-mode 'c-mode) (eq major-mode 'c++-mode))
      (doxymacs-font-lock)))
(add-hook 'font-lock-mode-hook 'my-doxymacs-font-lock-hook)

;;; Local Variables:
;;; eval: (defun byte-compile-this-file () (write-region (point-min) (point-max) buffer-file-name nil 't) (byte-compile-file buffer-file-name) nil)
;;; write-file-hooks: (byte-compile-this-file)
;;; End:
