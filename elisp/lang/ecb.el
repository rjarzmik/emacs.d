;;; Robert Jarzmik
;;; 2010-04-08
;;; CEDET setup

;; Load ECB minimal options
(add-to-list 'load-path "~/.emacs.d/elisp/lang/ecb-2.40")
(require 'ecb-autoloads)

;;; Local Variables:
;;; eval: (defun byte-compile-this-file () (write-region (point-min) (point-max) buffer-file-name nil 't) (byte-compile-file buffer-file-name) nil)
;;; write-file-hooks: (byte-compile-this-file)
;;; End:
