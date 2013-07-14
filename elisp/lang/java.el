;;; My personnal Java modes
(add-hook 'java-mode-hook
	  (lambda ()
	  (setq indent-tabs-mode t)
	  (setq tab-width 4)
	  (setq c-basic-offset 4)))

;;; Local Variables:
;;; eval: (defun byte-compile-this-file () (write-region (point-min) (point-max) buffer-file-name nil 't) (byte-compile-file buffer-file-name) nil)
;;; write-file-hooks: (byte-compile-this-file)
;;; End:
