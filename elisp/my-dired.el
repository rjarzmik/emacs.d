;;; Robert Jarzmik
;;; 2011-07-16
;;; Dired customization

(put 'dired-find-alternate-file 'disabled nil)

(provide 'my-dired)

;;; Local Variables:
;;; eval: (defun byte-compile-this-file () (write-region (point-min) (point-max) buffer-file-name nil 't) (byte-compile-file buffer-file-name) nil)
;;; write-file-hooks: (byte-compile-this-file)
;;; End:
