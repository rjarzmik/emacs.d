;;; Robert Jarzmik
;;; 2011-07-15
;;; Definitions for work (Alcatel, Atos, ...)

(add-to-list 'load-path (concat user-emacs-directory "elisp/work"))
(provide 'my-work)

(dolist (package '(work-atos-icare))
  (when (not (require package nil t))
    (message (concat (symbol-name package) " package is not available"))))

;;; Local Variables:
;;; eval: (defun byte-compile-this-file () (write-region (point-min) (point-max) buffer-file-name nil 't) (byte-compile-file buffer-file-name) nil)
;;; write-file-hooks: (byte-compile-this-file)
;;; End:
