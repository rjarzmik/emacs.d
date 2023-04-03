;;; My personnal Python modes

; Python
(add-hook 'python-mode-hook
       	  '(lambda ()
             ;;(load "py-mode-ext")
             ;;(load "pyp")
             (define-key py-mode-map [f12] 'pyp)
             (define-key py-mode-map "\C-c\C-c" 'py-execute-prog)
             (define-key py-mode-map "\C-c\C-g" 'py-call-pdb)
             (define-key py-mode-map "\C-c\C-w" 'pychecker)))

;;; Local Variables:
;;; eval: (defun byte-compile-this-file () (write-region (point-min) (point-max) buffer-file-name nil 't) (byte-compile-file buffer-file-name) nil)
;;; write-file-hooks: (byte-compile-this-file)
;;; End:
