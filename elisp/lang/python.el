;;; My personnal Python modes

; Python
(add-hook 'python-mode-hook
       	  '(lambda ()
             ;;(load "py-mode-ext")  
             ;;(load "pyp")
             (require 'pycomplete)
             (define-key py-mode-map [f12] 'pyp)
             (define-key py-mode-map "\C-c\C-c" 'py-execute-prog)
             (define-key py-mode-map "\C-c\C-g" 'py-call-pdb)
             (define-key py-mode-map "\C-c\C-w" 'pychecker)))
