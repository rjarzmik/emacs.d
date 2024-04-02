;;; Robert Jarzmik
;;; 2011-07-15
;;; Definitions for work (Alcatel, Atos, ...)

(add-to-list 'load-path (concat user-emacs-directory "elisp/work"))
(provide 'my-work)

(dolist (package '(work-atos-icare))
  (when (not (require package nil t))
    (message (concat (symbol-name package) " package is not available"))))
