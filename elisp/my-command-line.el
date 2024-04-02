;;; Robert Jarzmik
;;; 2010-04-12
;;; Command-line invocation utilities

;; diff session from command line
(defun command-line-diff (switch)
  (let ((file1 (pop command-line-args-left))
        (file2 (pop command-line-args-left)))
    (ediff file1 file2)))

(add-to-list 'command-switch-alist '("diff" . command-line-diff))

(provide 'my-command-line)
