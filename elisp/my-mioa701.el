;;; Robert Jarzmik
;;; 2013-09-01

(dolist (package '(openocd))
  (when (not (require package nil t))
    (message (concat (symbol-name package) " package is not available"))))

(provide 'my-mioa701)
