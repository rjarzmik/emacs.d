;;; Robert Jarzmik
;;; 2011-07-27
;;; Gnus customization

(when (featurep 'activity)
  (add-to-list 'available-activities (make-activity :name "Gnus"
						    :open-hook (lambda ()
								 (delete-other-windows)
								 (gnus)))))

(provide 'my-gnus)

;;; Local Variables:
;;; eval: (defun byte-compile-this-file () (write-region (point-min) (point-max) buffer-file-name nil 't) (byte-compile-file buffer-file-name) nil)
;;; write-file-hooks: (byte-compile-this-file)
;;; End:
