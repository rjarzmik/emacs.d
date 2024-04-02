;;; Robert Jarzmik
;;; 2011-07-27
;;; Gnus customization

(when (featurep 'activity)
  (add-to-list 'available-activities (make-activity :name "Gnus"
						    :open-hook (lambda ()
								 (delete-other-windows)
								 (gnus)))))

(provide 'my-gnus)
