;;; Robert Jarzmik
;;; 2011-07-16
;;; Dired customization

(when (featurep 'dired)
  (put 'dired-find-alternate-file 'disabled nil)

  (add-hook 'dired-mode-hook
	    (lambda ()
	      (define-key dired-mode-map (kbd "^")
		(lambda () (interactive) (find-alternate-file "..")))
					; was dired-up-directory
	      ))

  ;; Dired unique window
  (add-hook 'dired-after-readin-hook
	    (lambda () (rename-buffer (generate-new-buffer-name dired-directory))))
  (setq dired-recursive-deletes 'always))

(provide 'my-dired)

;;; Local Variables:
;;; eval: (defun byte-compile-this-file () (write-region (point-min) (point-max) buffer-file-name nil 't) (byte-compile-file buffer-file-name) nil)
;;; write-file-hooks: (byte-compile-this-file)
;;; End:
