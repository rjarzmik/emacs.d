;;; Robert Jarzmik
;;; 2014-08-07
;;; Shell customization

(defun my-shell-hook ()
  "When a shell is spawned, change its name to take into account
the host where the shell is running on."
  (let* ((tramp-path (when (tramp-tramp-file-p default-directory)
		       (tramp-dissect-file-name default-directory)))
	 (host (tramp-file-name-real-host tramp-path))
	 (user (if (tramp-file-name-user tramp-path)
		   (format "%s@" (tramp-file-name-user tramp-path)) ""))
	 (new-buffer-name (format "*shell:%s%s*" user host)))
    (rename-buffer new-buffer-name t)))

(add-hook 'shell-mode-hook 'my-shell-hook)

(provide 'my-shells)

;;; Local Variables:
;;; eval: (defun byte-compile-this-file () (write-region (point-min) (point-max) buffer-file-name nil 't) (byte-compile-file buffer-file-name) nil)
;;; write-file-hooks: (byte-compile-this-file)
;;; End:
