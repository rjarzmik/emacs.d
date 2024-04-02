;;; Robert Jarzmik
;;; 2014-08-07
;;; Shell customization

(defun my-shell-hook ()
  "When a shell is spawned, change its name to take into account
the host where the shell is running on."
  (when (tramp-tramp-file-p default-directory)
    (let* (
	   (tramp-path (tramp-dissect-file-name default-directory))
	   (host (tramp-file-name-host tramp-path))
	   (user (if (tramp-file-name-user tramp-path)
		     (format "%s@" (tramp-file-name-user tramp-path)) ""))
	   (new-buffer-name (format "*shell:%s%s*" user host)))
      (rename-buffer new-buffer-name t))))

(add-hook 'shell-mode-hook 'my-shell-hook)

(provide 'my-shells)
