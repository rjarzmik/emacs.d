;; Git commit edit mode
;; Usally used by defining: EDITOR="emacs -Q -nw -l ~/.emacs.d/elisp/my-git"

(defun git-fixes-string(commitSha1)
  "Get the Fixes string for a given commit id"
  (interactive "sCommit id: ")
  (insert (with-temp-buffer
	    (shell-command (concat "git log --pretty=fixes -n 1 " commitSha1)
			   (current-buffer))
	    (buffer-string))))

;; Integrated git-modes into magit
(use-package magit
  :ensure)

(provide 'my-git)
