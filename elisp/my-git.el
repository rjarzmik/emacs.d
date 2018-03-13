;; Git commit edit mode
;; Usally used by defining: EDITOR="emacs -Q -nw -l ~/.emacs.d/elisp/my-git"

(defvar gitcommit-font-lock-keywords
  (list
   '("Signed-off-by" . font-lock-keyword-face)
   '("Acked-by" . font-lock-keyword-face)
   '("Tested-by" . font-lock-keyword-face)
   '("Reviewed-by" . font-lock-keyword-face)))

(defvar gitcommit-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?# "<" st)
    (modify-syntax-entry ?\n ">" st)
    st)
  "Syntax table for `gitcommit-mode'.")

(define-derived-mode gitcommit-mode fundamental-mode "GitCommit"
  "Major mode for editing git commit messages"
  :syntax-table gitcommit-mode-syntax-table
  (column-number-mode t)
  (set-fill-column 60)
  (auto-fill-mode t)
  (transient-mark-mode t)

  (set (make-local-variable 'font-lock-defaults)
       '(gitcommit-font-lock-keywords))
  )
;;  (font-lock-mode))

;; Not used anymore, git-modes is now the default
;; (add-to-list 'auto-mode-alist '("COMMIT_EDITMSG" . gitcommit-mode))

(defun git-fixes-string(commitSha1)
  "Get the Fixes string for a given commit id"
  (interactive "sCommit id: ")
  (insert (with-temp-buffer
	    (shell-command (concat "git log --pretty=fixes -n 1 " commitSha1)
			   (current-buffer))
	    (buffer-string))))

;; Integrated git-modes into magit
(define-derived-mode magit-log-edit-mode git-commit-mode "Magit Log Edit"
  (set (make-local-variable 'git-commit-commit-function)
       (apply-partially #'call-interactively 'magit-log-edit-commit)))
(substitute-key-definition 'magit-log-edit-toggle-signoff
                           'git-commit-signoff
                           magit-log-edit-mode-map)
(provide 'my-git)
;;; Local Variables:
;;; eval: (defun byte-compile-this-file () (write-region (point-min) (point-max) buffer-file-name nil 't) (byte-compile-file buffer-file-name) nil)
;;; write-file-hooks: (byte-compile-this-file)
;;; End:
