;; Quick edit mode (like a small vi)
;; Usally used by defining: EDITOR="emacs -Q -nw -l ~/.emacs.d/elisp/quickedit/init.elc"

(column-number-mode t)
(transient-mark-mode t)
(custom-set-variables
 '(auto-save-file-name-transforms (quote (("\\`/[^/]*:\\(.+/\\)*\\(.*\\)" "/tmp/\\2") ("\\(.+/\\)*\\(.*\\)" "/tmp/\\2"))))
 '(auto-save-list-file-prefix "~/.emacs.d/data/auto-backup/auto-save-list/.saves-")
 '(backup-directory-alist (quote ((".*" . "~/.emacs.d/data/auto-backup/backup/")))))

(provide 'quickedit)
