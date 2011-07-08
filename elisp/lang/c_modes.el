;;; My personnal C modes

(defun linux-c-mode ()
  "C mode with adjusted defaults for use with the Linux kernel."
  (interactive)
  (c-mode)
  (c-set-style "linux")
  ; Dirty workaround for labels in column 0
  (remove-hook 'c-special-indent-hook 'c-gnu-impose-minimum)
  (highlight-beyond-fill-column)
  (setq show-trailing-whitespace t)
  )

(setq auto-mode-alist (cons '("/home/rj/.*/kernel.*/.*\\.[ch]$" . linux-c-mode)
                       auto-mode-alist))
