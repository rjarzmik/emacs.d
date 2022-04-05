;;; My personnal C modes

(require 'highlight-beyond-fill-column)

(defun my-default-c-mode-hook ()
  "My default setup for C mode"
  (c-set-style "linux")
					; Dirty workaround for labels in column 0
  (remove-hook 'c-special-indent-hook 'c-gnu-impose-minimum)
  (highlight-beyond-fill-column)
  (setq show-trailing-whitespace t)
  ;;(define-key c++-mode-map "\M-/" 'semantic-ia-complete-symbol-menu)
)
(add-hook 'c-mode-hook 'my-default-c-mode-hook)

(defun my-common-c-mode-hook ()
  "My default setup for C and C++ mode"

  (define-key c-mode-map "\M-." 'my-find-tag)
  (define-key c-mode-map "\M-*" 'my-pop-tag-mark)
  (define-key c++-mode-map "\M-." 'my-find-tag)
  (define-key c++-mode-map "\M-*" 'my-pop-tag-mark)
  (highlight-changes-mode t)
  )
(add-hook 'c-mode-common-hook 'my-common-c-mode-hook)

(c-add-style
 "rjk-c++-style"
 '("gnu"
   (c-basic-offset . 4)
   (indent-tabs-mode . nil) ; use spaces, never tabs
   (c-comment-only-line-offset 0 . 0)
   (c-offsets-alist . ((label . 0)
		       (innamespace . -)
		       (namespace-open . 0)
		       (namespace-close . 0))))
 nil)

(defun my-common-c++-mode-hook()
  "My default setup for C and C++ mode"
  (c-set-style "rjk-c++-style")
  )
(add-hook 'c++-mode-hook 'my-common-c++-mode-hook)

(defun linux-c-mode ()
  "C mode with adjusted defaults for use with the Linux kernel."
  (interactive)
  (c-mode)
  (c-set-style "linux")
					; Dirty workaround for labels in column 0
  (remove-hook 'c-special-indent-hook 'c-gnu-impose-minimum)
  (highlight-beyond-fill-column)
  (setq show-trailing-whitespace t)
  ;;(require 'my-cedet)
  ;;(require 'semantic-ia)
  ;;(require 'ede-linux)
  ;;(ede-linux-load "~/mio_linux/kernel")
  ;;(ede-minor-mode 1)
  (highlight-changes-mode 1)
  ;;(define-key c++-mode-map "\M-/" 'semantic-ia-complete-symbol-menu)
  )
(setq auto-mode-alist (cons '("/home/rj/.*/kernel.*/.*\\.[ch]$" . linux-c-mode)
			    auto-mode-alist))

(defun kernel-smart-compile ()
  "Hook called when opening a file in a linux kernel directory."
  (let* ((tpath
	  (and (string-match "^\\(.*?/kernel/\\).*$" default-directory)
	       (match-string 1 default-directory)))
	 (path (or (and (featurep 'tramp) (tramp-tramp-file-p tpath)
			(with-parsed-tramp-file-name tpath d d-localname))
		   tpath)))
    (when path (format "make -C %s/out -j 8" path))))

(add-hook 'my-compile-command-hooks 'kernel-smart-compile)

(defun linux-filter-activity (buf)
    (let ((bufname (buffer-file-name (get-buffer buf))))
      (when bufname
	(string-match "linux/" bufname))))

(when (featurep 'activity)
  (add-to-list 'available-activities
	       (make-activity :name "Linux"
			      :buffer-filter-p 'linux-filter-activity)))

(defun clean-c-file ()
  "Cleaning blank and doubled lines"
  (interactive)
  (save-excursion
    (beginning-of-buffer)
    (save-excursion (replace-regexp " +$" ""))
    (save-excursion (replace-regexp " *; +$" ";"))
    (save-excursion (replace-regexp " +; *$" ";"))
    (save-excursion (replace-regexp "\\([a-zA-Z0-9]+\\),\\([a-zA-Z0-9]+\\)" "\\1, \\2"))
    (save-excursion (replace-regexp "\\( +\\(if\\|for\\|while\\|switch\\)\\)(" "\\1 ("))
    (save-excursion (replace-regexp "^\n+\\(.*\\)}" "\\1}"))
    (save-excursion (replace-regexp "^\\( +\\(const \\)?[a-zA-Z0-9_]+\\) +\\* +\\([A-Za-z0-9_]+\\)" "\\1 *\\3"))
    (save-excursion (replace-regexp "^\\( +case +[A-Za-z0-9_]+\\) +:$" "\\1:"))
    (save-excursion (replace-regexp "^[\t ]+$" ""))
    (save-excursion (replace-regexp "^\n\n+" "\n"))
    (save-excursion (replace-regexp "\t" "    "))
    (save-excursion (replace-regexp "{\n\n+" "{\n"))))

					; (add-hook 'c-mode-common-hook
					; 	  (lambda() (add-hook 'local-write-file-hooks
					;			      (lambda () (clean-c-file) nil))))

;; Compilation
(setq-default compilation-scroll-output t
              compilation-read-command nil
	      compilation-window-height 15
	      compilation-auto-jump-to-first-error nil
	      compilation-ask-about-save nil)

;; Tags handling
(defun rtags-db-available-p ()
    (when (featurep 'rscope)
      (rscope-find-cscope-process (current-buffer))))

(defun my-find-tag ()
  "Find tag definition using cscope or tags.
If cscope database file is found, it is used. Fallback on tags."
  (interactive)
  (progn
    (call-interactively
     (cond ((rtags-db-available-p) 'rscope-find-global-definition)
	   (t 'find-tag))
     )))

(defun my-pop-tag-mark ()
  "Pop back to where M-. was las invoked (see pop-tag-mark)"
  (interactive)
  (progn
    (call-interactively
     (cond ((rtags-db-available-p) 'rscope-pop-mark)
	   (t 'pop-tag-mark))
)))

(defun my-find-next-tag ()
  "Find next tag definition using cscope or tags.
If cscope database file is found, it is used. Fallback on tags."
  (interactive)
  (progn
    (let ((cscope-dir (cscope-find-info default-directory)))
      (if (file-exists-p (concat (caar cscope-dir) cscope-database-file))
	  (call-interactively 'cscope-next-symbol)
	(find-tag "" t)
	)
      )
    )
  )

;; GDB
(setq-default gdb-many-windows t)
(add-to-list 'auto-mode-alist '("\\.dbg$" . gdb-script-mode))
(add-to-list 'auto-mode-alist '("\\.gdb$" . gdb-script-mode))

;;; Local Variables:
;;; eval: (defun byte-compile-this-file () (write-region (point-min) (point-max) buffer-file-name nil 't) (byte-compile-file buffer-file-name) nil)
;;; write-file-hooks: (byte-compile-this-file)
;;; End:
