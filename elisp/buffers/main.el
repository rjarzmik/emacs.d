;;; Robert Jarzmik
;;; 2008-03-28
;;; Global buffers and destop functions

;;; Save desktop
(load "buffers/desktop")

;; save a list of open files in ~/.emacs.desktop
;; save the desktop file automatically if it already exists
(if (or (<= (length command-line-args) 1)
	(equal (car (cdr command-line-args)) "--no-splash"))
    (progn (desktop-load-default)
	   (desktop-read)
	   (setq desktop-save 'if-exists)
	   (desktop-save-mode 1))
  )

;; save a bunch of variables to the desktop file
;; for lists specify the len of the maximal saved data also
(setq desktop-globals-to-save
      (append '((extended-command-history . 30)
                (file-name-history        . 100)
                (grep-history             . 30)
                (compile-history          . 30)
                (minibuffer-history       . 50)
                (query-replace-history    . 60)
                (read-expression-history  . 60)
                (regexp-history           . 60)
                (regexp-search-ring       . 20)
                (search-ring              . 20)
                (shell-command-history    . 50)
                tags-file-name
                register-alist)))

;;; Cycle buffers
(load "buffers/cycle-buffer")
(autoload 'cycle-buffer "cycle-buffer" "Cycle forward." t)
(autoload 'cycle-buffer-backward "cycle-buffer" "Cycle backward." t)
(global-set-key [\C-tab] 'cycle-buffer-backward)

;;; Whitespace visualization
(load "buffers/whitespace")

;;; Local Variables:
;;; eval: (defun byte-compile-this-file () (write-region (point-min) (point-max) buffer-file-name nil 't) (byte-compile-file buffer-file-name) nil)
;;; write-file-hooks: (byte-compile-this-file)
;;; End:
