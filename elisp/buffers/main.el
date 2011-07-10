;;; Robert Jarzmik
;;; 2008-03-28
;;; Global buffers and destop functions

;; save a list of open files in ~/.emacs.desktop
;; save the desktop file automatically if it already exists
(if (or (<= (length command-line-args) 1)
	(equal (car (cdr command-line-args)) "--no-splash"))
    (progn (desktop-read)
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

;;; Font globals
(set-language-environment 'utf-8)
;(cond ((eq system-type 'windows-nt)
;       (set-frame-font "-outline-Arial monospaced for SAP-normal-normal-normal-mono-*-*-*-*-c-*-iso10646-1")
;       (set-face-attribute 'default nil :height 80))
;      ((eq system-type 'gnu/linux)
;       (set-frame-font "-unknown-DejaVu Sans Mono-normal-normal-normal-*-10-*-*-*-m-0-iso10646-1")))

(blink-cursor-mode t)
(global-font-lock-mode t)
(global-hl-line-mode t)
(show-paren-mode t)
(tool-bar-mode nil)

(setq inhibit-startup-message t
      inhibit-startup-echo-area-message t
      initial-scratch-message nil
      visible-bell t)

; Maximum de decoration
(setq font-lock-maximum-decoration t)

;;; Activity
(load "buffers/activity")
;(require 'activity)

(when (featurep 'activity)
  (toggle-activity-mode-line)
  (global-set-key (kbd "C-` `") 'activity-pop)
  (global-set-key (kbd "C-` d") (lambda () (interactive)
				  (toggle-activity "default"))))

;;; Local Variables:
;;; eval: (defun byte-compile-this-file () (write-region (point-min) (point-max) buffer-file-name nil 't) (byte-compile-file buffer-file-name) nil)
;;; write-file-hooks: (byte-compile-this-file)
;;; End:
