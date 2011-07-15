;;; Robert Jarzmik
;;; 2008-03-28
;;; Global buffers and destop functions

;; save a list of open files in ~/.emacs.desktop
;; save the desktop file automatically if it already exists
(if (or (<= (length command-line-args) 1)
	(equal (car (cdr command-line-args)) "--no-splash"))
    (progn (desktop-save-mode 1))
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

;; Faces
;(custom-set-faces
;  ;; custom-set-faces was added by Custom.
;  ;; If you edit it by hand, you could mess it up, so be careful.
;  ;; Your init file should contain only one such instance.
;  ;; If there is more than one, they won't work right.
; '(highlight-beyond-fill-column-face ((t (:background "red")))))

;; Activity modeline
(when (featurep 'activity)
  (toggle-activity-mode-line))

;; Iswitchb-mode
(require 'edmacro)
(iswitchb-mode t)
(when (featurep 'iswitchb)
  (defun iswitchb-local-keys ()
    (mapc (lambda (K)
	    (let* ((key (car K)) (fun (cdr K)))
	      (define-key iswitchb-mode-map (edmacro-parse-keys key) fun)))
	  '(("<right>" . iswitchb-next-match)
	    ("<left>"  . iswitchb-prev-match)
	    ("<up>"    . ignore             )
	    ("<down>"  . ignore             ))))
  (add-hook 'iswitchb-define-mode-map-hook 'iswitchb-local-keys))

;; Dired unique window
(when (featurep 'dired)
  (add-hook 'dired-after-readin-hook
	    (lambda () (rename-buffer (generate-new-buffer-name dired-directory))))
  (setq dired-recursive-deletes 'always))

(provide 'my-buffers)

;;; Local Variables:
;;; eval: (defun byte-compile-this-file () (write-region (point-min) (point-max) buffer-file-name nil 't) (byte-compile-file buffer-file-name) nil)
;;; write-file-hooks: (byte-compile-this-file)
;;; End:
