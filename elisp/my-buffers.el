;;; Robert Jarzmik
;;; 2008-03-28
;;; Global buffers and destop functions

(eval-when-compile
  (require 'iswitchb)
  (require 'cl))

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
(tool-bar-mode 0)
(winner-mode)
(use-package helm
  :ensure t
  :bind
  (("C-x C-f" . helm-find-files)
   ("C-x b" . helm-buffers-list)
   ("M-x" . helm-M-x))
  :config
  (helm-mode t))
(when (featurep 'status)
  (turn-on-status))
(load-theme 'tsdh-light)

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
(when (featurep 'iswitchb)
  (defun iswitchb-local-keys ()
    (mapc (lambda (K)
	    (let* ((key (car K)) (fun (cdr K)))
	      (define-key iswitchb-mode-map (edmacro-parse-keys key) fun)))
	  '(("<right>" . iswitchb-next-match)
	    ("<left>"  . iswitchb-prev-match)
	    ("<up>"    . ignore             )
	    ("<down>"  . iswitchb-toggle-only-current-activity))))
  (add-hook 'iswitchb-define-mode-map-hook 'iswitchb-local-keys)

  ;; By default ignore from list buffers with a star
  ;;(setq iswitchb-buffer-ignore '("^ " "^*"))
  (defvar iswitchb-toggle-only-current-activity nil
    "If t, only buffer of current activity are displayed")

  (defun iswitchb-current-activity ()
    "Filter out any buffer not in the current activity"
    (if iswitchb-toggle-only-current-activity
	(setq iswitchb-temp-buflist
	      (delete-if-not 'activity-buffer-p iswitchb-temp-buflist))))

  (defun iswitchb-toggle-only-current-activity ()
    "Retain only activity buffers."
    (interactive)
    (setq iswitchb-toggle-only-current-activity (not iswitchb-toggle-only-current-activity))
    (if iswitchb-toggle-only-current-activity
	(add-hook 'iswitchb-make-buflist-hook 'iswitchb-current-activity)
      (remove-hook 'iswitchb-make-buflist-hook 'iswitchb-current-activity))
    (iswitchb-make-buflist iswitchb-default)
    ;; ask for list to be regenerated.
    (setq iswitchb-rescan t)
    (delete-minibuffer-contents))

  (add-hook 'iswitchb-make-buflist-hook 'iswitchb-current-activity)
  )

;; Window's buffer stickiness
(defun window-toggle-stickiness ()
  "Toggle between the 3 dedication states : free, dedicated and
strongly dedicated. See `set-window-dedicated-p."
  (interactive)
  (let* ((dedicated (window-dedicated-p))
	 (new (cond ((not dedicated) 1)
		    ((eq dedicated 1) t)
		    (t nil))))
    (set-window-dedicated-p (get-buffer-window (current-buffer)) new)))

;; Tramp: use remote host environment
(require 'tramp)
(add-to-list 'tramp-remote-path 'tramp-own-remote-path)

(add-to-list
 'global-mode-string
 '(:eval (assoc-default (window-dedicated-p) '((t . " S") (1 . " s")))) t)

;; Which-key: completion of possible key-bindings
(use-package which-key
  :ensure t
  :diminish
  :config
  (which-key-mode))

;; Workspaces with perspective
(use-package perspective
  :ensure t
  :bind
  ("C-x C-b" . persp-list-buffers)
  :custom
  ((persp-mode-prefix-key (kbd "C-c C-p"))
   (persp-state-default-file (file-name-concat user-emacs-directory "data/persist.backup")))
  :config
  (persp-mode -1)) ; By default, disable it. User should enable it manually.

(use-package persp-projectile
  :after
  projectile
  )

;; Day-mode and Night-mode
(defun night-mode ()
  "Toggle the color theme to a dark mode, suitable for night usage"
  (interactive)
  (load-theme 'tsdh-dark))

(defun day-mode ()
  "Toggle the color theme to a light mode, suitable for daylight usage"
  (interactive)
  (load-theme 'tsdh-light))

(provide 'my-buffers)
