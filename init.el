;; Emacs config file, Robert Jarzmik, oct. 1996

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(auto-save-file-name-transforms (quote (("\\`/[^/]*:\\(.+/\\)*\\(.*\\)" "/tmp/\\2") ("\\(.+/\\)*\\(.*\\)" "/tmp/\\2"))))
 '(auto-save-list-file-prefix "~/.emacs.d/data/auto-backup/auto-save-list/.saves-")
 '(backup-directory-alist (quote ((".*" . "~/.emacs.d/data/auto-backup/backup/"))))
 '(bbdb-file "~/.emacs.d/bbdb")
 '(c-basic-offset (quote set-from-style))
 '(c-default-style nil)
 '(c-indent-comments-syntactically-p t)
 '(c-tab-always-indent nil)
 '(column-number-mode t)
 '(compilation-window-height 10)
 '(delete-auto-save-files nil)
 '(display-time-24hr-format t)
 '(display-time-day-and-date t)
 '(display-time-mail-file "/var/spool/mail/jarzmik")
 '(doxymacs-doxygen-style "Kernel")
 '(ecb-options-version "2.32")
 '(ediff-merge-split-window-function (quote split-window-vertically))
 '(ediff-split-window-function (quote split-window-horizontally))
 '(ediff-window-setup-function (quote ediff-setup-windows-plain))
 '(eudc-strict-return-matches nil)
 '(explicit-shell-file-name "/bin/bash")
 '(fill-column 80)
 '(font-lock-global-modes t)
 '(fortune-dir "/usr/local/share/games/fortunes")
 '(fortune-file "/usr/local/share/games/fortunes/chucknorris")
 '(global-font-lock-mode t nil (font-lock))
 '(gnus-home-directory "~/.emacs.d/gnus/")
 '(gnus-list-groups-with-ticked-articles nil)
 '(gnus-save-score t)
 '(hexl-iso "-iso")
 '(ibuffer-saved-filter-groups nil)
 '(ibuffer-saved-filters (quote (("logs" ((name . "\\*.*"))) ("gnus" ((or (mode . message-mode) (mode . mail-mode) (mode . gnus-group-mode) (mode . gnus-summary-mode) (mode . gnus-article-mode)))) ("programming" ((or (mode . emacs-lisp-mode) (mode . cperl-mode) (mode . c-mode) (mode . java-mode) (mode . idl-mode) (mode . lisp-mode)))))))
 '(inhibit-startup-screen t)
 '(ispell-highlight-face (quote highlight))
 '(ispell-personal-dictionary nil)
 '(ispell-silently-savep t)
 '(jde-global-classpath (quote ("/usr/java/jdk1.3.1_01/connector.jar" "/usr/java/jdk1.3.1_01/src.jar" "/usr/java/jdk1.3.1_01/jta.jar" "/usr/java/jdk1.3.1_01/jaas.jar" "~rj/Basp/src/basp/adapter/ava/cci" "~rj/Basp/src/basp/adapter/ava/spi")))
 '(kill-whole-line nil)
 '(ldap-host-parameters-alist (quote (("aww.tlse.titn.alcatel.fr" base "o=tlse.titn.alcatel.fr"))))
 '(line-number-mode t)
 '(load-home-init-file t t)
 '(message-directory "~/.emacs.d/gnus/Mail/")
 '(message-send-mail-function (quote smtpmail-send-it))
 '(mew-imap-auth t)
 '(mew-imap-delete nil)
 '(mew-imap-server "imap.free.fr")
 '(mew-imap-user "robert.jarzmik")
 '(next-line-add-newlines nil)
 '(rmail-display-summary t)
 '(rmail-enable-mime t)
 '(rmail-redisplay-summary t)
 '(rmail-summary-window-size 10)
 '(send-mail-function (quote smtpmail-send-it))
 '(show-paren-mode t nil (paren))
 '(show-trailing-whitespace nil)
 '(smtpmail-smtp-server "smtp.free.fr")
 '(standard-indent 8)
 '(transient-mark-mode t)
 '(user-mail-address "robert.jarzmik@free.fr")
 '(visible-bell t))

(display-time)

;; Le path des fichiers .el et .elc
(when (not (boundp 'user-emacs-directory))
  (setq 'user-emacs-directory "~/.emacs.d/"))
(add-to-list 'load-path user-emacs-directory)
(add-to-list 'load-path (concat user-emacs-directory "site-elisp"))
(add-to-list 'load-path (concat user-emacs-directory "site-elisp/cedet-1.0/common"))
(add-to-list 'load-path (concat user-emacs-directory "site-elisp/ecb-2.40"))
(add-to-list 'load-path (concat user-emacs-directory "elisp"))

;; Fonts
(modify-frame-parameters nil '((wait-for-wm . nil)))

(defcustom package-to-load '(activity egg w3m)
  "List of my prefered packages"
  :group 'local
)

(dolist (package package-to-load)
  (when (not (require package nil t))
    (message (concat (symbol-name package) " package is not available"))))

(defcustom config-to-load
  '(my-buffers my-command-line my-dired my-emms my-git my-lang my-w3m my-keyboard
	       my-work)
  "List of my configurations"
  :group 'local
)

(dolist (config config-to-load)
  (when (not (require config nil t))
    (message (concat (symbol-name config) " configuration is not available"))))

; Start maximized
(if (not (eq window-system nil))
    (progn
  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
			  '(2 "_NET_WM_STATE_MAXIMIZED_VERT" 0))
  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
			  '(2 "_NET_WM_STATE_MAXIMIZED_HORZ" 0))
  ))

;;; Local Variables:
;;; eval: (defun byte-compile-this-file () (write-region (point-min) (point-max) buffer-file-name nil 't) (byte-compile-file buffer-file-name) nil)
;;; write-file-hooks: (byte-compile-this-file)
;;; End:
