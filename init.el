;; Emacs config file, Robert Jarzmik, oct. 1996

(require 'cl)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auth-sources
   '("~/.authinfo" "~/.authinfo.gpg" "~/.netrc" "~/.emacs.d/data/authinfo.gpg"))
 '(auto-save-file-name-transforms
   '(("\\`/[^/]*:\\(.+/\\)*\\(.*\\)" "/tmp/\\2")
     ("\\(.+/\\)*\\(.*\\)" "/tmp/\\2")))
 '(auto-save-list-file-prefix "~/.emacs.d/data/auto-backup/auto-save-list/.saves-")
 '(backup-directory-alist '((".*" . "~/.emacs.d/data/auto-backup/backup/")))
 '(battery-mode-line-format "[%b%p%%,%w,%t]")
 '(bbdb-file "~/.emacs.d/bbdb" t)
 '(c-basic-offset 'set-from-style)
 '(c-default-style nil)
 '(c-indent-comments-syntactically-p t)
 '(c-tab-always-indent nil)
 '(column-number-mode t)
 '(delete-auto-save-files nil)
 '(display-time-24hr-format t)
 '(display-time-mail-file "/var/spool/mail/jarzmik")
 '(doxymacs-doxygen-style "Kernel")
 '(ecb-options-version "2.32")
 '(ediff-merge-split-window-function 'split-window-vertically)
 '(ediff-split-window-function 'split-window-horizontally)
 '(ediff-window-setup-function 'ediff-setup-windows-plain)
 '(eudc-strict-return-matches nil)
 '(explicit-shell-file-name "/bin/bash")
 '(fill-column 80)
 '(font-lock-global-modes t)
 '(fortune-dir "/usr/local/share/games/fortunes")
 '(fortune-file "/usr/local/share/games/fortunes/chucknorris")
 '(global-font-lock-mode t nil (font-lock))
 '(gnus-gcc-mark-as-read t)
 '(gnus-home-directory "~/.emacs.d/gnus/")
 '(gnus-list-groups-with-ticked-articles nil)
 '(gnus-save-score t)
 '(hexl-iso "-iso")
 '(ibuffer-saved-filter-groups nil)
 '(ibuffer-saved-filters
   '(("logs"
      ((name . "\\*.*")))
     ("gnus"
      ((or
	(mode . message-mode)
	(mode . mail-mode)
	(mode . gnus-group-mode)
	(mode . gnus-summary-mode)
	(mode . gnus-article-mode))))
     ("programming"
      ((or
	(mode . emacs-lisp-mode)
	(mode . cperl-mode)
	(mode . c-mode)
	(mode . java-mode)
	(mode . idl-mode)
	(mode . lisp-mode))))))
 '(ido-save-directory-list-file "~/.emacs.d/data/ido.last")
 '(inhibit-startup-screen t)
 '(ispell-highlight-face 'highlight)
 '(ispell-personal-dictionary nil)
 '(ispell-silently-savep t)
 '(jde-global-classpath
   '("/usr/java/jdk1.3.1_01/connector.jar" "/usr/java/jdk1.3.1_01/src.jar" "/usr/java/jdk1.3.1_01/jta.jar" "/usr/java/jdk1.3.1_01/jaas.jar" "~rj/Basp/src/basp/adapter/ava/cci" "~rj/Basp/src/basp/adapter/ava/spi"))
 '(kill-whole-line nil)
 '(ldap-host-parameters-alist
   '(("aww.tlse.titn.alcatel.fr" base "o=tlse.titn.alcatel.fr")))
 '(line-number-mode t)
 '(load-home-init-file t t)
 '(magit-commit-signoff t)
 '(magit-log-cutoff-length 30)
 '(magit-process-popup-time 2)
 '(mediawiki-site-alist
   '(("Wikipedia" "http://en.wikipedia.org/w/" "username" "password" "Main Page")
     ("belgarath" "http://belgarath.local/mediawiki/" "rjarzmik" "XXX" "Main Page")))
 '(menu-bar-mode t)
 '(message-directory "~/.emacs.d/gnus/Mail/")
 '(message-send-mail-function 'smtpmail-send-it)
 '(next-line-add-newlines nil)
 '(org-log-done 'time)
 '(package-archives
   '(("gnu" . "http://elpa.gnu.org/packages/")
     ("melpa" . "https://melpa.org/packages/")))
 '(package-selected-packages
   '(persp-projectile helm-projectile perspective helm-ag dap-mode company lsp-java bbdb exec-path-from-shell toml-mode yasnippet-snippets yasnippet rust-playground flycheck git-commit rustic rutils lsp-ui helm-lsp lsp-mode plantuml-mode json-mode diminish-buffer which-key use-package sbt-mode scala-mode tabbar session pod-mode muttrc-mode mutt-alias markdown-mode magit initsplit htmlize graphviz-dot-mode folding eproject diminish csv-mode browse-kill-ring boxquote bm bar-cursor apache-mode scala-mode))
 '(sh-basic-offset 8)
 '(show-paren-mode t nil (paren))
 '(show-trailing-whitespace nil)
 '(standard-indent 8)
 '(status-refresh-timer-delay 10)
 '(tramp-persistency-file-name "/home/rj/.emacs.d/data/tramp")
 '(transient-mark-mode t)
 '(user-mail-address "robert.jarzmik@free.fr")
 '(visible-bell t))

(server-mode)

;; Le path des fichiers .el et .elc
;;emacs24 (when (not (boundp 'user-emacs-directory))
;;emacs24  (setq 'user-emacs-directory "~/.emacs.d/"))

;; Ajouter .emacs.d, .emacs.d/site-lisp et tous ses sous-repertoires
(dolist
    (path
     (delete-if-not 'file-directory-p
		    (directory-files (concat user-emacs-directory "site-elisp") t)))
  (add-to-list 'load-path path))
;(add-to-list 'load-path (concat user-emacs-directory "site-elisp/cedet-1.0/common"))
(add-to-list 'load-path (concat user-emacs-directory "site-elisp/gnus/lisp"))
(add-to-list 'load-path (concat user-emacs-directory "site-elisp/muse/lisp"))
(add-to-list 'load-path (concat user-emacs-directory "site-elisp/bbdb/lisp"))
(add-to-list 'load-path (concat user-emacs-directory "elisp"))

;; Fonts
(modify-frame-parameters nil '((wait-for-wm . nil)))

(defcustom package-to-load '(activity easymenu gnus-load magit git-commit-mode
				      muse-mode muse-html muse-publish
				      status zoom-frm)
  "List of my prefered packages"
  :group 'local
)

(package-initialize)

(dolist (package package-to-load)
  (when (not (require package nil t))
    (message (concat (symbol-name package) " package is not available"))))

; Host specific configurations
(dolist (my-host-el
	 (file-expand-wildcards
	  (format   "%selisp/my-host-%s*.el" user-emacs-directory (system-name))))
  (load my-host-el))

(defcustom config-to-load
  '(my-buffers my-command-line my-dired my-emms my-git my-lang my-mioa701 my-macos
	       my-mu4e my-muse my-pxa my-w3m my-work my-keyboard my-shells)
  "List of my configurations"
  :group 'local
)

(dolist (config config-to-load)
  (when (not (require config nil t))
    (message (concat (symbol-name config) " configuration is not available"))))

;; Start maximized
;;(if (not (eq window-system nil))
;;    (progn
;;  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
;;			  '(2 "_NET_WM_STATE_MAXIMIZED_VERT" 0))
;;  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
;;			  '(2 "_NET_WM_STATE_MAXIMIZED_HORZ" 0))
;;  ))

(defface gnus-summary-root '((t (:height 0.8)))
  "Face of common attributes for all summary faces in gnus summary."
  :group 'gnus-summary)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(gnus-header-subject ((((class color) (background light)) (:background "dark orange" :foreground "red4"))))
 '(gnus-summary-cancelled ((t (:inherit gnus-summary-root :background "black" :foreground "yellow"))))
 '(gnus-summary-high-ancient ((t (:foreground "RoyalBlue" :weight bold :inherit gnus-summary-root))))
 '(gnus-summary-high-read ((t (:foreground "DarkGreen" :weight bold :inherit gnus-summary-root))))
 '(gnus-summary-high-ticked ((t (:foreground "firebrick" :weight bold :inherit gnus-summary-root))))
 '(gnus-summary-high-unread ((t (:inherit gnus-summary-root :weight bold))))
 '(gnus-summary-low-ancient ((t (:inherit gnus-summary-root :foreground "RoyalBlue" :slant italic))))
 '(gnus-summary-low-read ((t (:inherit gnus-summary-root :foreground "DarkGreen" :slant italic))))
 '(gnus-summary-low-undownloaded ((t (:inherit gnus-summary-root :foreground "cyan4" :slant italic :weight normal))))
 '(gnus-summary-low-unread ((t (:inherit gnus-summary-root :slant italic))))
 '(gnus-summary-normal-ancient ((t (:foreground "RoyalBlue" :inherit gnus-summary-root))))
 '(gnus-summary-normal-read ((t (:foreground "DarkGreen" :inherit gnus-summary-root))))
 '(gnus-summary-normal-ticked ((t (:foreground "firebrick" :inherit gnus-summary-root))))
 '(gnus-summary-normal-unread ((t (:inherit gnus-summary-root))))
 '(gnus-summary-selected ((t (:underline t))))
 '(highlight-changes ((((class color) (background light)) (:background "antique white" :foreground nil))))
 '(highlight-changes-delete ((((class color) (background light)) (:background "antique white" :foreground nil)))))

;; BUG Workaround : melpa package installation errors (emacs <= 26.3)
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
