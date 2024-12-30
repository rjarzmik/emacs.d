;; The package mu4e is not yet available in ELPA
;; (use-package mu4e
;;   :ensure t
;;   )
;; It should be provided locally with mu4e

(when (featurep 'mu4e)
					; get mail
  (setq mu4e-get-mail-command "mbsync -a"
	;; mu4e-html2text-command "w3m -T text/html" ;;using the default mu4e-shr2text
	mu4e-view-prefer-html t
	mu4e-update-interval 180
	mu4e-headers-auto-update t
	mu4e-compose-signature-auto-include nil
	mu4e-compose-format-flowed t)

  ;; to view selected message in the browser, no signin, just html mail
  (add-to-list 'mu4e-view-actions
	       '("ViewInBrowser" . mu4e-action-view-in-browser) t)

  ;; enable inline images
  (setq mu4e-view-show-images t)
  ;; use imagemagick, if available
  (when (fboundp 'imagemagick-register-types)
    (imagemagick-register-types))

  ;; every new email composition doesn't get its own frame!
  (setq mu4e-compose-in-new-frame nil)

  ;; don't save message to Sent Messages, IMAP takes care of this
  (setq mu4e-sent-messages-behavior 'delete)

  (add-hook 'mu4e-view-mode-hook #'visual-line-mode)

  ;; <tab> to navigate to links, <RET> to open them in browser
  (add-hook 'mu4e-view-mode-hook
	    (lambda()
	      ;; try to emulate some of the eww key-bindings
	      (local-set-key (kbd "<RET>") 'mu4e~view-browse-url-from-binding)
	      (local-set-key (kbd "<tab>") 'shr-next-link)
	      (local-set-key (kbd "<backtab>") 'shr-previous-link)))

  ;; from https://www.reddit.com/r/emacs/comments/bfsck6/mu4e_for_dummies/elgoumx
  (add-hook 'mu4e-headers-mode-hook
	    (defun my/mu4e-change-headers ()
	      (interactive)
	      (setq mu4e-headers-fields
		    `((:human-date . 25) ;; alternatively, use :date
		      (:flags . 6)
		      (:from . 22)
		      (:thread-subject . ,(- (window-body-width) 70)) ;; alternatively, use :subject
		      (:size . 7)))))

  ;; if you use date instead of human-date in the above, use this setting
  ;; give me ISO(ish) format date-time stamps in the header list
					;(setq mu4e-headers-date-format "%Y-%m-%d %H:%M")
  ;; spell check
  (add-hook 'mu4e-compose-mode-hook
	    (defun my-do-compose-stuff ()
	      "My settings for message composition."
	      (visual-line-mode)
	      (flyspell-mode)))

  ;; use mu4e for e-mail in emacs
  (setq mail-user-agent 'mu4e-user-agent)

  ;; don't save message to Sent Messages, Gmail/IMAP takes care of this
  (setq mu4e-sent-messages-behavior 'delete)

  ;; (See the documentation for `mu4e-sent-messages-behavior' if you have
  ;; additional non-Gmail addresses and want assign them different
  ;; behavior.)

  ;; setup some handy shortcuts
  ;; you can quickly switch to your Inbox -- press ``ji''
  ;; then, when you want archive some messages, move them to
  ;; the 'All Mail' folder by pressing ``ma''.

  ;;rename files when moving
  ;;NEEDED FOR MBSYNC
  (setq mu4e-change-filenames-when-moving t)

  ;;from the info manual
  (setq mu4e-attachment-dir  "~/Downloads")

  (setq message-kill-buffer-on-exit t)

  ;; show full addresses in view message (instead of just names)
  ;; toggle per name with M-RET
  (setq mu4e-view-show-addresses 't)

  ;; don't ask when quitting
  (setq mu4e-confirm-quit nil)

  ;; sending mail
  (setq smtpmail-queue-mail nil  ;; start in normal mode
	smtpmail-queue-dir   "~/Maildir/queue/cur")

  ;; mu4e-context
  (setq mu4e-context-policy 'pick-first)
  (setq mu4e-compose-context-policy 'always-ask)
  (setq mu4e-contexts
	(list
	 (make-mu4e-context
	  :name "free"
	  :enter-func (lambda () (mu4e-message "Entering context free"))
	  :leave-func (lambda () (mu4e-message "Leaving context free"))
	  :match-func (lambda (msg)
			(when msg
			  (string-match-p "^/free" (mu4e-message-field msg :maildir))))
	  :vars '((user-mail-address . "robert.jarzmik@free.fr")
		  (user-full-name . "Robert Jarzmik")
		  (mu4e-sent-folder . "/free/Sent")
		  (mu4e-drafts-folder . "/free/Drafts")
		  (mu4e-trash-folder . "/free/Trash")
		  (mu4e-compose-signature . (concat "Formal Signature\n" "Emacs 25, org-mode 9, mu4e 1.0\n"))
		  (mu4e-compose-format-flowed . t)
		  (message-send-mail-function . smtpmail-send-it)
		  (smtpmail-servers-requiring-authorization . ".*")
		  (smtpmail-smtp-user . "robert.jarzmik@free.fr")
		  (smtpmail-starttls-credentials . (("smtp.free.fr" 587 nil nil)))
		  (smtpmail-auth-credentials . (expand-file-name "~/.authinfo"))
		  (smtpmail-default-smtp-server . "smtp.free.fr")
		  (smtpmail-smtp-server . "smtp.free.fr")
		  (smtpmail-smtp-service . 587)
		  (smtpmail-debug-info . t)
		  (smtpmail-debug-verbose . t)
		  (mu4e-maildir-shortcuts . ( ("/free/INBOX/incoming" . ?i)
					      ("/free/Sent"   . ?s)
					      ("/free/Trash"  . ?t)
					      ("/free/Drafts" . ?d)
					      ))))))

  )

(provide 'my-mu4e)
