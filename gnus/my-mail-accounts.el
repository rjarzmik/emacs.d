;;; Find the dovecot-imap executable
(setq dovecot-imap
      (car (remove-if-not 'file-exists-p
			  '("/usr/local/libexec/dovecot/imap"
			    "/usr/libexec/dovecot-imap"
			    "/usr/lib/dovecot/imap" ))))

(defun internet-available-p ()
  (eq (call-process "ping" nil nil t "-W" "5" "-c" "1" "google.fr") 0))

(defun intel-available-p ()
  (eq (call-process "ping" nil nil t "-W" "5" "-c" "1" "imapmail.intel.com") 0))

(setq gnus-select-method '(nnnil ""))
(setq gnus-secondary-select-methods
      (list
					;	(nnimap "free"
					;		(nnimap-stream network)
					;		(nnimap-address "imap.free.fr")
					;		(nnimap-server-port 143)
					;		(imap-username "robert.jarzmik")
					;		(remove-prefix "INBOX.")
					;		(nnimap-list-pattern ("INBOX*" "ARCHIVES*" "Mail/*" "ml*"))
					;		(nnimap-authinfo-file "~/.emacs.d/gnus/.imap-authinfo")
					;		)
       (list 'nnimap "free-mbsync"
	     '(nnimap-stream shell)
	     (list 'imap-shell-program (concat dovecot-imap " -c ~/Maildir/dovecot_mbsync.conf"))
	     '(nnir-search-engine imap))

;       (list 'nnimap "gmail"
;	     '(nnimap-address "imap.gmail.com")
;	     '(nnimap-server-port 993)
;	     '(nnimap-stream ssl)
;	     '(imap-username "robert.jarzmik"))

       (when (intel-available-p)
	 (list 'nnimap "intel"
	       '(nnimap-address "imapmail.intel.com")
	       '(nnimap-server-port 993)
	       '(nnimap-stream ssl)
	       '(imap-username "robert.jarzmik")))
       ))

;; Configure out-going mail, through free.fr, without a local MTA
(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-smtp-server "smtp.orange.fr"
      sendmail-program "/bin/false")

;; When group refresh is asked for, launch mbsync
(add-hook 'gnus-get-new-news-hook
	  '(lambda () (when (internet-available-p) (mbsync))))

;; When mbsync finishes, refresh unread articles in groups menu
(defun mbsync-event (process msg)
  (when (eq (process-status process) 'exit)
    (gnus-get-unread-articles)
    (gnus-group-list-groups)))
(add-hook 'mbsync-event-hooks 'mbsync-event)
					;
