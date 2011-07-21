;;; Update strings such as "my-isp.net" to yours.

(require 'gnus-load)
(require 'offlineimap)

(defun internet-available-p ()
  (eq (call-process "ping" nil nil t "-W" "5" "-c" "1" "google.fr") 0))

(when (require 'gnus-demon nil t)
  (defun gnus-demon-scan-mail-and-news ()
    "Scan for new mail/news and update the *Group* buffer."
    (when (and (gnus-alive-p) (internet-available-p))
      (when (string= "arbois" (system-name))
	(offlineimap))
      (save-window-excursion
	(set-buffer gnus-group-buffer)
	(gnus-group-get-new-news))))

  (add-hook 'gnus-group-mode-hook 'gnus-demon-init)
  (gnus-demon-add-handler 'gnus-demon-scan-mail-and-news 5 t)
  (require 'gnus-notify nil t))

(when (string= "arbois" (system-name))
  (add-hook 'gnus-before-startup-hook '(lambda () (when (internet-available-p) (offlineimap)))))

(setq gnus-select-method '(nnnil ""))
(setq gnus-secondary-select-methods
      '(
;	(nnimap "belgarath"
;		(nnimap-address "belgarath.local")
;		(nnimap-server-port 993)
;		(nnimap-stream ssl)
;		(nnimap-authinfo-file "~/.emacs.d/gnus/.imap-authinfo")
;		)
;	(nnimap "free"
;		(nnimap-stream network)
;		(nnimap-address "imap.free.fr")
;		(nnimap-server-port 143)
;		(imap-username "robert.jarzmik")
;		(remove-prefix "INBOX.")
;		(nnimap-list-pattern ("INBOX*" "ARCHIVES/*" "Mail/*" "ml*"))
;		(nnimap-authinfo-file "~/.emacs.d/gnus/.imap-authinfo")
;		)
	(nnimap "free-offline"
		  (nnimap-stream shell)
		  (imap-shell-program "/usr/libexec/dovecot/imap -c ~/.offlineimap/dovecot.conf")
		  (nnir-search-engine imap))
;	(nnimap "gmail"
;		  (nnimap-address "imap.gmail.com")
;		  (nnimap-server-port 993)
;		  (nnimap-stream ssl)
;		  (imap-username "robert.jarzmik@gmail.com")
;		  (nnimap-authinfo-file "~/.emacs.d/gnus/.imap-authinfo")
;		)
	))

; (setq nnimap-split-inbox '("INBOX"))

;; Fetch only part of the article if we can.  I saw this in someone
;; else's .gnus
(setq gnus-read-active-file 'some)

;; Tree view for groups.  I like the organisational feel this has.
(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)

;; Also, I prefer to see only the top level message.  If a message has
;; several replies or is part of a thread, only show the first
;; message.  'gnus-thread-ignore-subject' will ignore the subject and
;; look at 'In-Reply-To:' and 'References:' headers.
; (setq gnus-thread-hide-subtree t)
(setq gnus-thread-ignore-subject t)

;; Change email address for work folder.  This is one of the most
;; interesting features of Gnus.  I plan on adding custom .sigs soon
;; for different mailing lists.
(setq gnus-posting-styles
      '((".*"
         (name "Robert Jarzmik")
         ("X-URL" "http://belgarath.falguerolles.org/")
	 (signature "Robert"))
	("free"
         (address "robert.jarzmik@free.fr"))
	("gmail.com"
	 (address "robert.jarzmik@gmail.com"))))

; Archive outgoing messages, in one file per month
(setq gnus-message-archive-group t)
(setq gnus-message-archive-group
      '((if (message-news-p)
	    "misc-news"
	  "nnimap+free:INBOX/Sent")))

;; BBDB
;; Remember: in gnus, use ":" to add an entry
;(require 'bbdb)
;(bbdb-initialize 'gnus 'message)
;(add-hook 'gnus-startup-hook 'bbdb-insinuate-gnus)
;(add-hook 'gnus-startup-hook 'bbdb-insinuate-message)
;(setq bbdb-complete-name-allow-cycling t)
;(setq bbdb-use-pop-up nil)

;;; summary and article buffer
;;; ##########################

(setq gnus-visible-headers
      (mapconcat 'regexp-quote
                 '("From:" "Newsgroups:" "Subject:" "Date:"
                   "Organization:" "To:" "Cc:" "Followup-To" "Gnus-Warnings:"
                   "X-Sent:" "X-URL:" "User-Agent:" "X-Newsreader:"
                   "X-Mailer:" "Reply-To:" "X-Spam:" "X-Spam-Status:" "X-Now-Playing"
                   "X-Attachments" "X-Diagnostic" "Message-ID:")
                 "\\|"))

(setq gnus-group-line-format "%P  %(%~(pad-right 50)G%) %U/%y (unseen/unread)\n"
      gnus-summary-line-format "%U%R %d %*%5L%I %(%z%[%-23,23f%]%) %s\n"
      gnus-topic-line-format "%i[ %0{%(%n (new: %a)%)%} ]%v\n")

;; Wrap at 80 cols.
(add-hook 'message-mode-hook
	'(lambda()
		(turn-on-auto-fill)
		(setq fill-column 80)))

;; Window configurations
(gnus-add-configuration
           '(article
             (horizontal 1.0
                         (vertical 25 (group 1.0))
                         (vertical 1.0
                                   (summary 0.16 point)
                                   (article 1.0)))))
(gnus-add-configuration
 '(summary
   (horizontal 1.0
	       (vertical 25 (group 1.0))
	       (vertical 1.0 (summary 1.0 point)))))
(gnus-add-configuration
           '(reply
	     (vertical 1.0
		       (summary 0.16 point)
		       (horizontal 1.0
				   (article 0.5)
				   (message 1.0 point)))))
(gnus-add-configuration
           '(reply-yank
	     (vertical 1.0
		       (summary 0.16 point)
		       (horizontal 1.0
				   (article 0.5)
				   (message 1.0 point)))))


;;;--------------------------------------
;;; Deprecated used variables / functions
;;;--------------------------------------

; Old pop3 oriented mail splitting rules
;(setq nnimap-split-rule
;	'(("INBOX/ml-linux-alsa" "^\\(To\\|Cc\\):.*alsa-devel@alsa-project.org.*")
;	  ("INBOX/ml-linux-haret" "^\\(To\\|Cc\\):.*haret@handhelds.org.*")
;	  ("INBOX/ml-linux-arm" "^\\(To\\|Cc\\):.*linux-arm-kernel@lists.arm.linux.org.uk.*")
;	  ("INBOX/ml-linux-usb" "^\\(To\\|Cc\\):.*linux-usb@vger.kernel.org.*")
;	  ("INBOX/ml-linux-mtd" "^\\(To\\|Cc\\):.*linux-mtd@lists.infradead.org.*")
;	  ("INBOX/ml-linux-v4l" "^\\(To\\|Cc\\):.*video4linux-list@redhat.com.*")
;	  ("INBOX/ego" "^From:.*[rR]obert.[jJ]arzmik")
;	  ))

;;; Local Variables:
;;; eval: (defun byte-compile-this-file () (write-region (point-min) (point-max) buffer-file-name nil 't) (byte-compile-file buffer-file-name) nil)
;;; write-file-hooks: (byte-compile-this-file)
;;; End:
