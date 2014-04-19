;;; Update strings such as "my-isp.net" to yours.

;(require 'gnus-load)
(require 'mbsync)
(require 'cl)

(custom-set-variables
 '(gnus-home-directory "~/.emacs.d/gnus/")
 '(gnus-list-groups-with-ticked-articles nil)
 '(gnus-save-score t)
 '(gnus-gcc-mark-as-read t)
)

(defun internet-available-p ()
  (eq (call-process "ping" nil nil t "-W" "5" "-c" "1" "google.fr") 0))

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
;;; Find the dovecot-imap executable
(setq dovecot-imap
      (car (remove-if-not 'file-exists-p
			  '("/usr/local/libexec/dovecot/imap"
			    "/usr/libexec/dovecot-imap"
			    "/usr/lib/dovecot/imap" ))))

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

;	(nnimap "gmail"
;		  (nnimap-address "imap.gmail.com")
;		  (nnimap-server-port 993)
;		  (nnimap-stream ssl)
;		  (imap-username "robert.jarzmik@gmail.com")
;		  (nnimap-authinfo-file "~/.emacs.d/gnus/.imap-authinfo")
;		)
	))

;; Configure out-going mail, through free.fr, without a local MTA
(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-smtp-server "smtp.orange.fr"
      sendmail-program "/bin/false")

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

;; Show the threads sorted by most recent received message
;(setq gnus-thread-sort-functions 'gnus-thread-sort-by-most-recent-date)

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
	  "nnimap+free-mbsync:INBOX/Sent")))

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

;; Color customizations
(defface gnus-summary-tremendously-interesting '((t (:background "red")))
  "Face used for displaying high score articles."
  :group 'gnus-summary-emphasis)

(defface gnus-summary-very-interesting '((t (:background "orange")))
  "Face used for displaying high score articles."
  :group 'gnus-summary-emphasis)

(defface gnus-summary-interesting '((t (:background "yellow")))
  "Face used for displaying high score articles."
  :group 'gnus-summary-emphasis)

(setq gnus-summary-highlight
      (quote (
	      ((eq mark gnus-canceled-mark)
	       . gnus-summary-cancelled)
	      ((and uncached (> score default-high))
	       . gnus-summary-high-undownloaded)
	      ((and uncached (< score default-low))
	       . gnus-summary-low-undownloaded)
	      (uncached
	       . gnus-summary-normal-undownloaded)
	      ((and (> score default-high)
		    (or (eq mark gnus-dormant-mark)
			(eq mark gnus-ticked-mark)))
	       . gnus-summary-high-ticked)
	      ((and (< score default-low)
		    (or (eq mark gnus-dormant-mark)
			(eq mark gnus-ticked-mark)))
	       . gnus-summary-low-ticked)
	      ((or (eq mark gnus-dormant-mark)
		   (eq mark gnus-ticked-mark))
	       . gnus-summary-normal-ticked)

	      ;; RJK special colors
	      ((and (>= score 200) (eq mark gnus-unread-mark)) . gnus-summary-tremendously-interesting)
	      ((and (>= score 100) (eq mark gnus-unread-mark)) . gnus-summary-very-interesting)
	      ((and (>= score 30) (eq mark gnus-unread-mark)) . gnus-summary-interesting)

	      ((and (> score default-high) (eq mark gnus-ancient-mark))
	       . gnus-summary-high-ancient)
	      ((and (< score default-low) (eq mark gnus-ancient-mark))
	       . gnus-summary-low-ancient)
	      ((eq mark gnus-ancient-mark)
	       . gnus-summary-normal-ancient)
	      ((and (> score default-high) (eq mark gnus-unread-mark))
	       . gnus-summary-high-unread)
	      ((and (< score default-low) (eq mark gnus-unread-mark))
	       . gnus-summary-low-unread)
	      ((eq mark gnus-unread-mark)
	       . gnus-summary-normal-unread)
	      ((> score default-high)
	       . gnus-summary-high-read)
	      ((< score default-low)
	       . gnus-summary-low-read)
	      (t
	       . gnus-summary-normal-read))))

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
