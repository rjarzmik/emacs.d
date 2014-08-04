;; Barebox utilities, Robert Jarzmik, jan. 2014
;;
;; Tools to remote control a device in "bootloader" mode, where the bootloader
;; is barebox.
;; The main commands are :
;:   mioa701-barebox-start() : starts the remote controller
;;   mioa701-barebox-upload-file(): uploads a file to the device
;;   mioa701-barebox-command() : executes a command on the device
;;
;; Internals:
;;   The commander relies on a shell script, barebox-commander.sh
;;   The commander is a finite state machine, controlled by :
;;     - barebox-status : its state
;;       - barebox-status-idle : waiting for a command (implies barebox queue
;;         was emptied)
;;       - barebox-status-wait-prompt : waiting for the barebox prompt to appear
;;         This can be either a first start and no prompt ever came in because
;;         the bootloader was never powered up, or because the last command
;;         finished on error.
;;       - barebox-status-busy : executing a command
;;         The commander sentinel will retrigger a barebox-queue pop
;;     - barebox-queue : the queue of action to be performed
;;
;; Special tramp considerations
;;   The mioa701-barebox-start() is sensitive to the tramp path of the current
;;   buffer.

(require 'tramp)

(defconst barebox-buffer-fmt "*barebox-ctrl:%s*")
(defvar barebox-upload-filename "")

(defvar barebox-queue)
(defvar barebox-timer)
(defvar barebox-status)
(defvar barebox-host)

;; C mode
(define-minor-mode mioa701-barebox
  "Mio A701 barebox development mode"
  nil "mio-b" nil
  (set (make-local-variable 'compile-command)
       (concat "cd " mioa701-bpath " && make -j 8")))

(add-hook 'c-mode-hook
	  (lambda () (when (and (buffer-file-name)
				(string-match mioa701-bpath (buffer-file-name)))
		       (mioa701-barebox))))

(defun barebox-commander-filter (proc output)
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (goto-char (point-max))
      (insert output)
;      (dolist (str (split-string output ""))
;	(kill-line 0)
;	(insert str))
      )))

(defun barebox-guess-host (host)
  "Calculates the host where barebox commander is/will be launched."
  (cond (host)
	((and (boundp 'barebox-host) barebox-host))
	((and (featurep 'tramp) (tramp-tramp-file-p default-directory))
	 (with-parsed-tramp-file-name default-directory guess guess-host))
	(t "localhost")))

(defun barebox-buffer (&optional host)
  "Returns the name of the barebox control buffer."
  (let ((h (barebox-guess-host host)))
    (format barebox-buffer-fmt h)))

(defun barebox-command-exit-ko ()
  (let ((status
	(cond
	 ((eq barebox-status 'barebox-status-wait-prompt) 'barebox-status-wait-prompt)
	 ((eq barebox-status 'barebox-status-idle) 'barebox-status-idle)
	 ((eq barebox-status 'barebox-status-busy) 'barebox-status-idle))))
    (insert "\t => launch finished on error\n")
    (message "Barebox command error")
    (setq barebox-status status)))

(defun barebox-command-exit-ok ()
  (insert "\t => finished successfully\n")
  (message "Barebox command success")
  (setq barebox-status 'barebox-status-idle))
  
(defun barebox-commander-sentinel (process event)
  (let ((buffer (process-buffer process)))
    (when (string-match-p "finished\\|exited" event)
      (with-current-buffer buffer
	(goto-char (point-max))
	(if (= 0 (process-exit-status process)) (barebox-command-exit-ok)
	  (barebox-command-exit-ko))
	;; (cond ((eq barebox-status 'barebox-status-wait-prompt) (barebox-do-wait-prompt))
	;; 	    ((eq barebox-status 'barebox-idle) (barebox-do-one-action))
	(barebox-reschedule buffer)))))
					;(goto-char (point-max)))

(defun barebox-commander (command &rest args)
  ;(insert (apply 'concat "Launching commander with " command " " args) "\n")
  (let* ((exec-name (if (string= barebox-host "localhost")
			(concat user-emacs-directory "bin/barebox_commander.sh")
		      "bin/barebox_commander.sh"))
	 (proc (apply 'start-file-process "barebox-commander" (barebox-buffer)
		      exec-name command args)))
    (set-process-filter proc 'barebox-commander-filter)
    (set-process-sentinel proc 'barebox-commander-sentinel)))

(defun barebox-do-upload-file (filename)
  (insert (format "Uploading file %s\n" filename))
  (barebox-commander "upload_file" (file-truename filename)))

(defun barebox-do-wait-prompt ()
  (setq barebox-status 'barebox-status-wait-prompt)
  (insert "Waiting for barebox prompt\n")
  (barebox-commander "wait_prompt"))

(defun barebox-do-command (&rest args)
  (insert (format "Launching command %s\n" (apply 'concat args)))
  (apply 'barebox-commander "command" args))

(defun barebox-do-one-action ()
  (setq barebox-status 'barebox-status-busy)
  (let* ((action (pop barebox-queue))
	 (action-name (car action))
	 (action-args (cdr action)))
    ;(insert (format "RJK: doing command %s\n" action-name))
    (cond ((string= action-name "wait-prompt") (barebox-do-wait-prompt))
	  ((string= action-name "upload-file") (barebox-do-upload-file (car action-args)))
	  ((string= action-name "command") (apply 'barebox-do-command action-args))
	  (t (error (format "Action %s unknown" action-name)))))
  ; barebox-status is updated by barebox-do-*
  (barebox-reschedule (current-buffer)))

(defun barebox-scheduler (buffer)
  (with-current-buffer buffer
    (setq barebox-timer nil)
    (goto-char (point-max))
    (cond ((eq barebox-status 'barebox-status-wait-prompt)
	   (barebox-do-wait-prompt))
	  ((eq barebox-status 'barebox-status-idle)
	   (when barebox-queue (barebox-do-one-action)))
	  ((eq barebox-status 'barebox-status-busy) t)
	  (t (error "Barebox status unknown, buggy code !!!")))))

(defun barebox-reschedule (buffer)
  (unless barebox-timer
    (setq barebox-timer (run-with-timer 1 nil 'barebox-scheduler buffer))))

(defun barebox-enqueue (action-name &rest action-args)
  (if barebox-queue
      (nconc barebox-queue (list (apply 'list action-name action-args)))
    (setq barebox-queue (list (apply 'list action-name action-args))))
  (barebox-reschedule (current-buffer)))

(defun barebox-ctrl-mode ()
  "Major mode for controlling a barebox device."
)

;; Barebox control
(defun mioa701-barebox-start (&optional host)
  "Start barebox supervisor, which will connect to barebox and wait for a prompt,\
and the idle, expecting commands. Return the control buffer created"
  (interactive)
  (with-current-buffer (get-buffer-create (barebox-buffer))
    (make-local-variable 'barebox-status)
    (make-local-variable 'barebox-queue)
    (make-local-variable 'barebox-timer)
    (make-local-variable 'barebox-host)
    (setq barebox-status 'barebox-status-wait-prompt
	  barebox-queue nil
	  barebox-timer nil
	  mode-name "BareboxCtrl"
	  major-mode 'barebox-ctrl-mode)
    (setq barebox-host (barebox-guess-host host))
    (when (get-process "barebox-commander") (delete-process "barebox-commander"))
    (unless (string= barebox-host "localhost")
      (setq default-directory (tramp-make-tramp-file-name "ssh" nil barebox-host "")))
    (erase-buffer)
    (insert (format "Starting barebox controller on host %s\n" barebox-host))
    (barebox-reschedule (current-buffer))
    (current-buffer)))

(defun mioa701-barebox-choose-controller ()
  (let ((availables
	 (mapcar 'buffer-name
		 (remove-if-not (lambda (b)
				  (eq (buffer-local-value 'major-mode b)
				      'barebox-ctrl-mode)) (buffer-list)))))
    availables))

(defun barebox-prepare-tmp-upload-file (src-file dst-host buffer)
  "Transfer the file on the barebox controller host, and return the remote filename."
  (let* ((src-host (and (tramp-tramp-file-p src-file)
			(with-parsed-tramp-file-name src-file s s-host)))
	 (src-filename (or
			(and src-host (with-parsed-tramp-file-name src-file s s-localname))
			src-file))
	 (dst-filename (file-name-nondirectory src-file)))
    (cond (
	  ; Transfering remote file to localhost /tmp
	   (and (string= dst-host "localhost") src-host)
	   (with-temp-buffer
	     (setq dst-filename (concat "/tmp/" dst-filename))
	     (process-file-shell-command
	      (format "scp %s:%s %s" src-host src-filename dst-filename)
	      nil buffer nil)
	     dst-filename))
	  ; Keeping the source file on localhost as barebox is on localhost
	  ((string= dst-host "localhost") src-filename)
	  ; Transfering local/remote file to remote /tmp
	  (t
	   (with-temp-buffer
	     (setq default-directory
		   (tramp-make-tramp-file-name "ssh" nil src-host ""))
	     (setq dst-filename (concat "/tmp/" dst-filename))
	     (process-file-shell-command
	      (format "scp %s %s:%s" src-filename dst-host dst-filename)
	      nil buffer nil)
	     dst-filename)))))

(defun mioa701-barebox-upload-file (filename barebox-ctrl-buffer-name)
  "Ask barebox supervisor to upload a file to barebox device. Supposes \
mioa701-barebox-start was called first."
  (interactive (list (read-file-name "Filename to upload: "
				     (file-name-directory barebox-upload-filename)
				     nil t
				     (file-name-nondirectory barebox-upload-filename))
		     (if (= 1 (length (mioa701-barebox-choose-controller)))
			 (car (mioa701-barebox-choose-controller))
		       (completing-read "Barebox control: "
					(mioa701-barebox-choose-controller) nil t))))
  (let ((tmp-file)
	(barebox-ctrl-buffer (get-buffer barebox-ctrl-buffer-name)))
    (when barebox-ctrl-buffer
      (setq tmp-file
	    (barebox-prepare-tmp-upload-file
	     filename
	     (buffer-local-value 'barebox-host barebox-ctrl-buffer)
	     barebox-ctrl-buffer))
      (with-current-buffer barebox-ctrl-buffer
	(barebox-enqueue "upload-file" tmp-file)))))

(defun mioa701-barebox-command (bcmd barebox-ctrl-buffer-name)
  "Send a single command to barebox"
  (interactive (list
		(read-string "Barebox command: ")
		(if (= 1 (length (mioa701-barebox-choose-controller)))
		    (car (mioa701-barebox-choose-controller))
		  (completing-read "Barebox control: "
				   (mioa701-barebox-choose-controller) nil t))))
  (let ((barebox-ctrl-buffer (get-buffer barebox-ctrl-buffer-name)))
    (when barebox-ctrl-buffer
      (with-current-buffer barebox-ctrl-buffer
	(barebox-enqueue "command" (format "%s" bcmd))))))

(defun mioa701-barebox-term ()
  "Open terminal on barebox"
  (interactive)
  (serial-term "/dev/serial/by-id/usb-barebox_Scoter_Mitac_Mio_A701-if00-port0" 9600))

;;; Local Variables:
;;; eval: (defun byte-compile-this-file () (write-region (point-min) (point-max) buffer-file-name nil 't) (byte-compile-file buffer-file-name) nil)
;;; write-file-hooks: (byte-compile-this-file)
;;; byte-compile-warnings: (not cl-functions)
;;; End:
