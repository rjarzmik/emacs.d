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

(defconst barebox-buffer "*barebox-control*")
(defvar barebox-upload-filename "")

(defvar barebox-queue)
(defvar barebox-timer)

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
  (when (string-match-p "finished\\|exited" event)
    (with-current-buffer (get-buffer-create barebox-buffer)
      (goto-char (point-max))
      (if (= 0 (process-exit-status process)) (barebox-command-exit-ok)
	(barebox-command-exit-ko))
      ;; (cond ((eq barebox-status 'barebox-status-wait-prompt) (barebox-do-wait-prompt))
      ;; 	    ((eq barebox-status 'barebox-idle) (barebox-do-one-action))
      (barebox-reschedule))))
  ;(goto-char (point-max)))

(defun barebox-commander (command &rest args)
  ;(insert (apply 'concat "Launching commander with " command " " args) "\n")
  (let ((proc (apply 'start-process "barebox-commander" barebox-buffer
		      (concat user-emacs-directory "bin/barebox_commander.sh")
		      command args)))
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
  (barebox-reschedule))

(defun barebox-scheduler ()
  (with-current-buffer (get-buffer-create barebox-buffer)
    (setq barebox-timer nil)
    (goto-char (point-max))
    (cond ((eq barebox-status 'barebox-status-wait-prompt)
	   (barebox-do-wait-prompt))
	  ((eq barebox-status 'barebox-status-idle)
	   (when barebox-queue (barebox-do-one-action)))
	  ((eq barebox-status 'barebox-status-busy) t)
	  (t (error "Barebox status unknown, buggy code !!!")))))

(defun barebox-reschedule ()
  (unless barebox-timer
    (setq barebox-timer (run-with-timer 1 nil 'barebox-scheduler))))

(defun barebox-enqueue (action-name &rest action-args)
  (if barebox-queue
      (nconc barebox-queue (list (apply 'list action-name action-args)))
    (setq barebox-queue (list (apply 'list action-name action-args))))
  (barebox-reschedule))

;; Barebox control
(defun mioa701-barebox-start ()
  "Start barebox supervisor, which will connect to barebox and wait for a prompt,\
and the idle, expecting commands."
  (interactive)
  (with-current-buffer (get-buffer-create barebox-buffer)
    (make-local-variable 'barebox-status)
    (make-local-variable 'barebox-queue)
    (make-local-variable 'barebox-timer)
    (when (get-process "barebox-commander") (delete-process "barebox-commander"))
    (setq barebox-status 'barebox-status-wait-prompt)
    (setq barebox-queue nil)
    (setq barebox-timer nil)
    (erase-buffer)
    (insert "Starting barebox controller\n")
    (barebox-reschedule)
    ))

(defun mioa701-barebox-upload-file (filename)
  "Ask barebox supervisor to upload a file to barebox device. Supposes \
mioa701-barebox-start was called first."
  (interactive (list (read-file-name "Filename to upload: "
				     (file-name-directory barebox-upload-filename)
				     nil t
				     (file-name-nondirectory barebox-upload-filename))))
  (with-current-buffer (get-buffer-create barebox-buffer)
    (barebox-enqueue "upload-file" filename)))

(defun mioa701-barebox-command (bcmd)
  "Send a single command to barebox"
  (interactive "sBarebox command: ")
  (with-current-buffer (get-buffer-create barebox-buffer)
    (barebox-enqueue "command" (format "%s" bcmd))))

(defun mioa701-barebox-term ()
  "Open terminal on barebox"
  (interactive)
  (serial-term "/dev/ttyUSB0" 9600))

;;; Local Variables:
;;; eval: (defun byte-compile-this-file () (write-region (point-min) (point-max) buffer-file-name nil 't) (byte-compile-file buffer-file-name) nil)
;;; write-file-hooks: (byte-compile-this-file)
;;; End:
