(require 'device-control)
(require 'ido)
(require 'tramp)

(defvar barebox-exec "barebox_commander.sh")
(defconst barebox-default-devname
  "serial/by-id/usb-barebox_Scoter_Mitac_Mio_A701-if00-port0")

(defun dctrl-barebox-get-tty-devname ()
  (concat "/dev/"
	  (or
	   (and dctrl-automatic-mode barebox-default-devname)
	   (and (numberp (string-match "^\\\([^@]*\\\)@.*$" dctrl-device-name))
		(match-string 1 dctrl-device-name))
	   barebox-default-devname)))

(defun dctrl-barebox-run (&rest args)
  (dctrl-run-process
   (nconc (list barebox-exec)
	  (list (dctrl-barebox-get-tty-devname))
	  args)))

(defun dctrl-barebox-action-reset ()
  (dctrl-barebox-run "command" "reset"))

(defun dctrl-barebox-action-waitprompt ()
  ; Any command will trigger a waitprompt, let's go for echo
  (dctrl-barebox-run "echo"))

(defun dctrl-barebox-action-command (&optional command)
  (let ((command (or command
		     (read-string "Barebox command: "))))
    (dctrl-barebox-run "command" command)))

(defun dctrl-barebox-action-upload-file (&optional filename)
  (let* ((src (or filename (ido-read-file-name "Source: ")))
	 tramp-cmd ctrlhost-filename)
    (multiple-value-setq (tramp-cmd ctrlhost-filename)
      (dctrl-untramp-file src))
    (append tramp-cmd (dctrl-barebox-run "upload_file" ctrlhost-filename))))

(defun dctrl-barebox-process-sentinel (p e)
  (with-current-buffer (process-buffer p)
    (when (string-match-p "finished\\|exited" e)
      (if (= 0 (process-exit-status p))
	  (when (eq dctrl-state 'running)
	    (dctrl-continue))
	(progn
	  (dctrl-barebox-start 1)
	  (dctrl-continue))))))

(defun dctrl-barebox-waitprompt ()
  (lambda ()
	  (dctrl-msg "Waiting for barebox prompt")
	  (set-process-sentinel
	   (apply 'start-file-process "ctrl" (current-buffer)
		  barebox-exec (list (dctrl-barebox-get-tty-devname) "wait_prompt"))
	   'dctrl-barebox-process-sentinel)
	  (set-process-filter (get-buffer-process (current-buffer))
			      'dctrl-process-filter)
	  'sleep))

(defun dctrl-barebox-get-actions ()
  (dctrl-build-fun-list "dctrl-barebox-action-"))

(defun dctrl-barebox-guess-device-names ()
  (let* ((usb-devs
	  (split-string
	   (shell-command-to-string
	    "dirname $(grep barebox /sys/bus/usb/devices/*/manufacturer | cut -d: -f1)")))
	 (tty-names
	  (remove-if-not (curry 'string-prefix-p "tty")
			 (mapcar (lambda (dev)
				   (car
				    (split-string
				     (shell-command-to-string
				      (format "basename $(ls -1d %s/*/tty*)" dev)))))
				 usb-devs)))
	 (host
	  (if (tramp-tramp-file-p default-directory)
	      (with-parsed-tramp-file-name default-directory d d-host)
	    "localhost")))
    (mapcar (lambda (n) (format "%s@%s" n host)) tty-names)))

(defun dctrl-barebox-start (&optional delay)
  (setcdr dctrl-actions
	  (cons (dctrl-barebox-waitprompt) (cdr dctrl-actions)))
  (when delay
    (setcdr dctrl-actions
	    (cons (car (dctrl-action-wait delay)) (cdr dctrl-actions)))))

(defun dctrl-barebox-create ()
  (rename-buffer (format "*%s*" dctrl-device-name)))

(dctrl-register-backend
 (make-dctrl-backend :name "barebox"
		     :create 'dctrl-barebox-create
		     :start 'dctrl-barebox-start
		     :get-actions 'dctrl-barebox-get-actions
		     :guess-device-names 'dctrl-barebox-guess-device-names))

(provide 'device-control-barebox)
