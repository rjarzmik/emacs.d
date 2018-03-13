
(require 'telnet)

(defvar openocd-host nil
  "IPv4 address or hostname where the openocd server is launched.
If nil, localhost is used unless current buffer is a tramp one,
in which case the tramp hostname prevails")
(defvar openocd-port 4444
  "TCP port number of openocd server, normally 4444")

(defvar openocd-executable "openocd"
  "Name of the executable to be launched to have openocd server running.
If nil, openocd is not launched, and supposed to be already running")

(defun openocd-launch-daemon ()
  (when (get-process "openocd-server") (delete-process "openocd-server"))
  (when openocd-executable
    (start-process "openocd-server" " *openocd-server*" openocd-executable "-s" "/home/rj")))

(defun openocd-sentinel (process event)
  (when (get-process "openocd-server") (delete-process "openocd-server"))
  (when (get-buffer " *openocd-server*")
    (kill-buffer " *openocd-server*"))
)

(defun openocd-guess-host (host)
  "Calculates the host where openocd server is/will be launched."
  (cond (host)
	(openocd-host)
	((and (featurep 'tramp) (tramp-tramp-file-p default-directory))
	 (with-parsed-tramp-file-name default-directory guess guess-host))
	(t "localhost")))

(defun openocd (&optional host port)
  "Launch openocd, connect and provide the terminal to command openocd"
  (interactive)

  (let ((h (openocd-guess-host host))
	(p (or port openocd-port)))
    (when (string= h "localhost")
      (openocd-launch-daemon)
      (sleep-for 1))
    (telnet h p)
    (rename-buffer (format "*openocd-%s*" h))
    (set-process-sentinel (get-buffer-process (current-buffer)) 'openocd-sentinel)
    ))
      
(provide 'openocd)
