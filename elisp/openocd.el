
(require 'telnet)

(defvar openocd-host "localhost"
  "IPv4 address or hostname where the openocd server is launched")
(defvar openocd-port 4444
  "TCP port number of openocd server, normally 4444")

(defvar openocd-executable "openocd"
  "Name of the executable to be launched to have openocd server running.
If nil, openocd is not launched, and supposed to be already running")

(defun openocd-launch-daemon ()
  (when (get-process "openocd-server") (delete-process "openocd-server"))
  (when openocd-executable
    (start-process "openocd-server" "*openocd-server*" openocd-executable "-s" "/home/rj")))

(defun openocd-sentinel (process event)
  (when (get-process "openocd-server") (delete-process "openocd-server"))
  (when (get-buffer "*openocd-server*")
    (kill-buffer "*openocd-server*"))
)

(defun openocd ()
  "Launch openocd, connect and provide the terminal to command openocd"
  (interactive)

  (openocd-launch-daemon)
  (sleep-for 1)
  (telnet openocd-host openocd-port)
  (set-process-sentinel (get-buffer-process (current-buffer)) 'openocd-sentinel)
)

      
(provide 'openocd)
