;;; Robert Jarzmik
;;; 2013-09-01

(defconst pxa-path "/home/rj/mio_linux"
  "Path to pxa projects")
(defconst pxa-kpath (concat pxa-path "/kernel")
  "Path to pxa linux kernel")
(defconst pxa-bpath (concat pxa-path "/barebox")
  "Path to pxa barebox")
(defvar pxa-extra-bootargs nil
  "Extra bootargs to pass to the linux kernel")
(defvar-local pxa-openocd-str nil
  "Openocd control host:port")

(defun pxa-setup-bootargs (&optional extra-args)
  "Common bootargs to all launch types. Might be enhanced by pxa-extra-bootargs"
  (concat "bootargs=\\\"\$bootargs " extra-args "\\\""))

(defun belgarath-uart-log ()
  (interactive)
  (let ((buf (get-buffer-create "*belgarath-uart-log*")))
    (async-shell-command
     "ssh belgarath \"socat /dev/serial/by-id/usb-Prolific_Technology_Inc._USB_2.0_To_COM_Device-if00-pt0,raw,echo=0 STDIO\"" buf)))

(dolist (package '(openocd))
  (when (not (require package nil t))
    (message (concat (symbol-name package) " package is not available"))))

;; Barebox
(require 'device-control-pxa)

;; Log-tools
(require 'lt-serial-kernel)

(provide 'my-pxa)
