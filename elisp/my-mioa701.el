;;; Robert Jarzmik
;;; 2013-09-01

(defvar mioa701-host "localhost"
  "Host to which the mioa701 is connected through USB.")
(defconst mioa701-path "/home/rj/mio_linux"
  "Path to mioa701 projects")
(defconst mioa701-kpath (concat mioa701-path "/kernel")
  "Path to mioa701 linux kernel")
(defconst mioa701-bpath (concat mioa701-path "/barebox")
  "Path to mioa701 barebox")
(defvar mioa701-extra-bootargs nil
  "Extra bootargs to pass to the linux kernel")

(defun mioa701-setup-bootargs (&optional extra-args)
  "Common bootargs to all launch types. Might be enhanced by mioa701-extra-bootargs"
  (concat "bootargs=\"\$bootargs " extra-args "\""))

(dolist (package '(openocd))
  (when (not (require package nil t))
    (message (concat (symbol-name package) " package is not available"))))

;; Barebox
(require 'device-control-barebox)
(defun mioa701-barebox-term ()
  "Open terminal on barebox"
  (interactive)
  (serial-term "/dev/serial/by-id/usb-barebox_Scoter_Mitac_Mio_A701-if00-port0" 9600))

(defun mioa701-barebox-command (command)
  (dctrl-barebox-action-command command))

(defun mioa701-barebox-upload-file (file)
  (dctrl-barebox-action-upload-file file))

(defun dctrl-barebox-action-upload-launch-kernel ()
  (interactive)
  (let ((device-name (dctrl-complete-device nil "barebox")))
    (with-current-buffer (dctrl-get-buffer device-name)
      (append
       (mioa701-barebox-upload-file (concat mioa701-kpath "/out/arch/arm/boot/zImage"))
       (mioa701-barebox-command (mioa701-setup-bootargs mioa701-extra-bootargs))
       (mioa701-barebox-command "bootm zImage")))))

(defun dctrl-barebox-action-upload-launch-kernel-dt ()
  (interactive)
  (let ((device-name (dctrl-complete-device nil "barebox")))
    (with-current-buffer (dctrl-get-buffer device-name)
      (append
       (mioa701-barebox-upload-file (concat mioa701-kpath "/out/arch/arm/boot/zImage"))
       (mioa701-barebox-upload-file (concat mioa701-kpath "/out/arch/arm/boot/dts/pxa270-mioa701.dtb"))
       (mioa701-barebox-command (mioa701-setup-bootargs
				 (concat "loglevel=10 pxa2xx-cpufreq.pxa27x_maxfreq=624 " mioa701-extra-bootargs)))
       (mioa701-barebox-command "bootm -o pxa270-mioa701.dtb zImage")))))

(defun mioa701-change-host (host)
  "Changes the host which is connected to the mioa701."
  (interactive "sHost connected to the mioa701 device: ")
  (when (featurep 'openocd)
    (setq openocd-host host)))

;; Kernel
(define-minor-mode mioa701-kernel
  "Mio A701 kernel mode"
  nil "mio-k" nil
  (set (make-local-variable 'compile-command)
       (concat "cd " mioa701-kpath " && make -j 8")))

(add-hook 'c-mode-hook
	  (lambda () (when (and (buffer-file-name)
	       (string-match mioa701-kpath (buffer-file-name)))
	    (mioa701-kernel))))

;; Menu
(require 'easymenu)
(easy-menu-define mioa701-menu global-map "MioA701 smartphone menu"
  '("MioA701"
    ("Barebox"
     ["Barebox Term" (mioa701-barebox-term)]
     ["Barebox Commander Upload file"
      (with-current-buffer (dctrl-get-buffer (dctrl-complete-device nil "barebox"))
	(nconc dctrl-actions (dctrl-barebox-action-upload-file))
	(dctrl-start)) ]
     ["Barebox Commander Execute command"
      (with-current-buffer (dctrl-get-buffer (dctrl-complete-device nil "barebox"))
	(nconc dctrl-actions (dctrl-barebox-action-command))
	(dctrl-start)) ]
     )
    ("Kernel"
     ["Kernel Term" (mioa701-kernel-term)]
     )
    ))
(easy-menu-remove-item global-map '("menu-bar") "MioA701")
(easy-menu-add-item global-map '("menu-bar") mioa701-menu "Help")

;; Keyboard shortcuts
(global-set-key (kbd "\C-cm") 'device-control)

(provide 'my-mioa701)
