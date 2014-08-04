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
  (concat "bootargs=\"\$bootargs ramoops.mem_address=0xa2000000 ramoops.mem_size=1048576 ramoops.console_size=131072 " extra-args "\""))

(dolist (package '(openocd))
  (when (not (require package nil t))
    (message (concat (symbol-name package) " package is not available"))))

;; Barebox
(load "mioa701/mioa701_barebox")

(defun mioa701-launch-kernel-pstore ()
  (interactive)
  (let ((b (mioa701-barebox-start)))
    (mioa701-barebox-command (mioa701-setup-bootargs mioa701-extra-bootargs) b)
    (mioa701-barebox-command "mci0.probe=1" b)
    (mioa701-barebox-command "mkdir /sdcard" b)
    (mioa701-barebox-command "mount /dev/disk0.0 /sdcard" b)
    (mioa701-barebox-command "bootm /sdcard/zImage.pstore" b)))

(defun mioa701-upload-launch-kernel ()
  (interactive)
  (let ((b (mioa701-barebox-start)))
    (mioa701-barebox-upload-file (concat mioa701-kpath "/arch/arm/boot/zImage") b)
    (mioa701-barebox-command (mioa701-setup-bootargs mioa701-extra-bootargs) b)
    (mioa701-barebox-command "bootm zImage" b)))

(defun mioa701-upload-launch-kernel-dt ()
  (interactive)
  (let ((b (mioa701-barebox-start)))
  (mioa701-barebox-upload-file (concat mioa701-kpath "/arch/arm/boot/zImage") b)
  (mioa701-barebox-upload-file (concat mioa701-kpath "/arch/arm/boot/dts/mioa701.dtb") b)
  (mioa701-barebox-command (mioa701-setup-bootargs "loglevel=10 pxa2xx-cpufreq.pxa27x_maxfreq=624 dyndbg=\\\"file phy-gpio-vbus-usb.c +p\\\"") b)
  (mioa701-barebox-command "bootm -o mioa701.dtb zImage" b)))

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
     ["Barebox Commander start" (mioa701-barebox-start)]
     ["Barebox Commander Upload file" (mioa701-barebox-upload-file)]
     ["Barebox Commander Execute command" (mioa701-barebox-command)]
     )
    ("Kernel"
     ["Kernel Term" (mioa701-kernel-term)]
     )
    ))
(easy-menu-remove-item global-map '("menu-bar") "MioA701")
(easy-menu-add-item global-map '("menu-bar") mioa701-menu "Help")

;; Keyboard shortcuts
(defvar mioa701:map nil
  "The mioa701 keymap.")
(unless mioa701:map
    (define-prefix-command 'mioa701:map)
    ;; The following line corresponds to be beginning of the "Cscope" menu.
    (define-key 'mioa701:map "d" 'mioa701-upload-launch-kernel-dt)
    (define-key 'mioa701:map "k" 'mioa701-upload-launch-kernel)
    (define-key 'mioa701:map "u" 'mioa701-barebox-upload-file)
)
(global-set-key (kbd "\C-cm") 'mioa701:map)

(provide 'my-mioa701)

;;; Local Variables:
;;; eval: (defun byte-compile-this-file () (write-region (point-min) (point-max) buffer-file-name nil 't) (byte-compile-file buffer-file-name) nil)
;;; write-file-hooks: (byte-compile-this-file)
;;; End:
