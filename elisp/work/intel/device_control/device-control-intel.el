(require 'ido)
(require 'device-control)
(require 'device-control-adb)
(require 'device-control-fastboot)

(defconst dctrl-intel-flash-alist
  (append dctrl-fastboot-flash-alist
	  '(("ESP"		.	"ESP.img")
	    ("osloader"		.       "efilinux-eng.efi")
	    ("capsule"		.	"capsule.bin"))))

(defun dctrl-intel-action-kill-wizard ()
  (dctrl-adb-run "shell" "pm" "disable" "com.google.android.setupwizard"))

(defun dctrl-intel-action-adb2fastboot ()
  (dctrl-adb-run "shell" "setprop" "sys.adb.config" "fastboot"))

(defun dctrl-intel-action-fastboot2adb ()
  (dctrl-fastboot-run "oem" "fastboot2adb"))

(defun dctrl-intel-action-flash (&optional type file)
  (let* ((type (or type (ido-completing-read "Type of flash: " (mapcar 'car dctrl-intel-flash-alist) nil t)))
	 (out-path (concat aosp-path "/out/target/product/" board-name "/"))
	 (file (or file (concat out-path (assoc-default type ctrl-fastboot-flash-alist))))
	 tramp-cmd ctrlhost-filename)
    (unless (file-exists-p file)
      (setq file (ido-read-file-name "Fastboot file: " out-path nil t)))
    (dctrl-fastboot-action-flash file)))

(defun dctrl-intel-action-flashrom (&optional file voltage)
  (let* ((iafw-path (concat aosp-path "/vendor/intel/fw/PRIVATE/byt-iafw/stitching"))
	 (path (concat iafw-path "output_binaries/outimage.bin"))
	 (file (or file (and (file-exists-p path) path)
		   (ido-read-file-name "ROM File : " iafw-path)))
	 (voltage (or voltage (unless (file-exists-p path) (read-string "Voltage : " "1.8V")) "1.8V"))
	 tramp-cmd ctrlhost-filename)
    (multiple-value-setq (tramp-cmd ctrlhost-filename)
      (dctrl-untramp-file file))
    (append tramp-cmd
	    (dctrl-run-process (list "flashrom" "-p" (concat "dediprog:voltage=" voltage) "-w"
				    (expand-file-name ctrlhost-filename))))))

(defun dctrl-intel-action-phoneflashtool (&optional file)
  (let* ((path (concat aosp-path "/pub/" (upcase board-name) "/flash_files/"))
	 (file (expand-file-name (or file (ido-read-file-name "FlashFile: " path))))
	 tramp-cmd ctrlhost-filename)
    (multiple-value-setq (tramp-cmd ctrlhost-filename)
      (dctrl-untramp-file file))
    
    (append tramp-cmd
	    (dctrl-run-process (list "phoneflashtool-cli-launcher.sh" "--always-unzip" "-f" ctrlhost-filename)))))

(defun dctrl-intel-action-boot (&optional file)
  (let* ((out-path (concat aosp-path "/out/target/product/" board-name "/"))
	 (file (or file (concat out-path "droidboot.img")))
	 tramp-cmd ctrlhost-filename)
    (unless (file-exists-p file)
      (setq file (ido-read-file-name "Fastboot file: " out-path nil t)))
    (dctrl-fastboot-action-boot file)))

(defun dctrl-intel-action-power-on ()
  (dctrl-run-process '("~/bin/relay.sh" "on")))

(defun dctrl-intel-action-press-power-key ()
  (dctrl-run-process '("~/bin/relay.sh" "pk")))

(defun dctrl-intel-action-release-power-key ()
  (dctrl-run-process '("~/bin/relay.sh" "rpk")))

(defun dctrl-intel-action-force-shutdown ()
  (dctrl-run-process '("~/bin/relay.sh" "fco")))

(defun dctrl-intel-action-force-reboot ()
  (append (dctrl-intel-action-force-shutdown)
	  (dctrl-intel-action-power-on)))

(defun dctrl-intel-action-force-droidboot ()
  (append (dctrl-intel-action-force-shutdown)
	  (dctrl-intel-action-press-volume-down)
	  (dctrl-intel-action-power-on)
	  (dctrl-action-wait 10)
	  (dctrl-intel-action-release-volume-down)))

(defun dctrl-intel-action-press-volume-down ()
  (dctrl-run-process '("~/bin/relay.sh" "voldown")))

(defun dctrl-intel-action-release-volume-down ()
  (dctrl-run-process '("~/bin/relay.sh" "rvoldown")))

(defun dctrl-intel-action-press-volume-up ()
  (dctrl-run-process '("~/bin/relay.sh" "volup")))

(defun dctrl-intel-action-release-volume-up ()
  (dctrl-run-process '("~/bin/relay.sh" "rvolup")))

(defun dctrl-intel-action-boot-to-dnx ()
  (append (dctrl-intel-action-press-power-key)
	  (dctrl-intel-action-press-volume-up)
	  (dctrl-intel-action-press-volume-down)
	  (dctrl-action-wait 6)
	  (dctrl-intel-action-press-power-key)
	  (dctrl-intel-action-release-volume-up)
	  (dctrl-intel-action-release-volume-down)))

(defun dctrl-intel-action-usb-plug ()
  (dctrl-run-process '("~/bin/relay.sh" "plug")))

(defun dctrl-intel-action-usb-unplug ()
  (dctrl-run-process '("~/bin/relay.sh" "unplug")))

(defun dctrl-intel-get-actions ()
  (append
   (dctrl-build-fun-list "dctrl-intel-action-")
   (dctrl-build-fun-list "dctrl-adb-action-")
   (dctrl-build-fun-list "dctrl-fastboot-action-")))

(defun dctrl-intel-guess-device-names ()
  (append
   (dctrl-adb-guess-device-names)
   (dctrl-fastboot-guess-device-names)))

(dctrl-register-backend
 (make-dctrl-backend :name "intel"
		     :get-actions 'dctrl-intel-get-actions
		     :guess-device-names 'dctrl-intel-guess-device-names))

(provide 'device-control-intel)
