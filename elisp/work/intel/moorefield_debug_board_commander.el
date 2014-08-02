(require 'widget)

(eval-when-compile
  (require 'wid-edit))

(defvar mofd-dbg-process)
(defvar mofd-debug-board-commander (concat user-emacs-directory "bin/" "moorefield_debugboard_commander"))

(defun mofd-dbg-notify (wid)
  (let ((val (if (widget-value wid) "on" "off"))
	(name (widget-get wid :ctrlname)))
    (process-send-string mofd-dbg-process (format "%s=%s\n" name val))))

(defun mofd-dbg-create-onoff-button (name)
  (widget-create 'toggle
		 :on (concat name "=on")
		 :off (concat name "=off")
		 :ctrlname name
		 :notify (lambda (wid &rest ignore)
			   (mofd-dbg-notify wid))))

(defun mofd-dbg-start-process ()
  "Start the debug card backend, and eliminate the output"
  (let ((proc (start-process "mofd-debug-board-commander"
	      (current-buffer) mofd-debug-board-commander)))
    (set-process-filter proc (lambda (proc string) nil))
    (set-process-query-on-exit-flag proc nil)
    (setq mofd-dbg-process proc))
)

(defun mofd-dbg-debug-board ()
  "Create the widgets from the Widget manual."
  (interactive)
  (switch-to-buffer "*Moorefield Debug Board*")
  (kill-all-local-variables)
  (make-local-variable 'mofd-dbg-process)
  (let ((inhibit-read-only t))
    (erase-buffer))
  (remove-overlays)

  (mofd-dbg-start-process)

  (widget-insert "Moorefield Debug Board.\n")

  (mofd-dbg-create-onoff-button "usb_data")
  (mofd-dbg-create-onoff-button "vbus")
  (mofd-dbg-create-onoff-button "vbat")
  
  (use-local-map widget-keymap)
  (widget-setup))
