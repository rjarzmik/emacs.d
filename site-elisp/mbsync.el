;;; mbsync.el --- Run Mbsync from Emacs

;; Copyright (C) 2011 Robert Jarzmik

;; Author: Robert Jarzmik <robert.jarzmik@free.fr>

;; This file is NOT part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; M-x mbsync

;; We need comint for `comint-truncate-buffer'
(require 'comint)

(defgroup mbsync nil
  "Run Mbsync."
  :group 'comm)

(defcustom mbsync-buffer-name "*Mbsync*"
  "Name of the buffer used to run mbsync."
  :group 'mbsync
  :type 'string)

(defcustom mbsync-command "mbsync -q -a"
  "Command to run to launch Mbsync."
  :group 'mbsync
  :type 'string)

(defcustom mbsync-buffer-maximum-size comint-buffer-maximum-size
  "The maximum size in lines for Mbsync buffer."
  :group 'mbsync
  :type 'integer)

(defcustom mbsync-enable-mode-line-p '(member
				       major-mode
				       '(mbsync-mode gnus-group-mode))
  "Whether enable Mbsync mode line status display.
This form is evaluated and its return value determines if the
Mbsync status should be displayed in the mode line."
  :group 'mbsync)

(defcustom mbsync-mode-line-style 'symbol
  "Set what to display in mode-line.
If set to 'symbol, it will only display
`mbsync-mode-line-symbol' with different colors based on
what Mbsync is doing. If set to 'text, it will display the
action as a text in color instead of a single symbol."
  :group 'mbsync
  :type '(choice (const :tag "Symbol" symbol)
                 (const :tag "Action text" text)))

(defcustom mbsync-mode-line-symbol "✉"
  "Symbol used to display Mbsync status in mode-line.
This is used when `mbsync-mode-line-style' is set to 'symbol."
  :group 'mbsync
  :type 'string)

(defcustom mbsync-timestamp nil
  "Timestamp to add at the beginning of each mbsync line."
  :type 'string
  :group 'mbsync)

(defcustom mbsync-event-hooks nil
  "Hooks run when Mbsync state changes : hook(process, msg)."
  :type 'hook
  :group 'mbsync)

(defvar mbsync-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") 'mbsync-quit)
    (define-key map (kbd "g") 'mbsync-resync)
    (define-key map (kbd "K") 'mbsync-kill)
    map)
  "Keymap for mbsync-mode.")

(defvar mbsync-mode-line-string nil
  "Variable showed in mode line to display Mbsync status.")

(put 'mbsync-mode-line-string 'risky-local-variable t) ; allow properties

(defun mbsync-make-buffer ()
  "Get the mbsync buffer."
  (let ((buffer (get-buffer-create mbsync-buffer-name)))
    (with-current-buffer buffer
      (mbsync-mode))
    buffer))

(defun mbsync-switch-to-buffer (e)
  "Go to Mbsync buffer."
  (interactive "e")
  (save-selected-window
    (select-window
     (posn-window (event-start e)))
    (switch-to-buffer (get-buffer mbsync-buffer-name))))

(defvar mbsync-mode-line-map
  (let ((map (make-sparse-keymap)))
    (define-key map (vector 'mode-line 'mouse-2) 'mbsync-switch-to-buffer)
    map)
  "Keymap used in mode line.")

(defun mbsync-update-mode-line (process)
  "Update mode line information about Mbsync PROCESS."
  (setq mbsync-mode-line-string
        (propertize
         (concat " [Mbsync: "
                 (let* ((status (process-status process))
		       (line (case status
			       ('run "running")
			       ('exit "exit")
			       (t "unknown"))))
		   line)
		 "]")
         'mouse-face 'mode-line-highlight
         'help-echo "mouse-2: Go to Mbsync buffer"
         'local-map mbsync-mode-line-map))
  (force-mode-line-update))

(defun mbsync-insert (buffer text)
  "Insert TEXT in PROCESS buffer."
  (let ((buffer (process-buffer process)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (let ((inhibit-read-only t))
          ;; If the cursor is at the end, append text like we would be in
          ;; "tail".
          (if (eq (point) (point-max))
              (progn
                (when mbsync-timestamp
                  (insert (format-time-string mbsync-timestamp)))
                (insert text)
                (set-marker (process-mark process) (point)))
            ;; But if not, let the cursor where it is, so `save-excursion'.
            (save-excursion
              (goto-char (point-max))
              (when mbsync-timestamp
                (insert (format-time-string mbsync-timestamp)))
              (insert text)
              (set-marker (process-mark process) (point)))))))))

(defun mbsync-process-filter (process msg)
  "Filter PROCESS output MSG."
  (let* ((buffer (process-buffer process)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (mbsync-insert process (concat msg "\n"))
	(let ((comint-buffer-maximum-size mbsync-buffer-maximum-size))
          (comint-truncate-buffer))))
    (mbsync-update-mode-line process)
    (run-hook-with-args 'mbsync-event-hooks process msg)))

(defun mbsync-process-sentinel (process state)
  "Monitor STATE change of PROCESS."
  (mbsync-insert process (concat "*** Process " (process-name process) " " state))
  (mbsync-update-mode-line process)
  (run-hook-with-args 'mbsync-event-hooks process state))

(defun mbsync-mode-line ()
  "Return a string to display in mode line."
  (when (eval mbsync-enable-mode-line-p)
    mbsync-mode-line-string))

;;;###autoload
(defun mbsync ()
  "Start Mbsync."
  (interactive)
  (let* ((buffer (mbsync-make-buffer)))
    (add-to-list 'global-mode-string '(:eval (mbsync-mode-line)) t)
    (unless (get-buffer-process buffer)
      (let ((process (start-process-shell-command
                      "mbsync"
                      buffer
                      mbsync-command)))
	(mbsync-update-mode-line process)
        (set-process-filter process 'mbsync-process-filter)
        (set-process-sentinel process 'mbsync-process-sentinel)))))

(defun mbsync-quit ()
  "Quit Mbsync."
  (interactive)
  (kill-buffer (get-buffer mbsync-buffer-name)))

(defun mbsync-resync ()
  "Send a USR1 signal to Mbsync to force accounts synchronization."
  (interactive)
  (signal-process (get-buffer-process (get-buffer mbsync-buffer-name)) 'SIGUSR1))

(defun mbsync-kill ()
  "Send a TERM signal to Mbsync."
  (interactive)
  (signal-process (get-buffer-process (get-buffer mbsync-buffer-name)) 'SIGTERM))

(define-derived-mode mbsync-mode fundamental-mode "Mbsync"
  "A major mode for Mbsync interaction."
  :group 'comm
  (setq buffer-read-only t)
  (setq buffer-undo-list t))

(provide 'mbsync)
