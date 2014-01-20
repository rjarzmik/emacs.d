;;; rscope.el --- Another cscope interface for emacs
;;
;; Robert Jarzmik <robert.jarzmik@free.fr>
;;
;; Heavily inspired by ascope, relies on :
;;   - pre-launched cscope processes
;;   - auto finding of the best suited cscope process (based on file directory)
;;   - has navigation and preview capability in rscope result buffer

;; Usage:
;;   load this script using (require 'rscope) in you .emacs
;;   M-x rscope-init load one of the cscope databases.
;;     This command must be issue prior to issue any other command below, the
;;     directory fed to this command must be the directory holding the cscope.out
;;     file.
;;   M-x find-file "myfile.c"
;;   M-x rscope-find-this-symbol
;;     Find a symbol from the database.

;; Result buffer navigation (*Result*)
;;
;;   Use "n" to navigate to next entry in results
;;   Use "p" to navigate to previsous entry in results
;;   Use SPACE to preview an entry
;;   Use ENTER bury result buffer and switch to the previewed entry
;;   Use "q" to bury result buffer and switch to other window.

;; Available global commands (bound by default to C-c s [gstCcia])
;;
;; M-x rscope-find-global-definition
;; M-x rscope-find-this-symbol
;; M-x rscope-find-this-text-string
;; M-x rscope-find-functions-calling-this-function
;; M-x rscope-find-called-functions
;; M-x rscope-find-files-including-file
;; M-x rscope-all-symbol-assignments
;; M-x rscope-pop-mark

(defgroup rscope nil
  "Cscope interface for (X)Emacs.
Using cscope, you can easily search for where symbols are used and defined.
It is designed to answer questions like:

Where is this variable used?
What is the value of this preprocessor symbol?
Where is this function in the source files?
What functions call this function?
What functions are called by this function?
Where does the message \"out of space\" come from?
Where is this source file in the directory structure?
What files include this header file?
"
  :prefix "rscope-"
  :group 'tools)

(defcustom rscope-allow-arrow-overlays t
  "*If non-nil, use an arrow overlay to show target lines.
Arrow overlays are only used when the following functions are used:

rscope-show-entry-other-window
rscope-show-next-entry-other-window
rscope-show-prev-entry-other-window

The arrow overlay is removed when other cscope functions are used.
Note that the arrow overlay is not an actual part of the text, and can
be removed by quitting the cscope buffer."
  :type 'boolean
  :group 'rscope)

(defcustom rscope-overlay-arrow-string "=>"
  "*The overlay string to use when displaying arrow overlays."
  :type 'string
  :group 'rscope)

(defcustom rscope-name-line-width -30
  "*The width of the combined \"function name:line number\" field in the
cscope results buffer. If negative, the field is left-justified."
  :type 'integer
  :group 'rscope)

(defcustom rscope-use-face t
  "*Whether to use text highlighting (? la font-lock) or not."
  :group 'rscope
  :type '(boolean))

(defface rscope-file-face
  '((((class color) (background dark))
     (:foreground "yellow"))
    (((class color) (background light))
     (:foreground "blue"))
    (t (:bold t)))
  "Face used to highlight file name in the *rscope* buffer."
  :group 'cscope)

(defface rscope-function-face
  '((((class color) (background dark))
     (:foreground "cyan"))
    (((class color) (background light))
     (:foreground "magenta"))
    (t (:bold t)))
  "Face used to highlight function name in the *rscope* buffer."
  :group 'rscope)


(defface rscope-line-number-face
  '((((class color) (background dark))
     (:foreground "red"))
    (((class color) (background light))
     (:foreground "red"))
    (t (:bold t)))
  "Face used to highlight line number in the *rscope* buffer."
  :group 'rscope)


(defface rscope-line-face
  '((((class color) (background dark))
     (:foreground "green"))
    (((class color) (background light))
     (:foreground "black"))
    (t (:bold nil)))
  "Face used to highlight the rest of line in the *rscope* buffer."
  :group 'rscope)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defconst rscope-separator-line
  "-------------------------------------------------------------------------------\n"
  "Line of text to use as a visual separator.
Must end with a newline.")

(defvar rscope-first-match nil
  "The first match result output by cscope.")
(make-variable-frame-local 'rscope-first-match-point)

(defvar rscope-first-match-point nil
  "Buffer location of the first match.")
(make-variable-frame-local 'rscope-first-match-point)

(defvar rscope-action-message nil "The message about what action is taken")
(make-variable-frame-local 'rscope-action-message)

(defvar rscope-last-file nil
  "The file referenced by the last line of cscope process output.")
(make-variable-frame-local 'rscope-last-file)

(defvar rscope-list-entry-keymap nil
  "The keymap used in the *Result* buffer which lists search results.")
(if rscope-list-entry-keymap
    nil
  (setq rscope-list-entry-keymap (make-keymap))
  (suppress-keymap rscope-list-entry-keymap)
  (define-key rscope-list-entry-keymap "n" 'rscope-next-symbol)
  (define-key rscope-list-entry-keymap "p" 'rscope-prev-symbol)
  (define-key rscope-list-entry-keymap "q" 'rscope-close-results)
  (define-key rscope-list-entry-keymap " " 'rscope-preview-entry-other-window)
  (define-key rscope-list-entry-keymap (kbd "RET") 'rscope-select-entry-other-window)
  (define-key rscope-list-entry-keymap (kbd "C-<return>") 'rscope-select-entry-current-window)
  )

(defvar rscope-list-entry-hook nil
  "*Hook run after rscope-list-entry-mode entered.")

(defvar rscope-marker nil
  "The location from which cscope was invoked.")

(defvar rscope-output-buffer-name "*Result*"
  "The name of the cscope output buffer.")

(defvar rscope-marker-ring-length 30 )

(defvar rscope-marker-ring (make-ring rscope-marker-ring-length))

(defvar rscope:map nil
  "The rscope keymap.")
(if rscope:map
    nil
  (define-prefix-command 'rscope:map)
  (global-set-key (kbd "\C-cs") 'rscope:map)
  ;; The following line corresponds to be beginning of the "Cscope" menu.
  (define-key rscope:map "s" 'rscope-find-this-symbol)
  (define-key rscope:map "d" 'rscope-find-global-definition)
  (define-key rscope:map "g" 'rscope-find-global-definition)
  (define-key rscope:map "c" 'rscope-find-functions-calling-this-function)
  (define-key rscope:map "C" 'rscope-find-called-functions)
  (define-key rscope:map "t" 'rscope-find-this-text-string)
  (define-key rscope:map "i" 'rscope-find-files-including-file)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun rscope-init (dir)
  (interactive "DCscope Initial Directory: ")
  (let* ((buffer-name (format "*rscope-%s*" dir))
	 (rscope-buffer (get-buffer-create buffer-name))
	 process)
    (with-current-buffer rscope-buffer
      (if (get-process buffer-name) (kill-process (get-process buffer-name)))
      (setq default-directory dir)
      (setq process (start-process buffer-name buffer-name
				   "cscope" "-ld" "-f" "cscope.out"))
      (set-process-filter process 'rscope-filter)
      (set-process-query-on-exit-flag process nil)
      (accept-process-output process 3)
      (if (looking-at ".*cannot open.*cscope\.out.*")
	  (progn
	    (setq buf (get-buffer "*rscope*"))
	    (if buf
		(kill-buffer buf))
	    (message "rscope: no cscope.out file here"))
	(progn
	  (rscope-wait-for-output)
	  (message "rscope: load ok"))
	))))

(defun rscope-find-this-symbol (symbol)
  "Locate a symbol in source code."
  (interactive (rscope-interactive "Find this symbol: "))
  (setq query-command (concat "0" symbol "\n") )
  (setq rscope-action-message (format "Find this symbol: %s" symbol))
  (rscope-handle-query query-command))

(defun rscope-find-global-definition (symbol)
  "Find a symbol's global definition."
  (interactive (rscope-interactive "Find this global definition: "))
  (setq query-command (concat "1" symbol "\n") )
  (setq rscope-action-message (format "Finding global definition: %s" symbol))
  (rscope-handle-query query-command))

(defun rscope-find-called-functions (symbol)
  "Display functions called by a function."
  (interactive (rscope-interactive "Find functions called by this function: "))
  (setq query-command (concat "2" symbol "\n") )
  (setq rscope-action-message (format "Find functions called by this function: %s" symbol))
  (rscope-handle-query query-command))

(defun rscope-find-functions-calling-this-function (symbol)
  "Display functions calling a function."
  (interactive (rscope-interactive "Find functions calling this function: "))
  (setq query-command (concat "3" symbol "\n") )
  (setq rscope-action-message (format "Find functions calling this function: %s" symbol))
  (rscope-handle-query query-command))

(defun rscope-find-this-text-string (symbol)
  "Locate where a text string occurs."
  (interactive (rscope-interactive "Find this text string: "))
  (setq query-command (concat "4" symbol "\n") )
  (setq rscope-action-message (format "Find this text string: %s" symbol))
  (rscope-handle-query query-command))

(defun rscope-find-files-including-file (symbol)
  "Locate all files #including a file."
  (interactive (rscope-interactive "Find files #including this file: "))
  (setq query-command (concat "8" symbol "\n") )
  (setq rscope-action-message (format "Find files #including this file: %s" symbol))
  (rscope-handle-query query-command))

(defun rscope-all-symbol-assignments (symbol)
  "Find all the assignments of the symbol"
  (interactive (rscope-interactive "this don't work due to the bug of cscope, Find all assignments of symbol: "))
  (setq query-command (concat "10" symbol "\n") )
  (setq rscope-action-message (format "Find all assignments of symbol %s" symbol))
  (rscope-handle-query query-command))

(defun rscope-ring-bufferp (buffer)
  "Check if buffer is on the cscope searches ring."
  (member buffer
	  (mapcar 'marker-buffer (ring-elements rscope-marker-ring))))

(defun rscope-pop-mark()
  "Pop back to where cscope was last invoked."
  (interactive)
  (if (ring-empty-p rscope-marker-ring)
      (error "There are no marked buffers in the rscope-marker-ring yet"))
  (let* ((marker (ring-remove rscope-marker-ring 0))
	  (old-buffer (current-buffer))
	  (marker-buffer (marker-buffer marker))
	  marker-window
	  (marker-point (marker-position marker))
	  (old-buffer-killable))
    (setq old-buffer-killable
	  (and (with-current-buffer old-buffer
		 (and (boundp 'rscope-auto-open) rscope-auto-open))
	       (not (rscope-ring-bufferp old-buffer))))
			       
    (if marker-buffer
	(progn
	  (when old-buffer-killable (kill-buffer old-buffer))
	  (switch-to-buffer marker-buffer)
	  (goto-char marker-point)))
    ))

(defun get-strings-prefixed-by (prefix list)
  (delq nil
	(mapcar (lambda (x) (when (string-prefix-p prefix x) x)) list)))

(defun rscope-get-cscope-buffers ()
  (get-strings-prefixed-by "*rscope-"
			   (mapcar (function buffer-name) (buffer-list))))

(defun rscope-find-cscope-process (buffer)
  "Find the initialized (through rscope-init) cscope buffer for buffer.
The match is done by matching the buffer absolute path of the
file with the absolute path of each rscope initialized buffer,
and see if a match appears.

By default, if no match found and if exactly one cscope is launched,
use it."
  (let* ((filename (buffer-file-name buffer))
	(rscope-buffers (rscope-get-cscope-buffers))
	(exact-match))
    (when filename
      (setq exact-match
	    (car (delq nil (mapcar (lambda(buf)
				     (when (string-prefix-p
					    (expand-file-name (buffer-local-value 'default-directory (get-buffer buf)))
					    filename)
				       buf))
				   rscope-buffers)))))
    (if exact-match exact-match (car rscope-buffers))
    ))

(defun rscope-select-unique-result ()
  "Called when query returned only 1 result, and display window"
  (let (l file-name line-number)
    (with-current-buffer (get-buffer-create rscope-output-buffer-name)
      (goto-char (point-min))
      (setq l (rscope-get-relative-entry (current-buffer) +1))
      (setq file-name (nth 0 l))
      (setq line-number (nth 1 l))
      (if (and file-name line-number)
	  (rscope-select-entry-other-window)
	(error "No cscope unique entry found, that's abnormal")))))

(defun rscope-handle-query (query)
  "Launch the query in the rscope process."
  (let (nb-results
	(rscope-process (rscope-find-cscope-process (current-buffer))))
    (if rscope-process
	(progn
	  (setq nb-results (rscope-query (get-process rscope-process) query))
	  (when (>= nb-results 1)
	    (ring-insert rscope-marker-ring (point-marker)))
	  (when (= 1 nb-results) (rscope-select-unique-result)))
      (error "No rscope initialized found, did you call rscope-init ?")
    )))

(defun rscope-clear-overlay-arrow ()
  "Clean up the overlay arrow."
  (interactive)
  (let ()
    (if overlay-arrow-position
	(set-marker overlay-arrow-position nil))
    ))

(defun rscope-mark-buffer-opened (buffer)
  (with-current-buffer buffer
    (make-variable-frame-local 'rscope-auto-open)
    (setq rscope-auto-open t)
    (add-hook 'first-change-hook (lambda () (setq rscope-auto-open nil)))
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun rscope-get-nth-relative-entry (entry-number)
  "Returns the (file . line-number) of the nth relative entry in the result
buffer. This function should only be called inside the result buffer."
  (let* (line-number file-name stall
	 (direction (if (> entry-number 0) 1 -1)))
    (while (not (= 0 entry-number))
      (setq entry-number (- entry-number direction))
      ; Look for next valid entry
      (setq stall 0)
      (while (= 0 stall)
	(setq stall (forward-line direction))
	(setq line-number (get-text-property (point) 'rscope-line-number))
	(when line-number
	  (setq stall 1))))

    (setq line-number (get-text-property (point) 'rscope-line-number))
    (setq file-name (get-text-property (point) 'rscope-file))
    (list file-name line-number)))

(defun rscope-get-relative-entry (result-buffer &optional entry-number)
  "Returns the (file . line-number) of the currently selected result.
If entry-number is provided, return the nth next/previous entry in the results."
  (with-current-buffer result-buffer
    (unless entry-number
      (setq entry-number 0))
    (rscope-get-nth-relative-entry entry-number)))

(defun rscope-clear-previewed-buffers (result-buffer &optional spared-buffer)
  "Kills all previewed buffer, sparing only the spared-buffer and the
preview buffers which were already opened before preview.
The spared buffers are cleaned of his arrow."
  (let (all-buffers spared-buffers die-buffers)
    (setq all-buffers
	  (with-current-buffer result-buffer preview-buffers))
    (setq spared-buffers
	  (with-current-buffer result-buffer preview-already-opened-buffers))
    (push spared-buffer spared-buffers)
    (delete-dups all-buffers)
    (delete-dups spared-buffers)

    (setq die-buffers (set-difference all-buffers spared-buffers))
    (dolist (buffer die-buffers)
      (when (buffer-live-p buffer) (kill-buffer buffer)))
    (dolist (buffer spared-buffers)
      (when (buffer-live-p buffer)
	(with-current-buffer buffer
	  (set-marker overlay-arrow-position nil))))
    (with-current-buffer result-buffer
      (setq preview-buffers '())
      (setq preview-already-opened-buffers '()))
))

(defun rscope-get-buffer-file-line (file-name line-number &optional arrowp)
  "Display a (file, line) in either the current window or the other window.
Optionally draw an arrow at the line number."
  (let (already-opened)
    (when (and file-name line-number)
      (unless (file-readable-p file-name)
	(error "%s is not readable or exists" file-name))
      (when (get-file-buffer file-name)
	(setq already-opened t))
      (setq buffer (find-file-noselect file-name))
      (with-current-buffer buffer
	(when (not already-opened)
	  (rscope-mark-buffer-opened buffer))
	(goto-char (point-min))
	(forward-line (1- line-number))
	(when (and rscope-allow-arrow-overlays arrowp)
	  (set-marker overlay-arrow-position (point))))
      )
      buffer
))

(defun rscope-display-file-line (file-name line-number &optional otherp selectp arrowp)
  "Display a (file, line) in either the current window or the other window.
If selectp, select the buffer. If arrowp, draw an arrow on the line number
on this buffer for the selected entry.
Returns the buffer containing the file."
  (let ((buffer (rscope-get-buffer-file-line file-name line-number arrowp)))
    (if selectp
	(progn
	  (if otherp (pop-to-buffer buffer) (switch-to-buffer buffer))
	  (goto-line line-number))
      (display-buffer buffer))
    buffer
    ))

(defun rscope-display-entry-current-window ()
  "Display the entry at point in current window.
Open a new buffer if necessary."
  (interactive)
  (apply 'rscope-display-file-line
	 (append (rscope-get-relative-entry (current-buffer) 0) nil)))

(defun rscope-display-entry-other-window ()
  (interactive)
  "Display the entry at point in other window, without loosing selection."
  (apply 'rscope-display-file-line
	 (append (rscope-get-relative-entry (current-buffer) 0) '(t nil)))
)

(defun rscope-select-entry-current-window ()
  "Select the entry in the cscope result buffer at current point,
display it in the current window replacing the result buffer."
  (interactive)
  (let ((result-buffer (current-buffer))
	(buffer
	 (apply 'rscope-display-file-line
		(append (rscope-get-relative-entry (current-buffer) 0) '(nil t)))))
    (rscope-clear-previewed-buffers result-buffer buffer)
))

(defun rscope-select-entry-other-window ()
  "Select the entry in the cscope result buffer at current point,
display it in the other window, and bury the result buffer."
  (interactive)
  (let* ((result-buffer (current-buffer))
	(buffer (apply 'rscope-display-file-line
		       (append (rscope-get-relative-entry result-buffer 0) '(t t)))))
    (quit-window nil (get-buffer-window result-buffer))
    (rscope-clear-previewed-buffers result-buffer buffer)
))

(defun rscope-preview-entry-other-window ()
  "Preview the entry in another window, without loosing selection, and
with an optionnal arrow to show what was found."
  (interactive)
  (let (buffer file-line file already-opened)
    (setq file-line (rscope-get-relative-entry (current-buffer) 0))
    (setq file (nth 0 file-line))
    (when (get-file-buffer file)
      (setq already-opened t))
    (apply 'rscope-display-file-line (append file-line '(t nil t)))
    (push buffer preview-buffers)
    (when already-opened
      (push buffer preview-already-opened-buffers))
))

(defun rscope-next-symbol ()
  "Move to the next symbol in the *rscope* buffer."
  (interactive)
  (rscope-get-relative-entry (current-buffer) +1))

(defun rscope-prev-symbol ()
  "Move to the previous symbol in the *rscope* buffer."
  (interactive)
  (rscope-get-relative-entry (current-buffer) -1))

(defun rscope-close-results ()
  "Close the cscope result buffer, and all previews."
  (interactive)
  (let ((result-buffer (current-buffer)))
    (rscope-clear-previewed-buffers result-buffer)
    (quit-window nil (get-buffer-window result-buffer))))

(defun rscope-list-entry-mode ()
  (use-local-map rscope-list-entry-keymap)
  (setq buffer-read-only t
	mode-name "rscope"
	major-mode 'rscope-list-entry-mode
	overlay-arrow-string rscope-overlay-arrow-string
	)
  (or overlay-arrow-position
      (setq overlay-arrow-position (make-marker)))
  (run-hooks 'rscope-list-entry-hook)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun rscope-filter (process string)
  ;; Write the output into the Tramp Process
  (with-current-buffer (process-buffer process)
    (save-excursion
      (goto-char (point-max))
      (insert string))))

(defun rscope-query (proc command)
  (let ((nb-lines 0) procbuf outbuf)
    (setq procbuf (process-buffer proc))
    (with-current-buffer procbuf
      (goto-char (point-max))
      (insert command)
      (process-send-string proc command)
      (setq nb-lines (rscope-wait-for-output))
      (rscope-process-output procbuf)
      )

    (setq outbuf (get-buffer-create rscope-output-buffer-name))
    (with-current-buffer outbuf
      (progn
	(make-local-variable 'preview-buffers)
	(make-local-variable 'preview-already-opened-buffers)
	(setq preview-buffers '())
	(setq preview-already-opened-buffers '())
	(pop-to-buffer outbuf)
	(shrink-window 5)
	(insert rscope-separator-line "\n")
	(insert "Search complete.")
	(goto-char (point-min))
	(rscope-get-relative-entry (current-buffer) +1)
	(rscope-list-entry-mode)
	)
      )
    nb-lines))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun rscope-interactive (prompt)
  (list
   (let (sym)
     (setq sym (current-word))
     (read-string
      (if sym
	  (format "%s (default %s): "
		  (substring prompt 0 (string-match "[ :]+\\'" prompt))
		  sym)
	prompt)
      nil nil sym)
     ))
  )

(defun rscope-make-entry-line (func-name line-number line)
  ;; The format of entry line:
  ;; func-name[line-number]______line
  ;; <- cscope-name-line-width ->
  ;; `format' of Emacs doesn't have "*s" spec.
  (let* ((fmt (format "%%%ds %%s" rscope-name-line-width))
	 (str (format fmt (format "%s[%s]" func-name line-number) line))
	 beg end)
    (if rscope-use-face
	(progn
	  (setq end (length func-name))
	  (put-text-property 0 end 'face 'rscope-function-face str)
	  (setq beg (1+ end)
		end (+ beg (length line-number)))
	  (put-text-property beg end 'face 'rscope-line-number-face str)
	  (setq end (length str)
		beg (- end (length line)))
	  (put-text-property beg end 'face 'rscope-line-face str)
	  ))
    str))

(defun rscope-insert-text-with-properites (text filename &optional line-number)
  (let (plist beg end
	      (outbuf (get-buffer-create rscope-output-buffer-name)))
    (progn
      (set-buffer outbuf)
      (if (not rscope-first-match)
	  (progn
	    (insert rscope-action-message "\n\n")
	    (insert rscope-separator-line)
	    ))

      ;;insert file name here
      ;; If the current file is not the same as the previous
      ;; one ...
      (if (not (and rscope-last-file
		    (string= filename rscope-last-file)))
	  (progn
	    ;; The current file is different.

	    ;; Insert a separating blank line if
	    ;; necessary.
	    (if rscope-last-file (insert "\n"))
	    ;; Insert the file name
	    (setq str (concat "*** " filename ":"))
	    (if rscope-use-face
		(put-text-property 0 (length str)
				   'face 'rscope-file-face
				   str))
	    (insert str)
	    (insert "\n")
	    (setq rscope-last-file filename)
	    ))

      (if (not rscope-first-match)
	  (progn
	    (setq rscope-first-match-point (point))
	    (setq rscope-first-match '(t))
	    ))
      (setq beg (point))
      (insert text)

      (setq end (point)
	    plist (plist-put plist 'rscope-file filename))
      (if line-number
	  (progn
	    (if (stringp line-number)
		(setq line-number (string-to-number line-number)))
	    (setq plist (plist-put plist 'rscope-line-number line-number))
	    ))
      (add-text-properties beg end plist)
      (insert "\n")
      )))

(defun rscope-process_one_chunk (buf text-start text-end)
  (with-current-buffer buf
    (setq stuff (buffer-substring-no-properties text-start text-end))
    (while (and stuff
		(string-match "\\([^\n]+\n\\)\\(\\(.\\|\n\\)*\\)" stuff))
      (setq line (substring stuff
			    (match-beginning 1) (match-end 1)))

      (setq stuff (substring stuff
			     (match-beginning 2)
			     (match-end 2)))
      (if (= (length stuff) 0)
	  (setq stuff nil))

      (if (string-match
	   "\\([^[:blank:]]*\\)[[:blank:]]+\\([^[:blank:]]*\\)[[:blank:]]+\\([[:digit:]]*\\)[[:blank:]]+\\(.*\\)"
	   line)
	  (progn
	    (let (str)
	      (setq file (substring line (match-beginning 1)
				    (match-end 1))
		    function-name (substring line (match-beginning 2)
					     (match-end 2))
		    line-number (substring line
					   (match-beginning 3)
					   (match-end 3))
		    line (substring line (match-beginning 4)
				    (match-end 4))
		    )

	      (rscope-insert-text-with-properites
	       (rscope-make-entry-line function-name
				       line-number
				       line)
	       (expand-file-name file)
	       line-number)
	      ))))
    )
  )

(defun rscope-process-output (buf)
  (setq rscope-first-match nil
	rscope-last-file nil)
  (if (get-buffer rscope-output-buffer-name)
      (kill-buffer rscope-output-buffer-name)
    )
  (let (text-start text-end text-max)
    (with-current-buffer buf
      (setq text-start (point))
      (setq text-max (point-max))
      (if (>= (- text-max text-start) 5000)
	  (setq text-end (+ text-start 5000))
	(setq text-end text-max))
      )
    (while (and (> (- text-end text-start) 0) (<= text-end text-max))

      (rscope-process_one_chunk buf text-start text-end)

      (setq text-start (+ text-end 1))
      (if (>= (- text-max text-start) 5000)
	  (setq text-end (+ text-start 5000))
	(setq text-end text-max))))
  )

(defun rscope-wait-for-output (&optional timeout)
  (let ((proc (get-buffer-process (current-buffer)))
	(found nil)
	(start-time (current-time))
	(start-point (point))
	(nb-lines 0)
	)

    (save-excursion
      (while (not found)
	(accept-process-output proc 1)
	(goto-char (point-max)) ;move the last line
	(beginning-of-line) ;move the beginning of last line
	(setq found (looking-at "^>>"))) ;looking for cscope prompt "^>>"
      )
    ;; Find the number of results returned by the search
    (save-excursion
      (goto-char start-point)
      (when (re-search-forward "^cscope: \\([0-9]+\\) lines$" nil t)
	(setq nb-lines (string-to-number (match-string 1))))
      )
    nb-lines
    )
  )

(defvar rscope-minor-mode-hooks nil
  "List of hooks to call when entering cscope-minor-mode.")

;;; Minor mode
(defvar rscope-minor-mode nil
  "")
(make-variable-buffer-local 'rscope-minor-mode)
(put 'rscope-minor-mode 'permanent-local t)

(defun rscope-minor-mode (&optional arg)
  ""
  (progn
    (setq rscope-minor-mode (if (null arg) t (car arg)))
    (when rscope-minor-mode
      (run-hooks 'rscope-minor-mode-hooks)
      )
    rscope-minor-mode
    ))


(defun rscope:hook ()
  ""
  (progn
    (rscope-minor-mode)
    ))


(or (assq 'rscope-minor-mode minor-mode-map-alist)
    (setq minor-mode-map-alist (cons (cons 'rscope-minor-mode rscope:map)
				     minor-mode-map-alist)))

(add-hook 'c-mode-hook (function rscope:hook))
(add-hook 'c++-mode-hook (function rscope:hook))
(add-hook 'dired-mode-hook (function rscope:hook))

(provide 'rscope)
;;; rscope.el ends here
