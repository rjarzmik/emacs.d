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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; High-level user usable functions (init + queries)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Result buffer navigation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Result buffer helpers: internal navigation, buffer spawning
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun rscope-get-nth-relative-entry (entry-number)
  "Returns the (file . line-number) of the nth relative entry in the result
buffer. This function should only be called inside the result buffer."
  (let (line-number file-name stall
	(direction (if (> entry-number 0) 1 -1)))
					; Be at beginning of line
    (forward-line 0)
    (while (not (= 0 entry-number))
      (setq entry-number (- entry-number direction))
					; Look for next valid entry
      (setq stall 0)
      (while (= 0 stall)
	(setq stall (forward-line direction))
					;(when (looking-at "^\\*\\* ")
					;  (setq stall 1))))
	(setq line-number (get-text-property (point) 'rscope-line-number))
	(when line-number
	  (setq stall 1))))

    (setq line-number (get-text-property (point) 'rscope-line-number))
    (setq file-name (get-text-property (point) 'rscope-file-name))
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
      (setq file-name
	    (expand-file-name file-name default-directory))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Result buffer helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun rscope-ring-bufferp (buffer)
  "Check if buffer is on the cscope searches ring."
  (member buffer
	  (mapcar 'marker-buffer (ring-elements rscope-marker-ring))))

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
  (let (nb-results result-buf
		   (rscope-process (rscope-find-cscope-process (current-buffer))))
    (setq result-buf
	  (rscope-create-result-buffer rscope-action-message rscope-process))
    (if rscope-process
	(progn
	  (setq nb-results
		(rscope-cscope-exec-query rscope-process query))
	  (rscope-cscope-parse-output rscope-process
				      result-buf 'rscope-results-organize-filename)
	  (when (>= nb-results 1)
	    (ring-insert rscope-marker-ring (point-marker)))
	  (rscope-finish-result-buffer result-buf)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Cscope process running and result formatting
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun rscope-filter (process string)
  ;; Write the output into the Tramp Process
  (with-current-buffer (process-buffer process)
    (save-excursion
      (goto-char (point-max))
      (insert string))))

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
	(forward-line 0) ;move to the beggining of last line
	(setq found (looking-at "^>>"))) ;looking for cscope prompt "^>>"
      )
    ;; Find the number of results returned by the search
    (goto-char start-point)
    (when (re-search-forward "^cscope: \\([0-9]+\\) lines$" nil t)
      (setq nb-lines (string-to-number (match-string 1)))
      (forward-line +1))
    nb-lines
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rscope minor mode hook: provides rscope:keymap for key shortcuts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
      (run-hooks 'rscope-minor-mode-hooks))
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

;;; To be melded into process handling
(defun rscope-cscope-parse-output (procbuf resultbuf organizer)
  "Process a cscope raw output in procbuf, between point [(point)..(point-max)].
Parse each line, and once file, line number and line content are parsed,
call organizer to handle them within resultbuf."
  (let (line file line-number function-name content
	     (stall 0)
	     (cscope-regexp
	      "\\([^[:blank:]]*\\)[[:blank:]]+\\([^[:blank:]]*\\)[[:blank:]]+\\([[:digit:]]*\\)[[:blank:]]+\\(.*\\)"
	      ))
    (with-current-buffer procbuf
      (while (and (< (point) (point-max)) (= 0 stall))
	(setq line (buffer-substring-no-properties (line-beginning-position)
						   (line-end-position)))
	(setq stall (forward-line 1))

					; Match a cscope output line
	(when (string-match cscope-regexp line)
	  (setq file (match-string 1 line))
	  (setq function-name (match-string 2 line))
	  (setq line-number (string-to-number (match-string 3 line)))
	  (setq content (match-string 4 line))
	  (apply organizer (list resultbuf file line-number function-name content))
	  )))))

(defun rscope-results-insert-filename (file-name level)
  (insert (propertize "*"
		      'rscope-file-name file-name))
  (insert-char ?* (- level 1))
  (insert " ")
  (insert (propertize file-name
		      'face 'rscope-file-face
		      'rscope 'rscope-file-name))
  (insert "\n")
  )

(defun rscope-results-insert-function (function-name level line-number content file-name)
  (let (str)
    (insert (propertize "*"
			'rscope-function-name function-name
			'rscope-line-number line-number
			'rscope-file-name file-name))
    (insert-char ?* (- level 1))
    (insert " ")
    (setq str (concat
	       (propertize function-name 'face 'rscope-function-face)
	       "() ["
	       (propertize (number-to-string line-number) 'face 'rscope-line-number-face)
	       "]"
	       (propertize content 'face 'rscope-line-face)
	       "\n"))
    (insert str)))

(defun rscope-results-organize-filename (buf file line-number function-name content)
  "Insert in buffer buf the entry, where all functions are grouped by file."
  (let (found)
    (with-current-buffer (get-buffer-create buf)
      ;; Find the file in buf if already present
      (goto-char (point-min))
      (setq found (re-search-forward (format "^\* %s" file) nil t))
      (forward-line +0)
      
      ;;; If found the file, move to the next line, beggining of line
      ;;; Else insert the new filename
      (unless found
	(goto-char (point-max))
	(rscope-results-insert-filename file 1)
	(forward-line -1))
      (forward-line +1)

      ;;; Insert the found result
      (rscope-results-insert-function function-name 2 line-number content
				      file)
      )))

(defun rscope-results-organize-funcs (buf file line-number function-name content)
  "Insert in buffer buf the entry, where all functions are not grouped."
  (let (found)
    (with-current-buffer buf
      (rscope-results-insert-function function-name rscope-level line-number content
				      file))))

(defun rscope-create-result-buffer (header procbuf)
  (when (get-buffer rscope-output-buffer-name)
    (kill-buffer rscope-output-buffer-name))
  (let ((result-buf (get-buffer-create rscope-output-buffer-name)))
    (with-current-buffer result-buf
      (make-local-variable 'preview-buffers)
      (make-local-variable 'preview-already-opened-buffers)
      (setq preview-buffers '()
	    preview-already-opened-buffers '())
      (setq default-directory
	    (buffer-local-value 'default-directory (get-buffer  procbuf)))
      (when header
	(insert header "\n"))
      (insert rscope-separator-line "\n"))
    result-buf))

(defun rscope-finish-result-buffer (result-buf)
  (with-current-buffer result-buf
    (goto-char (point-max))
    (insert rscope-separator-line "\n")
    (insert "Search complete.")
    (pop-to-buffer result-buf)
    (goto-char (point-min))
    (rscope-get-relative-entry (current-buffer) +1)
    (rscope-list-entry-mode)))

(defun rscope-cscope-exec-query (procbuf command)
  (let ((nb-lines 0)
	(proc (get-buffer-process procbuf)))
    (with-current-buffer procbuf
      (goto-char (point-max))
      (insert "\n")
      (process-send-string proc command)
      (setq nb-lines (rscope-wait-for-output))
      nb-lines)))

(defun rscope-call-hierarchy (procbuf function-name levels)
  (let (result-buf regexp found nb-lines)
    (setq result-buf (rscope-create-result-buffer rscope-action-message procbuf))
    (with-current-buffer result-buf
      (rscope-results-insert-function function-name 1 0 "" "File")
      (make-local-variable 'rscope-level)

      (dotimes (level levels)
	(setq regexp (format "^[*]\\{%d\\}" (+ 1 level))
	      rscope-level (+ 2 level))
	(goto-char (point-min))
	(setq found (re-search-forward regexp nil t))
	(while found
	  (forward-line 0)
	  (setq function-name (get-text-property (point) 'rscope-function-name))
	  (forward-line +1)
	  (setq nb-lines
		(rscope-cscope-exec-query procbuf (concat "3" function-name "\n")))
	  (rscope-cscope-parse-output procbuf
				      result-buf 'rscope-results-organize-funcs)
	  (setq found (re-search-forward regexp nil t)))
	))
    (rscope-finish-result-buffer result-buf)))
