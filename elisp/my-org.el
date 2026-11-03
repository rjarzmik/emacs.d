;;; my-org.el --- Org configuration for software development -*- lexical-binding: t; -*-

;;; Commentary:
;; Org-mode configuration for managing programming work, meetings,
;; deadlines, ideas, and JIRA issues.
;;
;; Install org-jira with:
;;
;;   M-x package-install RET org-jira RET

;;; Code:

;;;; User settings
(defgroup my-org nil
  "Org-mode configuration for software development."
  :group 'org)

(defcustom my-org-directory
  (expand-file-name (format "%s/data/org" user-emacs-directory))
  "Directory containing Org files."
  :type 'directory)

(defcustom programmer-jira-url ""
  "JIRA base URL, for example https://company.atlassian.net."
  :type 'string)

(defcustom programmer-jira-user ""
  "JIRA username or email address."
  :type 'string)

(defcustom programmer-jira-directory
  (expand-file-name "jira/" my-org-directory)
  "Directory used by org-jira."
  :type 'directory)

(setq org-directory my-org-directory)

(defconst my-org-inbox-file
  (expand-file-name "inbox.org" my-org-directory))

(defconst my-org-projects-file
  (expand-file-name "projects.org" my-org-directory))

(defconst my-org-meetings-file
  (expand-file-name "meetings.org" my-org-directory))

(defconst my-org-notes-file
  (expand-file-name "notes.org" my-org-directory))

;;;; Initialize directories and files

(defun my-org-create-file-if-missing (file title)
  "Create FILE with TITLE if it does not already exist."
  (unless (file-exists-p file)
    (with-temp-file file
      (insert "#+title: " title "\n\n"))))

(use-package org
  :hook ((org-mode . (lambda () (setq indent-tabs-mode nil))))
  :init
  (make-directory my-org-directory t)
  (make-directory programmer-jira-directory t)

  (my-org-create-file-if-missing
   my-org-inbox-file "Inbox")

  (my-org-create-file-if-missing
   my-org-projects-file "Projects")

  (my-org-create-file-if-missing
   my-org-meetings-file "Meetings")

  (my-org-create-file-if-missing
   my-org-notes-file "Notes")

  ;; General Org behavior
  (setq org-log-done 'time
	org-log-into-drawer t
	org-enforce-todo-dependencies t
	org-use-fast-todo-selection 'expert
	org-return-follows-link t
	org-startup-indented t
	org-hide-emphasis-markers t
	org-pretty-entities t
	org-use-speed-commands t
	org-catch-invisible-edits 'smart
	org-deadline-warning-days 14)

  (setq org-todo-keywords
	'((sequence
           "TODO(t)"
           "NEXT(n)"
           "IN-PROGRESS(i)"
           "WAIT(w@/!)"
           "BLOCKED(b@/!)"
           "|"
           "DONE(d!)"
           "CANCELLED(c@)")))

  (setq org-todo-keyword-faces
	'(("NEXT"        . (:foreground "DeepSkyBlue" :weight bold))
          ("IN-PROGRESS" . (:foreground "orange" :weight bold))
          ("WAIT"        . (:foreground "goldenrod"))
          ("BLOCKED"     . (:foreground "red" :weight bold))
          ("DONE"        . (:foreground "forest green"))
          ("CANCELLED"   . (:foreground "gray"))))

  (setq org-tag-alist
	'((:startgroup)
          ("work"       . ?w)
          ("personal"   . ?p)
          (:endgroup)
          ("jira"       . ?j)
          ("bug"        . ?b)
          ("feature"    . ?f)
          ("meeting"    . ?m)
          ("code-review". ?r)
          ("idea"       . ?i)
          ("urgent"     . ?u)))
  
  (setopt org-catch-invisible-edits 'show-and-error)
  )

;;;; Agenda files
(use-package org-agenda
  :commands org-agenda
  :bind ("C-c o a" . org-agenda)
  :custom (
	   ;; Directories in `org-agenda-files' include the .org files directly
	   ;; contained in that directory.
	   (org-agenda-files
	    (list my-org-inbox-file
		  my-org-projects-file
		  my-org-meetings-file
		  my-org-notes-file
		  programmer-jira-directory)))
  :config
  (setq org-agenda-window-setup 'current-window
	org-agenda-restore-windows-after-quit t
	org-agenda-start-on-weekday 1
	org-agenda-span 'week
	org-agenda-show-future-repeats nil
	org-agenda-skip-scheduled-if-done t
	org-agenda-skip-deadline-if-done t
	org-agenda-skip-timestamp-if-done t
	org-agenda-todo-ignore-deadlines 'far
	org-agenda-todo-ignore-scheduled 'future
	org-agenda-tags-column -100)

  (setq org-agenda-prefix-format
	'((agenda . " %i %-12:c%?-12t% s")
          (todo   . " %i %-12:c ")
          (tags   . " %i %-12:c ")
          (search . " %i %-12:c ")))

  (setq org-agenda-sorting-strategy
	'((agenda time-up priority-down category-keep)
          (todo priority-down effort-up category-keep)
          (tags priority-down category-keep)
          (search category-keep)))

  ;;;; Custom agenda dashboard
  (defun my-org-agenda-skip-unless-meeting ()
    "Skip the current subtree unless it has the `meeting' tag."
    (unless (member "meeting" (org-get-tags))
      (org-end-of-subtree t)))

  (setq org-agenda-custom-commands
	'(("d" "Developer dashboard"
           ((agenda ""
                    ((org-agenda-overriding-header "Today's agenda")
                     (org-agenda-span 1)
                     (org-agenda-start-day "0d")))

            (todo "NEXT|IN-PROGRESS|BLOCKED"
                  ((org-agenda-overriding-header "Active tasks")
                   (org-agenda-sorting-strategy
                    '(priority-down effort-up category-keep))))

            (agenda ""
                    ((org-agenda-overriding-header
                      "Deadlines in the next 14 days")
                     (org-agenda-span 14)
                     (org-agenda-start-day "0d")
                     (org-agenda-entry-types '(:deadline))))

            (agenda ""
                    ((org-agenda-overriding-header
                      "Meetings in the next 14 days")
                     (org-agenda-span 14)
                     (org-agenda-start-day "0d")
                     (org-agenda-entry-types '(:timestamp))
                     (org-agenda-skip-function
                      #'my-org-agenda-skip-unless-meeting)))))

          ("n" "Next actions"
           todo "NEXT|IN-PROGRESS")

          ("b" "Blocked and waiting"
           todo "BLOCKED|WAIT")

          ("j" "JIRA tasks"
           tags-todo "+jira")

          ("m" "Meetings"
           tags "+meeting")))
  )

;;;; Capture templates
(use-package org-capture
  :bind ("C-c o c" . org-capture)
  :custom (
	   (org-capture-templates
		 `(("t" "Task" entry
		    (file ,my-org-inbox-file)
		    "* TODO %^{Task}\n:PROPERTIES:\n:CREATED: %U\n:END:\n%?"
		    :empty-lines 1)

		   ("i" "Idea" entry
		    (file ,my-org-notes-file)
		    "* %^{Idea} :idea:\n:PROPERTIES:\n:CREATED: %U\n:END:\n%?"
		    :empty-lines 1)

		   ("m" "Meeting" entry
		    (file ,my-org-meetings-file)
		    "* %^{Meeting title} :meeting:\n%^T\n\n** Attendees\n- %?\n\n** Agenda\n- \n\n** Notes\n\n** Action items\n"
		    :empty-lines 1)

		   ("p" "Project task" entry
		    (file ,my-org-projects-file)
		    "* TODO %^{Task}\n:PROPERTIES:\n:CREATED: %U\n:END:\n%?"
		    :empty-lines 1)

		   ("j" "JIRA task" entry
		    (file ,my-org-inbox-file)
		    "* TODO %^{Summary} :jira:\n:PROPERTIES:\n:JIRA_KEY: %^{JIRA key}\n:CREATED: %U\n:END:\n[[jira:%\\2][%\\2]]\n\n%?"
		    :empty-lines 1)))))

;;;; Refile configuration
(use-package org-refile
  :custom (
	   (org-refile-targets
	    `((,my-org-projects-file :maxlevel . 3)
              (,my-org-notes-file :maxlevel . 2)
              (,my-org-meetings-file :maxlevel . 2)
              (,my-org-inbox-file :maxlevel . 2)))
	   (org-refile-use-outline-path 'file
					org-outline-path-complete-in-steps nil
					org-refile-allow-creating-parent-nodes 'confirm)))

;;;; Visuals: Global
(use-package org-appear
  :after org
  :hook (org-mode . org-appear-mode)
  :custom (
	   (org-appear-autolinks t)
	   (org-appear-autoentities t)
	   (org-appear-autosubmarkers t))
  )
;;;; Visuals: better Looking Bullets
(use-package org-modern
  :ensure t
  :hook ((org-mode                 . org-modern-mode)
         (org-agenda-finalize-hook . org-modern-agenda))
  :custom ((org-modern-todo t)
           (org-modern-table nil)
           (org-modern-variable-pitch nil)
           (org-modern-block-fringe nil)
           (org-modern-star 'replace))
  :commands (org-modern-mode org-modern-agenda)
  :init (global-org-modern-mode))

;;;; JIRA links
(use-package org-jira
  :requires (auth-source url-parse)
  :bind-keymap ("C-c o j" . my-org-jira-map)
  :bind (:map my-org-jira-map
	 ("g" . org-jira-get-issues)
	 ("f" . org-jira-get-issues-from-filter)
	 ("c" . org-jira-create-issue)
	 ("u" . org-jira-update-issue)
	 ("p" . org-jira-progress-issue))
  :init
    (setq org-jira-working-dir programmer-jira-directory)
  :config
  (defun my-org-jira-browse (issue-key)
    "Open ISSUE-KEY in the configured JIRA instance."
    (interactive "sJIRA issue: ")
    (unless (and programmer-jira-url
		 (not (string-empty-p programmer-jira-url)))
      (user-error "`programmer-jira-url' is not configured"))
    (browse-url
     (format "%s/browse/%s"
             (string-remove-suffix "/" programmer-jira-url)
             issue-key)))

  (org-link-set-parameters
   "jira"
   :follow #'my-org-jira-browse
   :export
   (lambda (path description backend _info)
     (let ((url (format "%s/browse/%s"
			(string-remove-suffix "/" programmer-jira-url)
			path))
           (text (or description path)))
       (pcase backend
	 ('html (format "<a href=\"%s\">%s</a>" url text))
	 ('latex (format "\\href{%s}{%s}" url text))
	 (_ (format "%s (%s)" text url))))))

  ;;; org-jira authentication
  (defun my-org-jira-host ()
    "Return the hostname from `programmer-jira-url'."
    (when (and programmer-jira-url
               (not (string-empty-p programmer-jira-url)))
      (url-host (url-generic-parse-url programmer-jira-url))))

  (defun my-org-jira-secret ()
    "Retrieve the JIRA API token using `auth-source'."
    (let* ((host (my-org-jira-host))
           (entry
            (and host
		 (car
                  (auth-source-search
                   :host host
                   :user programmer-jira-user
                   :require '(:secret)
                   :max 1))))
           (secret (plist-get entry :secret)))
      (when secret
	(if (functionp secret)
            (funcall secret)
          secret))))

  (defun my-org-jira-auth-header ()
    "Create an HTTP Basic Authorization header for JIRA."
    (when-let ((secret (my-org-jira-secret)))
      (cons
       "Authorization"
       (concat
	"Basic "
	(base64-encode-string
	 (concat programmer-jira-user ":" secret)
	 t)))))

  ;; Configure jiralib when it becomes available.
  (with-eval-after-load 'jiralib
    (setq jiralib-url programmer-jira-url
          jiralib-user programmer-jira-user)

    (when-let ((header (my-org-jira-auth-header)))
      (setq jiralib-token header))))

;; Store the API token in `~/.authinfo.gpg`:
;; machine your-company.atlassian.net login your-email@example.com password YOUR_API_TOKEN

(provide 'my-org)
