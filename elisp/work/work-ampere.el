;; Ampere(Renault) related stuff

(defgroup work-ampere nil
	"Ampere(Renault) related stuff"
	:group 'work)

(defcustom work-ampere-jira-server-url "https://jira.dt.renault.com"
  "Jira URL for Renault"
  :type 'string
  :group 'work-ampere)

(defun work-ampere-jira-url (jira_id)
  "Generate the Ampere JIRA URL from the jira ID"
  (concat work-ampere-jira-server-url "/browse/" jira_id))

(use-package org
  :config
  (org-link-set-parameters "jira"
			    :follow (lambda (jira_id)
				      (browse-url (work-ampere-jira-url jira_id)))
			    :export (lambda (jira_id desc backend)
				      (cond
				       ((eq backend 'html) (format "<a href=\"%s\">%s</a>" (work-ampere-jira-url jira_id) (or desc path)))
				       ((eq backend 'latex) (format "\\href{%s}{%s}" (work-ampere-jira-url jira_id) path (or desc path)))
				       ((eq backend 'md) (format "[%s](%s)" (or desc path) (work-ampere-jira-url jira_id)))
				       )))
  )

(provide 'work-ampere)
