;;; Robert Jarzmik
;;; 2011-07-10

;; Copyright (C) 2011 Robert Jarzmik

;; Author: Robert Jarzmik <robert.jarzmik@free.fr>
;; Keywords: emacs Atos Icare

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
;; Definitions for Atos Icare project

(when (featurep 'my-work)
  (message "Loading Atos Icare project settings")

  (c-add-style
   "atos-icare"
   '(
     (c-basic-offset . 4)
     (c-offsets-alist
      (substatement-open . 0)
      )
     )
   )

  (defun atos-icare-c++-mode-hook ()
    (c-set-style "atos-icare")
    (setq show-trailing-whitespace t)
    (setq indent-tabs-mode nil)
    (require 'my-cedet)
    (require 'semantic-ia)
    (dolist (dir (append (file-expand-wildcards "/mnt/LOCAL/rjarzmik/Projets/Icare-NG/icareng/*/trunk/code/include")
			 (file-expand-wildcards "/mnt/LOCAL/rjarzmik/Projets/Icare-NG/icareng/*/trunk/code/include/*")))
      (semantic-add-system-include dir))
    (define-key c++-mode-map "\M-/" 'semantic-ia-complete-symbol-menu)
    )

  (defun atos-icare-c-mode ()
    "C++ mode with adjusted defaults for use with Atos Icare project."
    (interactive)
    (c++-mode)
    (add-hook 'c++-mode-hook 'atos-icare-c++-mode-hook)
    )

  (setq auto-mode-alist (cons '(".*/icareng.*\\.[ch]p*$" . atos-icare-c-mode)
			      auto-mode-alist))

  (when (featurep 'activity)
    (add-to-list 'available-activities
		 (make-activity :name "Icare")))

  (provide 'work-atos-icare)
  )
