;;; Robert Jarzmik
;;; 2011-07-04

;; Copyright (C) 2010-2011 Jérémy Compostella

;; Author: Jérémy Compostella <jeremy.compostella@gmail.com>
;; Keywords: emacs GNUS mail configuration

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
;; My emacs w3m configuration

(when (featurep 'w3m)
  (setq w3m-use-cookies t
	w3m-default-coding-system 'utf-8
	w3m-default-directory "~/.emacs.d/data/w3m"
	browse-url-browser-function 'w3m
	w3m-profile-directory "~/.emacs.d/data/w3m"
	w3m-language "english"
	w3m-use-japanese-menu nil)
  (defun w3m-search-next-page ()
    (interactive)
    (save-excursion
      (goto-char (point-max))
      (search-backward "Suivan" nil t)
      (w3m-view-this-url)))

  (defun w3m-search-prev-page ()
    (interactive)
    (save-excursion
      (goto-char (point-max))
      (search-backward "Précé" nil t)
      (w3m-view-this-url)))

  (defconst w3m-title-regexp "^ ?\\([0-9]+\.\\)*[0-9]+\.?")

  (defun w3m-search-next-title ()
    (interactive)
    (search-forward-regexp w3m-title-regexp nil t)
    (recenter))
  (defun w3m-search-prev-title ()
    (interactive)
    (beginning-of-line)
    (search-backward-regexp w3m-title-regexp nil t)
    (w3m-search-next-title))

  (define-key w3m-mode-map (kbd "M-n") 'w3m-search-next-page)
  (define-key w3m-mode-map (kbd "M-p") 'w3m-search-prev-page)
  (define-key w3m-mode-map (kbd "n") 'w3m-search-next-title)
  (define-key w3m-mode-map (kbd "p") 'w3m-search-prev-title)
  (define-key w3m-mode-map (kbd "C-j") 'w3m-view-this-url))

(when (featurep 'w3m-session)
  (setq w3m-session-file "~/.emacs.d/data/w3m/w3m-session"
	w3m-session-save-always t
	w3m-session-load-always t
	w3m-session-show-titles t
	w3m-session-duplicate-tabs 'ask))

(when (featurep 'activity)
  (add-to-list 'available-activities '("Web" (lambda ()
					       (delete-other-windows)
					       (w3m)))))

;(setq w3m-command-arguments
;      (nconc w3m-command-arguments
;	     '("-o" "http_proxy=http://proxy.mdp:3128/")))
(provide 'my-w3m)

;;; Local Variables:
;;; eval: (defun byte-compile-this-file () (write-region (point-min) (point-max) buffer-file-name nil 't) (byte-compile-file buffer-file-name) nil)
;;; write-file-hooks: (byte-compile-this-file)
;;; End:
